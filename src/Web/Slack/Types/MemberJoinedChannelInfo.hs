{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.Types.MemberJoinedChannelInfo
  ( MemberJoinedChannelInfo(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), parseJSON, withObject)
import Data.Text (Text)
import Web.Slack.Types.Id (ChannelId, TeamId, UserId)

data MemberJoinedChannelInfo =
  MemberJoinedChannelInfo
    { _memberJoinedChannelInfoUser :: UserId
    , _memberJoinedChannelInfoChannel :: ChannelId
    , _memberJoinedChannelInfoChannelType :: Text
    , _memberJoinedChannelInfoTeam :: TeamId
    , _memberJoinedChannelInfoInviter :: Maybe UserId
    }
  deriving (Show)

instance FromJSON MemberJoinedChannelInfo where
  parseJSON =
    withObject
      "MemberJoinedChannelInfo"
      (\o ->
         MemberJoinedChannelInfo <$> o .: "user" <*> o .: "channel" <*>
         o .: "channel_type" <*>
         o .: "team" <*>
         o .: "inviter")
