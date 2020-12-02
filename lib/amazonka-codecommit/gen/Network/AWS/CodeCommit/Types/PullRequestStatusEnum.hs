{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestStatusEnum where

import Network.AWS.Prelude

data PullRequestStatusEnum
  = Closed
  | Open
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PullRequestStatusEnum where
  parser =
    takeLowerText >>= \case
      "closed" -> pure Closed
      "open" -> pure Open
      e ->
        fromTextError $
          "Failure parsing PullRequestStatusEnum from value: '" <> e
            <> "'. Accepted values: closed, open"

instance ToText PullRequestStatusEnum where
  toText = \case
    Closed -> "CLOSED"
    Open -> "OPEN"

instance Hashable PullRequestStatusEnum

instance NFData PullRequestStatusEnum

instance ToByteString PullRequestStatusEnum

instance ToQuery PullRequestStatusEnum

instance ToHeader PullRequestStatusEnum

instance ToJSON PullRequestStatusEnum where
  toJSON = toJSONText

instance FromJSON PullRequestStatusEnum where
  parseJSON = parseJSONText "PullRequestStatusEnum"
