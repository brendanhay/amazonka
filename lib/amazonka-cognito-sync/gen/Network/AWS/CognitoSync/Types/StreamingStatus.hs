{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.StreamingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.StreamingStatus where

import Network.AWS.Prelude

data StreamingStatus
  = Disabled
  | Enabled
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

instance FromText StreamingStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing StreamingStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText StreamingStatus where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable StreamingStatus

instance NFData StreamingStatus

instance ToByteString StreamingStatus

instance ToQuery StreamingStatus

instance ToHeader StreamingStatus

instance ToJSON StreamingStatus where
  toJSON = toJSONText

instance FromJSON StreamingStatus where
  parseJSON = parseJSONText "StreamingStatus"
