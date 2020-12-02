{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkS3BackupMode where

import Network.AWS.Prelude

data SplunkS3BackupMode
  = AllEvents
  | FailedEventsOnly
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

instance FromText SplunkS3BackupMode where
  parser =
    takeLowerText >>= \case
      "allevents" -> pure AllEvents
      "failedeventsonly" -> pure FailedEventsOnly
      e ->
        fromTextError $
          "Failure parsing SplunkS3BackupMode from value: '" <> e
            <> "'. Accepted values: allevents, failedeventsonly"

instance ToText SplunkS3BackupMode where
  toText = \case
    AllEvents -> "AllEvents"
    FailedEventsOnly -> "FailedEventsOnly"

instance Hashable SplunkS3BackupMode

instance NFData SplunkS3BackupMode

instance ToByteString SplunkS3BackupMode

instance ToQuery SplunkS3BackupMode

instance ToHeader SplunkS3BackupMode

instance ToJSON SplunkS3BackupMode where
  toJSON = toJSONText

instance FromJSON SplunkS3BackupMode where
  parseJSON = parseJSONText "SplunkS3BackupMode"
