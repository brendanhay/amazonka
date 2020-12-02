{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3BackupMode where

import Network.AWS.Prelude

data S3BackupMode
  = SBMDisabled
  | SBMEnabled
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

instance FromText S3BackupMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SBMDisabled
      "enabled" -> pure SBMEnabled
      e ->
        fromTextError $
          "Failure parsing S3BackupMode from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText S3BackupMode where
  toText = \case
    SBMDisabled -> "Disabled"
    SBMEnabled -> "Enabled"

instance Hashable S3BackupMode

instance NFData S3BackupMode

instance ToByteString S3BackupMode

instance ToQuery S3BackupMode

instance ToHeader S3BackupMode

instance ToJSON S3BackupMode where
  toJSON = toJSONText

instance FromJSON S3BackupMode where
  parseJSON = parseJSONText "S3BackupMode"
