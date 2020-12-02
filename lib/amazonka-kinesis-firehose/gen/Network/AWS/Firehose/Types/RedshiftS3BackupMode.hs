{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftS3BackupMode where

import Network.AWS.Prelude

data RedshiftS3BackupMode
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

instance FromText RedshiftS3BackupMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing RedshiftS3BackupMode from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText RedshiftS3BackupMode where
  toText = \case
    Disabled -> "Disabled"
    Enabled -> "Enabled"

instance Hashable RedshiftS3BackupMode

instance NFData RedshiftS3BackupMode

instance ToByteString RedshiftS3BackupMode

instance ToQuery RedshiftS3BackupMode

instance ToHeader RedshiftS3BackupMode

instance ToJSON RedshiftS3BackupMode where
  toJSON = toJSONText

instance FromJSON RedshiftS3BackupMode where
  parseJSON = parseJSONText "RedshiftS3BackupMode"
