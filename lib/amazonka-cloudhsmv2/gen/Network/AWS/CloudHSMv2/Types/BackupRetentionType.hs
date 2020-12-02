{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupRetentionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupRetentionType where

import Network.AWS.Prelude

data BackupRetentionType = Days
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

instance FromText BackupRetentionType where
  parser =
    takeLowerText >>= \case
      "days" -> pure Days
      e ->
        fromTextError $
          "Failure parsing BackupRetentionType from value: '" <> e
            <> "'. Accepted values: days"

instance ToText BackupRetentionType where
  toText = \case
    Days -> "DAYS"

instance Hashable BackupRetentionType

instance NFData BackupRetentionType

instance ToByteString BackupRetentionType

instance ToQuery BackupRetentionType

instance ToHeader BackupRetentionType

instance ToJSON BackupRetentionType where
  toJSON = toJSONText

instance FromJSON BackupRetentionType where
  parseJSON = parseJSONText "BackupRetentionType"
