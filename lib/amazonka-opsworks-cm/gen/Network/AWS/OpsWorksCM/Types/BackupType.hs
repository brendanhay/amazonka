{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.BackupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.BackupType where

import Network.AWS.Prelude

data BackupType
  = Automated
  | Manual
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

instance FromText BackupType where
  parser =
    takeLowerText >>= \case
      "automated" -> pure Automated
      "manual" -> pure Manual
      e ->
        fromTextError $
          "Failure parsing BackupType from value: '" <> e
            <> "'. Accepted values: automated, manual"

instance ToText BackupType where
  toText = \case
    Automated -> "AUTOMATED"
    Manual -> "MANUAL"

instance Hashable BackupType

instance NFData BackupType

instance ToByteString BackupType

instance ToQuery BackupType

instance ToHeader BackupType

instance FromJSON BackupType where
  parseJSON = parseJSONText "BackupType"
