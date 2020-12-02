{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupPolicy where

import Network.AWS.Prelude

data BackupPolicy = Default
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

instance FromText BackupPolicy where
  parser =
    takeLowerText >>= \case
      "default" -> pure Default
      e ->
        fromTextError $
          "Failure parsing BackupPolicy from value: '" <> e
            <> "'. Accepted values: default"

instance ToText BackupPolicy where
  toText = \case
    Default -> "DEFAULT"

instance Hashable BackupPolicy

instance NFData BackupPolicy

instance ToByteString BackupPolicy

instance ToQuery BackupPolicy

instance ToHeader BackupPolicy

instance FromJSON BackupPolicy where
  parseJSON = parseJSONText "BackupPolicy"
