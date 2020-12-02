{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MigrationTypeValue where

import Network.AWS.Prelude

data MigrationTypeValue
  = Cdc
  | FullLoad
  | FullLoadAndCdc
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

instance FromText MigrationTypeValue where
  parser =
    takeLowerText >>= \case
      "cdc" -> pure Cdc
      "full-load" -> pure FullLoad
      "full-load-and-cdc" -> pure FullLoadAndCdc
      e ->
        fromTextError $
          "Failure parsing MigrationTypeValue from value: '" <> e
            <> "'. Accepted values: cdc, full-load, full-load-and-cdc"

instance ToText MigrationTypeValue where
  toText = \case
    Cdc -> "cdc"
    FullLoad -> "full-load"
    FullLoadAndCdc -> "full-load-and-cdc"

instance Hashable MigrationTypeValue

instance NFData MigrationTypeValue

instance ToByteString MigrationTypeValue

instance ToQuery MigrationTypeValue

instance ToHeader MigrationTypeValue

instance ToJSON MigrationTypeValue where
  toJSON = toJSONText

instance FromJSON MigrationTypeValue where
  parseJSON = parseJSONText "MigrationTypeValue"
