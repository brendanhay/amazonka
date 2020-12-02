{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReloadOptionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReloadOptionValue where

import Network.AWS.Prelude

data ReloadOptionValue
  = DataReload
  | ValidateOnly
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

instance FromText ReloadOptionValue where
  parser =
    takeLowerText >>= \case
      "data-reload" -> pure DataReload
      "validate-only" -> pure ValidateOnly
      e ->
        fromTextError $
          "Failure parsing ReloadOptionValue from value: '" <> e
            <> "'. Accepted values: data-reload, validate-only"

instance ToText ReloadOptionValue where
  toText = \case
    DataReload -> "data-reload"
    ValidateOnly -> "validate-only"

instance Hashable ReloadOptionValue

instance NFData ReloadOptionValue

instance ToByteString ReloadOptionValue

instance ToQuery ReloadOptionValue

instance ToHeader ReloadOptionValue

instance ToJSON ReloadOptionValue where
  toJSON = toJSONText
