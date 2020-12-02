{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderStatus where

import Network.AWS.Prelude

data CapacityProviderStatus
  = CPSActive
  | CPSInactive
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

instance FromText CapacityProviderStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure CPSActive
      "inactive" -> pure CPSInactive
      e ->
        fromTextError $
          "Failure parsing CapacityProviderStatus from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText CapacityProviderStatus where
  toText = \case
    CPSActive -> "ACTIVE"
    CPSInactive -> "INACTIVE"

instance Hashable CapacityProviderStatus

instance NFData CapacityProviderStatus

instance ToByteString CapacityProviderStatus

instance ToQuery CapacityProviderStatus

instance ToHeader CapacityProviderStatus

instance FromJSON CapacityProviderStatus where
  parseJSON = parseJSONText "CapacityProviderStatus"
