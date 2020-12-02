{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy where

import Network.AWS.Prelude

data OnDemandProvisioningAllocationStrategy = LowestPrice
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

instance FromText OnDemandProvisioningAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "lowest-price" -> pure LowestPrice
      e ->
        fromTextError $
          "Failure parsing OnDemandProvisioningAllocationStrategy from value: '" <> e
            <> "'. Accepted values: lowest-price"

instance ToText OnDemandProvisioningAllocationStrategy where
  toText = \case
    LowestPrice -> "lowest-price"

instance Hashable OnDemandProvisioningAllocationStrategy

instance NFData OnDemandProvisioningAllocationStrategy

instance ToByteString OnDemandProvisioningAllocationStrategy

instance ToQuery OnDemandProvisioningAllocationStrategy

instance ToHeader OnDemandProvisioningAllocationStrategy

instance ToJSON OnDemandProvisioningAllocationStrategy where
  toJSON = toJSONText

instance FromJSON OnDemandProvisioningAllocationStrategy where
  parseJSON = parseJSONText "OnDemandProvisioningAllocationStrategy"
