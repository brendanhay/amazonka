{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy where

import Network.AWS.Prelude

data SpotProvisioningAllocationStrategy = CapacityOptimized
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

instance FromText SpotProvisioningAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "capacity-optimized" -> pure CapacityOptimized
      e ->
        fromTextError $
          "Failure parsing SpotProvisioningAllocationStrategy from value: '" <> e
            <> "'. Accepted values: capacity-optimized"

instance ToText SpotProvisioningAllocationStrategy where
  toText = \case
    CapacityOptimized -> "capacity-optimized"

instance Hashable SpotProvisioningAllocationStrategy

instance NFData SpotProvisioningAllocationStrategy

instance ToByteString SpotProvisioningAllocationStrategy

instance ToQuery SpotProvisioningAllocationStrategy

instance ToHeader SpotProvisioningAllocationStrategy

instance ToJSON SpotProvisioningAllocationStrategy where
  toJSON = toJSONText

instance FromJSON SpotProvisioningAllocationStrategy where
  parseJSON = parseJSONText "SpotProvisioningAllocationStrategy"
