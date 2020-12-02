{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotAllocationStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotAllocationStrategy
  = CapacityOptimized
  | Diversified
  | LowestPrice
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

instance FromText SpotAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "capacity-optimized" -> pure CapacityOptimized
      "diversified" -> pure Diversified
      "lowest-price" -> pure LowestPrice
      e ->
        fromTextError $
          "Failure parsing SpotAllocationStrategy from value: '" <> e
            <> "'. Accepted values: capacity-optimized, diversified, lowest-price"

instance ToText SpotAllocationStrategy where
  toText = \case
    CapacityOptimized -> "capacity-optimized"
    Diversified -> "diversified"
    LowestPrice -> "lowest-price"

instance Hashable SpotAllocationStrategy

instance NFData SpotAllocationStrategy

instance ToByteString SpotAllocationStrategy

instance ToQuery SpotAllocationStrategy

instance ToHeader SpotAllocationStrategy

instance FromXML SpotAllocationStrategy where
  parseXML = parseXMLText "SpotAllocationStrategy"
