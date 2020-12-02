{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllocationStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AllocationStrategy
  = ASCapacityOptimized
  | ASDiversified
  | ASLowestPrice
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

instance FromText AllocationStrategy where
  parser =
    takeLowerText >>= \case
      "capacityoptimized" -> pure ASCapacityOptimized
      "diversified" -> pure ASDiversified
      "lowestprice" -> pure ASLowestPrice
      e ->
        fromTextError $
          "Failure parsing AllocationStrategy from value: '" <> e
            <> "'. Accepted values: capacityoptimized, diversified, lowestprice"

instance ToText AllocationStrategy where
  toText = \case
    ASCapacityOptimized -> "capacityOptimized"
    ASDiversified -> "diversified"
    ASLowestPrice -> "lowestPrice"

instance Hashable AllocationStrategy

instance NFData AllocationStrategy

instance ToByteString AllocationStrategy

instance ToQuery AllocationStrategy

instance ToHeader AllocationStrategy

instance FromXML AllocationStrategy where
  parseXML = parseXMLText "AllocationStrategy"
