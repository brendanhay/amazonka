{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetOnDemandAllocationStrategy
  = FODASLowestPrice
  | FODASPrioritized
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

instance FromText FleetOnDemandAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "lowest-price" -> pure FODASLowestPrice
      "prioritized" -> pure FODASPrioritized
      e ->
        fromTextError $
          "Failure parsing FleetOnDemandAllocationStrategy from value: '" <> e
            <> "'. Accepted values: lowest-price, prioritized"

instance ToText FleetOnDemandAllocationStrategy where
  toText = \case
    FODASLowestPrice -> "lowest-price"
    FODASPrioritized -> "prioritized"

instance Hashable FleetOnDemandAllocationStrategy

instance NFData FleetOnDemandAllocationStrategy

instance ToByteString FleetOnDemandAllocationStrategy

instance ToQuery FleetOnDemandAllocationStrategy

instance ToHeader FleetOnDemandAllocationStrategy

instance FromXML FleetOnDemandAllocationStrategy where
  parseXML = parseXMLText "FleetOnDemandAllocationStrategy"
