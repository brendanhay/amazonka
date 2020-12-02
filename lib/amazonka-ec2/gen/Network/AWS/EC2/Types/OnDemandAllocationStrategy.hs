{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OnDemandAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OnDemandAllocationStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OnDemandAllocationStrategy
  = ODASLowestPrice
  | ODASPrioritized
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

instance FromText OnDemandAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "lowestprice" -> pure ODASLowestPrice
      "prioritized" -> pure ODASPrioritized
      e ->
        fromTextError $
          "Failure parsing OnDemandAllocationStrategy from value: '" <> e
            <> "'. Accepted values: lowestprice, prioritized"

instance ToText OnDemandAllocationStrategy where
  toText = \case
    ODASLowestPrice -> "lowestPrice"
    ODASPrioritized -> "prioritized"

instance Hashable OnDemandAllocationStrategy

instance NFData OnDemandAllocationStrategy

instance ToByteString OnDemandAllocationStrategy

instance ToQuery OnDemandAllocationStrategy

instance ToHeader OnDemandAllocationStrategy

instance FromXML OnDemandAllocationStrategy where
  parseXML = parseXMLText "OnDemandAllocationStrategy"
