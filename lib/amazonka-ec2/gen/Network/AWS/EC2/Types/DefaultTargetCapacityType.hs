{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DefaultTargetCapacityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DefaultTargetCapacityType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DefaultTargetCapacityType
  = DTCTOnDemand
  | DTCTSpot
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

instance FromText DefaultTargetCapacityType where
  parser =
    takeLowerText >>= \case
      "on-demand" -> pure DTCTOnDemand
      "spot" -> pure DTCTSpot
      e ->
        fromTextError $
          "Failure parsing DefaultTargetCapacityType from value: '" <> e
            <> "'. Accepted values: on-demand, spot"

instance ToText DefaultTargetCapacityType where
  toText = \case
    DTCTOnDemand -> "on-demand"
    DTCTSpot -> "spot"

instance Hashable DefaultTargetCapacityType

instance NFData DefaultTargetCapacityType

instance ToByteString DefaultTargetCapacityType

instance ToQuery DefaultTargetCapacityType

instance ToHeader DefaultTargetCapacityType

instance FromXML DefaultTargetCapacityType where
  parseXML = parseXMLText "DefaultTargetCapacityType"
