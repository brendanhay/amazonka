{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTenancy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CapacityReservationTenancy
  = CRTDedicated
  | CRTDefault
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

instance FromText CapacityReservationTenancy where
  parser =
    takeLowerText >>= \case
      "dedicated" -> pure CRTDedicated
      "default" -> pure CRTDefault
      e ->
        fromTextError $
          "Failure parsing CapacityReservationTenancy from value: '" <> e
            <> "'. Accepted values: dedicated, default"

instance ToText CapacityReservationTenancy where
  toText = \case
    CRTDedicated -> "dedicated"
    CRTDefault -> "default"

instance Hashable CapacityReservationTenancy

instance NFData CapacityReservationTenancy

instance ToByteString CapacityReservationTenancy

instance ToQuery CapacityReservationTenancy

instance ToHeader CapacityReservationTenancy

instance FromXML CapacityReservationTenancy where
  parseXML = parseXMLText "CapacityReservationTenancy"
