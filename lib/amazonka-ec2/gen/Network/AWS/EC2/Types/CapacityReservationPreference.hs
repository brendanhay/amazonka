{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationPreference where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CapacityReservationPreference
  = None
  | Open
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

instance FromText CapacityReservationPreference where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "open" -> pure Open
      e ->
        fromTextError $
          "Failure parsing CapacityReservationPreference from value: '" <> e
            <> "'. Accepted values: none, open"

instance ToText CapacityReservationPreference where
  toText = \case
    None -> "none"
    Open -> "open"

instance Hashable CapacityReservationPreference

instance NFData CapacityReservationPreference

instance ToByteString CapacityReservationPreference

instance ToQuery CapacityReservationPreference

instance ToHeader CapacityReservationPreference

instance FromXML CapacityReservationPreference where
  parseXML = parseXMLText "CapacityReservationPreference"
