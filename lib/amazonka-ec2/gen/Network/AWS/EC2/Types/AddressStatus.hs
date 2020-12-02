{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AddressStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddressStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AddressStatus
  = InClassic
  | InVPC
  | MoveInProgress
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

instance FromText AddressStatus where
  parser =
    takeLowerText >>= \case
      "inclassic" -> pure InClassic
      "invpc" -> pure InVPC
      "moveinprogress" -> pure MoveInProgress
      e ->
        fromTextError $
          "Failure parsing AddressStatus from value: '" <> e
            <> "'. Accepted values: inclassic, invpc, moveinprogress"

instance ToText AddressStatus where
  toText = \case
    InClassic -> "InClassic"
    InVPC -> "InVpc"
    MoveInProgress -> "MoveInProgress"

instance Hashable AddressStatus

instance NFData AddressStatus

instance ToByteString AddressStatus

instance ToQuery AddressStatus

instance ToHeader AddressStatus

instance FromXML AddressStatus where
  parseXML = parseXMLText "AddressStatus"
