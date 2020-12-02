{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceStatus
  = NISAssociated
  | NISAttaching
  | NISAvailable
  | NISDetaching
  | NISInUse
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

instance FromText NetworkInterfaceStatus where
  parser =
    takeLowerText >>= \case
      "associated" -> pure NISAssociated
      "attaching" -> pure NISAttaching
      "available" -> pure NISAvailable
      "detaching" -> pure NISDetaching
      "in-use" -> pure NISInUse
      e ->
        fromTextError $
          "Failure parsing NetworkInterfaceStatus from value: '" <> e
            <> "'. Accepted values: associated, attaching, available, detaching, in-use"

instance ToText NetworkInterfaceStatus where
  toText = \case
    NISAssociated -> "associated"
    NISAttaching -> "attaching"
    NISAvailable -> "available"
    NISDetaching -> "detaching"
    NISInUse -> "in-use"

instance Hashable NetworkInterfaceStatus

instance NFData NetworkInterfaceStatus

instance ToByteString NetworkInterfaceStatus

instance ToQuery NetworkInterfaceStatus

instance ToHeader NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
  parseXML = parseXMLText "NetworkInterfaceStatus"
