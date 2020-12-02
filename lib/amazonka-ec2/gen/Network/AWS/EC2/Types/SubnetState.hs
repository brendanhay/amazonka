{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SubnetState
  = SubAvailable
  | SubPending
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

instance FromText SubnetState where
  parser =
    takeLowerText >>= \case
      "available" -> pure SubAvailable
      "pending" -> pure SubPending
      e ->
        fromTextError $
          "Failure parsing SubnetState from value: '" <> e
            <> "'. Accepted values: available, pending"

instance ToText SubnetState where
  toText = \case
    SubAvailable -> "available"
    SubPending -> "pending"

instance Hashable SubnetState

instance NFData SubnetState

instance ToByteString SubnetState

instance ToQuery SubnetState

instance ToHeader SubnetState

instance FromXML SubnetState where
  parseXML = parseXMLText "SubnetState"
