{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCState
  = VPCSAvailable
  | VPCSPending
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

instance FromText VPCState where
  parser =
    takeLowerText >>= \case
      "available" -> pure VPCSAvailable
      "pending" -> pure VPCSPending
      e ->
        fromTextError $
          "Failure parsing VPCState from value: '" <> e
            <> "'. Accepted values: available, pending"

instance ToText VPCState where
  toText = \case
    VPCSAvailable -> "available"
    VPCSPending -> "pending"

instance Hashable VPCState

instance NFData VPCState

instance ToByteString VPCState

instance ToQuery VPCState

instance ToHeader VPCState

instance FromXML VPCState where
  parseXML = parseXMLText "VPCState"
