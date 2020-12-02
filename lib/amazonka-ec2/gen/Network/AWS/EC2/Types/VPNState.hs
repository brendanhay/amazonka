{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNState
  = VSAvailable
  | VSDeleted
  | VSDeleting
  | VSPending
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

instance FromText VPNState where
  parser =
    takeLowerText >>= \case
      "available" -> pure VSAvailable
      "deleted" -> pure VSDeleted
      "deleting" -> pure VSDeleting
      "pending" -> pure VSPending
      e ->
        fromTextError $
          "Failure parsing VPNState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText VPNState where
  toText = \case
    VSAvailable -> "available"
    VSDeleted -> "deleted"
    VSDeleting -> "deleting"
    VSPending -> "pending"

instance Hashable VPNState

instance NFData VPNState

instance ToByteString VPNState

instance ToQuery VPNState

instance ToHeader VPNState

instance FromXML VPNState where
  parseXML = parseXMLText "VPNState"
