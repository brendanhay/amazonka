{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpointStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNEndpointStatusCode
  = CVESCAvailable
  | CVESCDeleted
  | CVESCDeleting
  | CVESCPendingAssociate
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

instance FromText ClientVPNEndpointStatusCode where
  parser =
    takeLowerText >>= \case
      "available" -> pure CVESCAvailable
      "deleted" -> pure CVESCDeleted
      "deleting" -> pure CVESCDeleting
      "pending-associate" -> pure CVESCPendingAssociate
      e ->
        fromTextError $
          "Failure parsing ClientVPNEndpointStatusCode from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending-associate"

instance ToText ClientVPNEndpointStatusCode where
  toText = \case
    CVESCAvailable -> "available"
    CVESCDeleted -> "deleted"
    CVESCDeleting -> "deleting"
    CVESCPendingAssociate -> "pending-associate"

instance Hashable ClientVPNEndpointStatusCode

instance NFData ClientVPNEndpointStatusCode

instance ToByteString ClientVPNEndpointStatusCode

instance ToQuery ClientVPNEndpointStatusCode

instance ToHeader ClientVPNEndpointStatusCode

instance FromXML ClientVPNEndpointStatusCode where
  parseXML = parseXMLText "ClientVPNEndpointStatusCode"
