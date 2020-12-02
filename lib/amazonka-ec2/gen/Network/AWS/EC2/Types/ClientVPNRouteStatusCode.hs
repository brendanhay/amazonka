{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRouteStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRouteStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNRouteStatusCode
  = CVRSCActive
  | CVRSCCreating
  | CVRSCDeleting
  | CVRSCFailed
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

instance FromText ClientVPNRouteStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure CVRSCActive
      "creating" -> pure CVRSCCreating
      "deleting" -> pure CVRSCDeleting
      "failed" -> pure CVRSCFailed
      e ->
        fromTextError $
          "Failure parsing ClientVPNRouteStatusCode from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, failed"

instance ToText ClientVPNRouteStatusCode where
  toText = \case
    CVRSCActive -> "active"
    CVRSCCreating -> "creating"
    CVRSCDeleting -> "deleting"
    CVRSCFailed -> "failed"

instance Hashable ClientVPNRouteStatusCode

instance NFData ClientVPNRouteStatusCode

instance ToByteString ClientVPNRouteStatusCode

instance ToQuery ClientVPNRouteStatusCode

instance ToHeader ClientVPNRouteStatusCode

instance FromXML ClientVPNRouteStatusCode where
  parseXML = parseXMLText "ClientVPNRouteStatusCode"
