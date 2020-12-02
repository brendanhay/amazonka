{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNConnectionStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNConnectionStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNConnectionStatusCode
  = CVCSCActive
  | CVCSCFailedToTerminate
  | CVCSCTerminated
  | CVCSCTerminating
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

instance FromText ClientVPNConnectionStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure CVCSCActive
      "failed-to-terminate" -> pure CVCSCFailedToTerminate
      "terminated" -> pure CVCSCTerminated
      "terminating" -> pure CVCSCTerminating
      e ->
        fromTextError $
          "Failure parsing ClientVPNConnectionStatusCode from value: '" <> e
            <> "'. Accepted values: active, failed-to-terminate, terminated, terminating"

instance ToText ClientVPNConnectionStatusCode where
  toText = \case
    CVCSCActive -> "active"
    CVCSCFailedToTerminate -> "failed-to-terminate"
    CVCSCTerminated -> "terminated"
    CVCSCTerminating -> "terminating"

instance Hashable ClientVPNConnectionStatusCode

instance NFData ClientVPNConnectionStatusCode

instance ToByteString ClientVPNConnectionStatusCode

instance ToQuery ClientVPNConnectionStatusCode

instance ToHeader ClientVPNConnectionStatusCode

instance FromXML ClientVPNConnectionStatusCode where
  parseXML = parseXMLText "ClientVPNConnectionStatusCode"
