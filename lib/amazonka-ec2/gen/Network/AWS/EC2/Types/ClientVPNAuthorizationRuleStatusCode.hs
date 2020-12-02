{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNAuthorizationRuleStatusCode
  = CVARSCActive
  | CVARSCAuthorizing
  | CVARSCFailed
  | CVARSCRevoking
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

instance FromText ClientVPNAuthorizationRuleStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure CVARSCActive
      "authorizing" -> pure CVARSCAuthorizing
      "failed" -> pure CVARSCFailed
      "revoking" -> pure CVARSCRevoking
      e ->
        fromTextError $
          "Failure parsing ClientVPNAuthorizationRuleStatusCode from value: '" <> e
            <> "'. Accepted values: active, authorizing, failed, revoking"

instance ToText ClientVPNAuthorizationRuleStatusCode where
  toText = \case
    CVARSCActive -> "active"
    CVARSCAuthorizing -> "authorizing"
    CVARSCFailed -> "failed"
    CVARSCRevoking -> "revoking"

instance Hashable ClientVPNAuthorizationRuleStatusCode

instance NFData ClientVPNAuthorizationRuleStatusCode

instance ToByteString ClientVPNAuthorizationRuleStatusCode

instance ToQuery ClientVPNAuthorizationRuleStatusCode

instance ToHeader ClientVPNAuthorizationRuleStatusCode

instance FromXML ClientVPNAuthorizationRuleStatusCode where
  parseXML = parseXMLText "ClientVPNAuthorizationRuleStatusCode"
