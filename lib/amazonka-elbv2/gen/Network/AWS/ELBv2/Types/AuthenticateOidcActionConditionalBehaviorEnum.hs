{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum where

import Network.AWS.Prelude

data AuthenticateOidcActionConditionalBehaviorEnum
  = AOACBEAllow
  | AOACBEAuthenticate
  | AOACBEDeny
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

instance FromText AuthenticateOidcActionConditionalBehaviorEnum where
  parser =
    takeLowerText >>= \case
      "allow" -> pure AOACBEAllow
      "authenticate" -> pure AOACBEAuthenticate
      "deny" -> pure AOACBEDeny
      e ->
        fromTextError $
          "Failure parsing AuthenticateOidcActionConditionalBehaviorEnum from value: '" <> e
            <> "'. Accepted values: allow, authenticate, deny"

instance ToText AuthenticateOidcActionConditionalBehaviorEnum where
  toText = \case
    AOACBEAllow -> "allow"
    AOACBEAuthenticate -> "authenticate"
    AOACBEDeny -> "deny"

instance Hashable AuthenticateOidcActionConditionalBehaviorEnum

instance NFData AuthenticateOidcActionConditionalBehaviorEnum

instance ToByteString AuthenticateOidcActionConditionalBehaviorEnum

instance ToQuery AuthenticateOidcActionConditionalBehaviorEnum

instance ToHeader AuthenticateOidcActionConditionalBehaviorEnum

instance FromXML AuthenticateOidcActionConditionalBehaviorEnum where
  parseXML = parseXMLText "AuthenticateOidcActionConditionalBehaviorEnum"
