{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthDecision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthDecision where

import Network.AWS.Prelude

data AuthDecision
  = Allowed
  | ExplicitDeny
  | ImplicitDeny
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

instance FromText AuthDecision where
  parser =
    takeLowerText >>= \case
      "allowed" -> pure Allowed
      "explicit_deny" -> pure ExplicitDeny
      "implicit_deny" -> pure ImplicitDeny
      e ->
        fromTextError $
          "Failure parsing AuthDecision from value: '" <> e
            <> "'. Accepted values: allowed, explicit_deny, implicit_deny"

instance ToText AuthDecision where
  toText = \case
    Allowed -> "ALLOWED"
    ExplicitDeny -> "EXPLICIT_DENY"
    ImplicitDeny -> "IMPLICIT_DENY"

instance Hashable AuthDecision

instance NFData AuthDecision

instance ToByteString AuthDecision

instance ToQuery AuthDecision

instance ToHeader AuthDecision

instance FromJSON AuthDecision where
  parseJSON = parseJSONText "AuthDecision"
