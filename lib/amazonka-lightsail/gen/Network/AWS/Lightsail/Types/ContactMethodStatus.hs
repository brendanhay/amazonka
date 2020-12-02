{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethodStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethodStatus where

import Network.AWS.Prelude

data ContactMethodStatus
  = Invalid
  | PendingVerification
  | Valid
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

instance FromText ContactMethodStatus where
  parser =
    takeLowerText >>= \case
      "invalid" -> pure Invalid
      "pendingverification" -> pure PendingVerification
      "valid" -> pure Valid
      e ->
        fromTextError $
          "Failure parsing ContactMethodStatus from value: '" <> e
            <> "'. Accepted values: invalid, pendingverification, valid"

instance ToText ContactMethodStatus where
  toText = \case
    Invalid -> "Invalid"
    PendingVerification -> "PendingVerification"
    Valid -> "Valid"

instance Hashable ContactMethodStatus

instance NFData ContactMethodStatus

instance ToByteString ContactMethodStatus

instance ToQuery ContactMethodStatus

instance ToHeader ContactMethodStatus

instance FromJSON ContactMethodStatus where
  parseJSON = parseJSONText "ContactMethodStatus"
