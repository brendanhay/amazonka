{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol where

import Network.AWS.Prelude

data ContactMethodVerificationProtocol = CMVPEmail
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

instance FromText ContactMethodVerificationProtocol where
  parser =
    takeLowerText >>= \case
      "email" -> pure CMVPEmail
      e ->
        fromTextError $
          "Failure parsing ContactMethodVerificationProtocol from value: '" <> e
            <> "'. Accepted values: email"

instance ToText ContactMethodVerificationProtocol where
  toText = \case
    CMVPEmail -> "Email"

instance Hashable ContactMethodVerificationProtocol

instance NFData ContactMethodVerificationProtocol

instance ToByteString ContactMethodVerificationProtocol

instance ToQuery ContactMethodVerificationProtocol

instance ToHeader ContactMethodVerificationProtocol

instance ToJSON ContactMethodVerificationProtocol where
  toJSON = toJSONText
