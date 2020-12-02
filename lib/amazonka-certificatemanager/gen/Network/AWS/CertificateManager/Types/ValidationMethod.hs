{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ValidationMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ValidationMethod where

import Network.AWS.Prelude

data ValidationMethod
  = DNS
  | Email
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

instance FromText ValidationMethod where
  parser =
    takeLowerText >>= \case
      "dns" -> pure DNS
      "email" -> pure Email
      e ->
        fromTextError $
          "Failure parsing ValidationMethod from value: '" <> e
            <> "'. Accepted values: dns, email"

instance ToText ValidationMethod where
  toText = \case
    DNS -> "DNS"
    Email -> "EMAIL"

instance Hashable ValidationMethod

instance NFData ValidationMethod

instance ToByteString ValidationMethod

instance ToQuery ValidationMethod

instance ToHeader ValidationMethod

instance ToJSON ValidationMethod where
  toJSON = toJSONText

instance FromJSON ValidationMethod where
  parseJSON = parseJSONText "ValidationMethod"
