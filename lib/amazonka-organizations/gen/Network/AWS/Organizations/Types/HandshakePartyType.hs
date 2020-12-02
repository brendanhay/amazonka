{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakePartyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakePartyType where

import Network.AWS.Prelude

data HandshakePartyType
  = HPTAccount
  | HPTEmail
  | HPTOrganization
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

instance FromText HandshakePartyType where
  parser =
    takeLowerText >>= \case
      "account" -> pure HPTAccount
      "email" -> pure HPTEmail
      "organization" -> pure HPTOrganization
      e ->
        fromTextError $
          "Failure parsing HandshakePartyType from value: '" <> e
            <> "'. Accepted values: account, email, organization"

instance ToText HandshakePartyType where
  toText = \case
    HPTAccount -> "ACCOUNT"
    HPTEmail -> "EMAIL"
    HPTOrganization -> "ORGANIZATION"

instance Hashable HandshakePartyType

instance NFData HandshakePartyType

instance ToByteString HandshakePartyType

instance ToQuery HandshakePartyType

instance ToHeader HandshakePartyType

instance ToJSON HandshakePartyType where
  toJSON = toJSONText

instance FromJSON HandshakePartyType where
  parseJSON = parseJSONText "HandshakePartyType"
