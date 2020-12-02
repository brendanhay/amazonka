{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityType where

import Network.AWS.Prelude

data IdentityType
  = Domain
  | EmailAddress
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

instance FromText IdentityType where
  parser =
    takeLowerText >>= \case
      "domain" -> pure Domain
      "emailaddress" -> pure EmailAddress
      e ->
        fromTextError $
          "Failure parsing IdentityType from value: '" <> e
            <> "'. Accepted values: domain, emailaddress"

instance ToText IdentityType where
  toText = \case
    Domain -> "Domain"
    EmailAddress -> "EmailAddress"

instance Hashable IdentityType

instance NFData IdentityType

instance ToByteString IdentityType

instance ToQuery IdentityType

instance ToHeader IdentityType
