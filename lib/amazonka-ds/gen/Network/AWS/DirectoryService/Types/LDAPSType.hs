{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSType where

import Network.AWS.Prelude

data LDAPSType = Client
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

instance FromText LDAPSType where
  parser =
    takeLowerText >>= \case
      "client" -> pure Client
      e ->
        fromTextError $
          "Failure parsing LDAPSType from value: '" <> e
            <> "'. Accepted values: client"

instance ToText LDAPSType where
  toText = \case
    Client -> "Client"

instance Hashable LDAPSType

instance NFData LDAPSType

instance ToByteString LDAPSType

instance ToQuery LDAPSType

instance ToHeader LDAPSType

instance ToJSON LDAPSType where
  toJSON = toJSONText
