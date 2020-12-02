{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RegistryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RegistryType where

import Network.AWS.Prelude

data RegistryType
  = Module
  | Resource
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

instance FromText RegistryType where
  parser =
    takeLowerText >>= \case
      "module" -> pure Module
      "resource" -> pure Resource
      e ->
        fromTextError $
          "Failure parsing RegistryType from value: '" <> e
            <> "'. Accepted values: module, resource"

instance ToText RegistryType where
  toText = \case
    Module -> "MODULE"
    Resource -> "RESOURCE"

instance Hashable RegistryType

instance NFData RegistryType

instance ToByteString RegistryType

instance ToQuery RegistryType

instance ToHeader RegistryType

instance FromXML RegistryType where
  parseXML = parseXMLText "RegistryType"
