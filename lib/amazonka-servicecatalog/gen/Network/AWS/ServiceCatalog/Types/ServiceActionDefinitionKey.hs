{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey where

import Network.AWS.Prelude

data ServiceActionDefinitionKey
  = AssumeRole
  | Name
  | Parameters
  | Version
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

instance FromText ServiceActionDefinitionKey where
  parser =
    takeLowerText >>= \case
      "assumerole" -> pure AssumeRole
      "name" -> pure Name
      "parameters" -> pure Parameters
      "version" -> pure Version
      e ->
        fromTextError $
          "Failure parsing ServiceActionDefinitionKey from value: '" <> e
            <> "'. Accepted values: assumerole, name, parameters, version"

instance ToText ServiceActionDefinitionKey where
  toText = \case
    AssumeRole -> "AssumeRole"
    Name -> "Name"
    Parameters -> "Parameters"
    Version -> "Version"

instance Hashable ServiceActionDefinitionKey

instance NFData ServiceActionDefinitionKey

instance ToByteString ServiceActionDefinitionKey

instance ToQuery ServiceActionDefinitionKey

instance ToHeader ServiceActionDefinitionKey

instance ToJSON ServiceActionDefinitionKey where
  toJSON = toJSONText

instance FromJSON ServiceActionDefinitionKey where
  parseJSON = parseJSONText "ServiceActionDefinitionKey"
