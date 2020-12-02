{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ProvisioningType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ProvisioningType where

import Network.AWS.Prelude

data ProvisioningType
  = FullyMutable
  | Immutable
  | NonProvisionable
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

instance FromText ProvisioningType where
  parser =
    takeLowerText >>= \case
      "fully_mutable" -> pure FullyMutable
      "immutable" -> pure Immutable
      "non_provisionable" -> pure NonProvisionable
      e ->
        fromTextError $
          "Failure parsing ProvisioningType from value: '" <> e
            <> "'. Accepted values: fully_mutable, immutable, non_provisionable"

instance ToText ProvisioningType where
  toText = \case
    FullyMutable -> "FULLY_MUTABLE"
    Immutable -> "IMMUTABLE"
    NonProvisionable -> "NON_PROVISIONABLE"

instance Hashable ProvisioningType

instance NFData ProvisioningType

instance ToByteString ProvisioningType

instance ToQuery ProvisioningType

instance ToHeader ProvisioningType

instance FromXML ProvisioningType where
  parseXML = parseXMLText "ProvisioningType"
