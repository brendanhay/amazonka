{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderField where

import Network.AWS.Prelude

data CapacityProviderField = CPFTags
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

instance FromText CapacityProviderField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure CPFTags
      e ->
        fromTextError $
          "Failure parsing CapacityProviderField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText CapacityProviderField where
  toText = \case
    CPFTags -> "TAGS"

instance Hashable CapacityProviderField

instance NFData CapacityProviderField

instance ToByteString CapacityProviderField

instance ToQuery CapacityProviderField

instance ToHeader CapacityProviderField

instance ToJSON CapacityProviderField where
  toJSON = toJSONText
