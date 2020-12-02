{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParametersFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParametersFilterKey where

import Network.AWS.Prelude

data ParametersFilterKey
  = PFKKeyId
  | PFKName
  | PFKType
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

instance FromText ParametersFilterKey where
  parser =
    takeLowerText >>= \case
      "keyid" -> pure PFKKeyId
      "name" -> pure PFKName
      "type" -> pure PFKType
      e ->
        fromTextError $
          "Failure parsing ParametersFilterKey from value: '" <> e
            <> "'. Accepted values: keyid, name, type"

instance ToText ParametersFilterKey where
  toText = \case
    PFKKeyId -> "KeyId"
    PFKName -> "Name"
    PFKType -> "Type"

instance Hashable ParametersFilterKey

instance NFData ParametersFilterKey

instance ToByteString ParametersFilterKey

instance ToQuery ParametersFilterKey

instance ToHeader ParametersFilterKey

instance ToJSON ParametersFilterKey where
  toJSON = toJSONText
