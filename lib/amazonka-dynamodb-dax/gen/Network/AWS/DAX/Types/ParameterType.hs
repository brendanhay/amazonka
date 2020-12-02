{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterType where

import Network.AWS.Prelude

data ParameterType
  = Default
  | NodeTypeSpecific
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

instance FromText ParameterType where
  parser =
    takeLowerText >>= \case
      "default" -> pure Default
      "node_type_specific" -> pure NodeTypeSpecific
      e ->
        fromTextError $
          "Failure parsing ParameterType from value: '" <> e
            <> "'. Accepted values: default, node_type_specific"

instance ToText ParameterType where
  toText = \case
    Default -> "DEFAULT"
    NodeTypeSpecific -> "NODE_TYPE_SPECIFIC"

instance Hashable ParameterType

instance NFData ParameterType

instance ToByteString ParameterType

instance ToQuery ParameterType

instance ToHeader ParameterType

instance FromJSON ParameterType where
  parseJSON = parseJSONText "ParameterType"
