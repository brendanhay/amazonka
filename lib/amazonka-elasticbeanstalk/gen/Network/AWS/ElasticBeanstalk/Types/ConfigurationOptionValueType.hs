{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType where

import Network.AWS.Prelude

data ConfigurationOptionValueType
  = List
  | Scalar
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

instance FromText ConfigurationOptionValueType where
  parser =
    takeLowerText >>= \case
      "list" -> pure List
      "scalar" -> pure Scalar
      e ->
        fromTextError $
          "Failure parsing ConfigurationOptionValueType from value: '" <> e
            <> "'. Accepted values: list, scalar"

instance ToText ConfigurationOptionValueType where
  toText = \case
    List -> "List"
    Scalar -> "Scalar"

instance Hashable ConfigurationOptionValueType

instance NFData ConfigurationOptionValueType

instance ToByteString ConfigurationOptionValueType

instance ToQuery ConfigurationOptionValueType

instance ToHeader ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
  parseXML = parseXMLText "ConfigurationOptionValueType"
