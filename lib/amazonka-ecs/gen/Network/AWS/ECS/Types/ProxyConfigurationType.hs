{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ProxyConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ProxyConfigurationType where

import Network.AWS.Prelude

data ProxyConfigurationType = Appmesh
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

instance FromText ProxyConfigurationType where
  parser =
    takeLowerText >>= \case
      "appmesh" -> pure Appmesh
      e ->
        fromTextError $
          "Failure parsing ProxyConfigurationType from value: '" <> e
            <> "'. Accepted values: appmesh"

instance ToText ProxyConfigurationType where
  toText = \case
    Appmesh -> "APPMESH"

instance Hashable ProxyConfigurationType

instance NFData ProxyConfigurationType

instance ToByteString ProxyConfigurationType

instance ToQuery ProxyConfigurationType

instance ToHeader ProxyConfigurationType

instance ToJSON ProxyConfigurationType where
  toJSON = toJSONText

instance FromJSON ProxyConfigurationType where
  parseJSON = parseJSONText "ProxyConfigurationType"
