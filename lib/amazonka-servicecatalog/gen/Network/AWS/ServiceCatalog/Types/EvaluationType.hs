{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.EvaluationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.EvaluationType where

import Network.AWS.Prelude

data EvaluationType
  = Dynamic
  | Static
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

instance FromText EvaluationType where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure Dynamic
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing EvaluationType from value: '" <> e
            <> "'. Accepted values: dynamic, static"

instance ToText EvaluationType where
  toText = \case
    Dynamic -> "DYNAMIC"
    Static -> "STATIC"

instance Hashable EvaluationType

instance NFData EvaluationType

instance ToByteString EvaluationType

instance ToQuery EvaluationType

instance ToHeader EvaluationType

instance FromJSON EvaluationType where
  parseJSON = parseJSONText "EvaluationType"
