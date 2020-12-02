{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LayerFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerFailureCode where

import Network.AWS.Prelude

data LayerFailureCode
  = InvalidLayerDigest
  | MissingLayerDigest
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

instance FromText LayerFailureCode where
  parser =
    takeLowerText >>= \case
      "invalidlayerdigest" -> pure InvalidLayerDigest
      "missinglayerdigest" -> pure MissingLayerDigest
      e ->
        fromTextError $
          "Failure parsing LayerFailureCode from value: '" <> e
            <> "'. Accepted values: invalidlayerdigest, missinglayerdigest"

instance ToText LayerFailureCode where
  toText = \case
    InvalidLayerDigest -> "InvalidLayerDigest"
    MissingLayerDigest -> "MissingLayerDigest"

instance Hashable LayerFailureCode

instance NFData LayerFailureCode

instance ToByteString LayerFailureCode

instance ToQuery LayerFailureCode

instance ToHeader LayerFailureCode

instance FromJSON LayerFailureCode where
  parseJSON = parseJSONText "LayerFailureCode"
