{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode where

import Network.AWS.Prelude

data PiiEntitiesDetectionMaskMode
  = Mask
  | ReplaceWithPiiEntityType
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

instance FromText PiiEntitiesDetectionMaskMode where
  parser =
    takeLowerText >>= \case
      "mask" -> pure Mask
      "replace_with_pii_entity_type" -> pure ReplaceWithPiiEntityType
      e ->
        fromTextError $
          "Failure parsing PiiEntitiesDetectionMaskMode from value: '" <> e
            <> "'. Accepted values: mask, replace_with_pii_entity_type"

instance ToText PiiEntitiesDetectionMaskMode where
  toText = \case
    Mask -> "MASK"
    ReplaceWithPiiEntityType -> "REPLACE_WITH_PII_ENTITY_TYPE"

instance Hashable PiiEntitiesDetectionMaskMode

instance NFData PiiEntitiesDetectionMaskMode

instance ToByteString PiiEntitiesDetectionMaskMode

instance ToQuery PiiEntitiesDetectionMaskMode

instance ToHeader PiiEntitiesDetectionMaskMode

instance ToJSON PiiEntitiesDetectionMaskMode where
  toJSON = toJSONText

instance FromJSON PiiEntitiesDetectionMaskMode where
  parseJSON = parseJSONText "PiiEntitiesDetectionMaskMode"
