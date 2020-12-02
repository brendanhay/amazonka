{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode where

import Network.AWS.Prelude

data PiiEntitiesDetectionMode
  = OnlyOffsets
  | OnlyRedaction
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

instance FromText PiiEntitiesDetectionMode where
  parser =
    takeLowerText >>= \case
      "only_offsets" -> pure OnlyOffsets
      "only_redaction" -> pure OnlyRedaction
      e ->
        fromTextError $
          "Failure parsing PiiEntitiesDetectionMode from value: '" <> e
            <> "'. Accepted values: only_offsets, only_redaction"

instance ToText PiiEntitiesDetectionMode where
  toText = \case
    OnlyOffsets -> "ONLY_OFFSETS"
    OnlyRedaction -> "ONLY_REDACTION"

instance Hashable PiiEntitiesDetectionMode

instance NFData PiiEntitiesDetectionMode

instance ToByteString PiiEntitiesDetectionMode

instance ToQuery PiiEntitiesDetectionMode

instance ToHeader PiiEntitiesDetectionMode

instance ToJSON PiiEntitiesDetectionMode where
  toJSON = toJSONText

instance FromJSON PiiEntitiesDetectionMode where
  parseJSON = parseJSONText "PiiEntitiesDetectionMode"
