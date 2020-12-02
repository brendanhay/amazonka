{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentType where

import Network.AWS.Prelude

data ProtectiveEquipmentType
  = FaceCover
  | HandCover
  | HeadCover
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

instance FromText ProtectiveEquipmentType where
  parser =
    takeLowerText >>= \case
      "face_cover" -> pure FaceCover
      "hand_cover" -> pure HandCover
      "head_cover" -> pure HeadCover
      e ->
        fromTextError $
          "Failure parsing ProtectiveEquipmentType from value: '" <> e
            <> "'. Accepted values: face_cover, hand_cover, head_cover"

instance ToText ProtectiveEquipmentType where
  toText = \case
    FaceCover -> "FACE_COVER"
    HandCover -> "HAND_COVER"
    HeadCover -> "HEAD_COVER"

instance Hashable ProtectiveEquipmentType

instance NFData ProtectiveEquipmentType

instance ToByteString ProtectiveEquipmentType

instance ToQuery ProtectiveEquipmentType

instance ToHeader ProtectiveEquipmentType

instance ToJSON ProtectiveEquipmentType where
  toJSON = toJSONText

instance FromJSON ProtectiveEquipmentType where
  parseJSON = parseJSONText "ProtectiveEquipmentType"
