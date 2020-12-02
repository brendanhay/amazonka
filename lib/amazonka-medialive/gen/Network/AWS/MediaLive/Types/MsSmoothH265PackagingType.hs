{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothH265PackagingType where

import Network.AWS.Prelude

-- | Ms Smooth H265 Packaging Type
data MsSmoothH265PackagingType
  = MSHPTHEV1
  | MSHPTHVC1
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

instance FromText MsSmoothH265PackagingType where
  parser =
    takeLowerText >>= \case
      "hev1" -> pure MSHPTHEV1
      "hvc1" -> pure MSHPTHVC1
      e ->
        fromTextError $
          "Failure parsing MsSmoothH265PackagingType from value: '" <> e
            <> "'. Accepted values: hev1, hvc1"

instance ToText MsSmoothH265PackagingType where
  toText = \case
    MSHPTHEV1 -> "HEV1"
    MSHPTHVC1 -> "HVC1"

instance Hashable MsSmoothH265PackagingType

instance NFData MsSmoothH265PackagingType

instance ToByteString MsSmoothH265PackagingType

instance ToQuery MsSmoothH265PackagingType

instance ToHeader MsSmoothH265PackagingType

instance ToJSON MsSmoothH265PackagingType where
  toJSON = toJSONText

instance FromJSON MsSmoothH265PackagingType where
  parseJSON = parseJSONText "MsSmoothH265PackagingType"
