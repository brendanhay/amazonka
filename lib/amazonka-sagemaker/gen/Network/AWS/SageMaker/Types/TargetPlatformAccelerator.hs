{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatformAccelerator where

import Network.AWS.Prelude

data TargetPlatformAccelerator
  = IntelGraphics
  | Mali
  | Nvidia
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

instance FromText TargetPlatformAccelerator where
  parser =
    takeLowerText >>= \case
      "intel_graphics" -> pure IntelGraphics
      "mali" -> pure Mali
      "nvidia" -> pure Nvidia
      e ->
        fromTextError $
          "Failure parsing TargetPlatformAccelerator from value: '" <> e
            <> "'. Accepted values: intel_graphics, mali, nvidia"

instance ToText TargetPlatformAccelerator where
  toText = \case
    IntelGraphics -> "INTEL_GRAPHICS"
    Mali -> "MALI"
    Nvidia -> "NVIDIA"

instance Hashable TargetPlatformAccelerator

instance NFData TargetPlatformAccelerator

instance ToByteString TargetPlatformAccelerator

instance ToQuery TargetPlatformAccelerator

instance ToHeader TargetPlatformAccelerator

instance ToJSON TargetPlatformAccelerator where
  toJSON = toJSONText

instance FromJSON TargetPlatformAccelerator where
  parseJSON = parseJSONText "TargetPlatformAccelerator"
