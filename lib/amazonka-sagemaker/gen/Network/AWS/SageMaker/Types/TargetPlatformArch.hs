{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformArch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatformArch where

import Network.AWS.Prelude

data TargetPlatformArch
  = ARM64
  | ArmEabi
  | ArmEabihf
  | X86
  | X86_64
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

instance FromText TargetPlatformArch where
  parser =
    takeLowerText >>= \case
      "arm64" -> pure ARM64
      "arm_eabi" -> pure ArmEabi
      "arm_eabihf" -> pure ArmEabihf
      "x86" -> pure X86
      "x86_64" -> pure X86_64
      e ->
        fromTextError $
          "Failure parsing TargetPlatformArch from value: '" <> e
            <> "'. Accepted values: arm64, arm_eabi, arm_eabihf, x86, x86_64"

instance ToText TargetPlatformArch where
  toText = \case
    ARM64 -> "ARM64"
    ArmEabi -> "ARM_EABI"
    ArmEabihf -> "ARM_EABIHF"
    X86 -> "X86"
    X86_64 -> "X86_64"

instance Hashable TargetPlatformArch

instance NFData TargetPlatformArch

instance ToByteString TargetPlatformArch

instance ToQuery TargetPlatformArch

instance ToHeader TargetPlatformArch

instance ToJSON TargetPlatformArch where
  toJSON = toJSONText

instance FromJSON TargetPlatformArch where
  parseJSON = parseJSONText "TargetPlatformArch"
