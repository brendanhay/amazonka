{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformArch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TargetPlatformArch
  ( TargetPlatformArch
    ( TargetPlatformArch'
    , TargetPlatformArchX8664
    , TargetPlatformArchX86
    , TargetPlatformArchARM64
    , TargetPlatformArchArmEabi
    , TargetPlatformArchArmEabihf
    , fromTargetPlatformArch
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TargetPlatformArch = TargetPlatformArch'{fromTargetPlatformArch
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern TargetPlatformArchX8664 :: TargetPlatformArch
pattern TargetPlatformArchX8664 = TargetPlatformArch' "X86_64"

pattern TargetPlatformArchX86 :: TargetPlatformArch
pattern TargetPlatformArchX86 = TargetPlatformArch' "X86"

pattern TargetPlatformArchARM64 :: TargetPlatformArch
pattern TargetPlatformArchARM64 = TargetPlatformArch' "ARM64"

pattern TargetPlatformArchArmEabi :: TargetPlatformArch
pattern TargetPlatformArchArmEabi = TargetPlatformArch' "ARM_EABI"

pattern TargetPlatformArchArmEabihf :: TargetPlatformArch
pattern TargetPlatformArchArmEabihf = TargetPlatformArch' "ARM_EABIHF"

{-# COMPLETE 
  TargetPlatformArchX8664,

  TargetPlatformArchX86,

  TargetPlatformArchARM64,

  TargetPlatformArchArmEabi,

  TargetPlatformArchArmEabihf,
  TargetPlatformArch'
  #-}
