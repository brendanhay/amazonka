{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformArch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatformArch
  ( TargetPlatformArch
      ( ..,
        TargetPlatformArch_ARM64,
        TargetPlatformArch_ARM_EABI,
        TargetPlatformArch_ARM_EABIHF,
        TargetPlatformArch_X86,
        TargetPlatformArch_X86_64
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TargetPlatformArch = TargetPlatformArch'
  { fromTargetPlatformArch ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern TargetPlatformArch_ARM64 :: TargetPlatformArch
pattern TargetPlatformArch_ARM64 = TargetPlatformArch' "ARM64"

pattern TargetPlatformArch_ARM_EABI :: TargetPlatformArch
pattern TargetPlatformArch_ARM_EABI = TargetPlatformArch' "ARM_EABI"

pattern TargetPlatformArch_ARM_EABIHF :: TargetPlatformArch
pattern TargetPlatformArch_ARM_EABIHF = TargetPlatformArch' "ARM_EABIHF"

pattern TargetPlatformArch_X86 :: TargetPlatformArch
pattern TargetPlatformArch_X86 = TargetPlatformArch' "X86"

pattern TargetPlatformArch_X86_64 :: TargetPlatformArch
pattern TargetPlatformArch_X86_64 = TargetPlatformArch' "X86_64"

{-# COMPLETE
  TargetPlatformArch_ARM64,
  TargetPlatformArch_ARM_EABI,
  TargetPlatformArch_ARM_EABIHF,
  TargetPlatformArch_X86,
  TargetPlatformArch_X86_64,
  TargetPlatformArch'
  #-}
