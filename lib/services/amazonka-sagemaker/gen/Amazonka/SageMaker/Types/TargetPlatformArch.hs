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
-- Module      : Amazonka.SageMaker.Types.TargetPlatformArch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TargetPlatformArch
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetPlatformArch = TargetPlatformArch'
  { fromTargetPlatformArch ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
