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
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatformAccelerator
  ( TargetPlatformAccelerator
      ( ..,
        TargetPlatformAccelerator_INTEL_GRAPHICS,
        TargetPlatformAccelerator_MALI,
        TargetPlatformAccelerator_NVIDIA
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TargetPlatformAccelerator = TargetPlatformAccelerator'
  { fromTargetPlatformAccelerator ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern TargetPlatformAccelerator_INTEL_GRAPHICS :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_INTEL_GRAPHICS = TargetPlatformAccelerator' "INTEL_GRAPHICS"

pattern TargetPlatformAccelerator_MALI :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_MALI = TargetPlatformAccelerator' "MALI"

pattern TargetPlatformAccelerator_NVIDIA :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_NVIDIA = TargetPlatformAccelerator' "NVIDIA"

{-# COMPLETE
  TargetPlatformAccelerator_INTEL_GRAPHICS,
  TargetPlatformAccelerator_MALI,
  TargetPlatformAccelerator_NVIDIA,
  TargetPlatformAccelerator'
  #-}
