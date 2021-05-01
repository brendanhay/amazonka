{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype TargetPlatformAccelerator = TargetPlatformAccelerator'
  { fromTargetPlatformAccelerator ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
