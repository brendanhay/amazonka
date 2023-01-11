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
-- Module      : Amazonka.SageMaker.Types.TargetPlatformAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TargetPlatformAccelerator
  ( TargetPlatformAccelerator
      ( ..,
        TargetPlatformAccelerator_INTEL_GRAPHICS,
        TargetPlatformAccelerator_MALI,
        TargetPlatformAccelerator_NNA,
        TargetPlatformAccelerator_NVIDIA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetPlatformAccelerator = TargetPlatformAccelerator'
  { fromTargetPlatformAccelerator ::
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

pattern TargetPlatformAccelerator_INTEL_GRAPHICS :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_INTEL_GRAPHICS = TargetPlatformAccelerator' "INTEL_GRAPHICS"

pattern TargetPlatformAccelerator_MALI :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_MALI = TargetPlatformAccelerator' "MALI"

pattern TargetPlatformAccelerator_NNA :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_NNA = TargetPlatformAccelerator' "NNA"

pattern TargetPlatformAccelerator_NVIDIA :: TargetPlatformAccelerator
pattern TargetPlatformAccelerator_NVIDIA = TargetPlatformAccelerator' "NVIDIA"

{-# COMPLETE
  TargetPlatformAccelerator_INTEL_GRAPHICS,
  TargetPlatformAccelerator_MALI,
  TargetPlatformAccelerator_NNA,
  TargetPlatformAccelerator_NVIDIA,
  TargetPlatformAccelerator'
  #-}
