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
-- Module      : Amazonka.EKS.Types.AMITypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AMITypes
  ( AMITypes
      ( ..,
        AMITypes_AL2_ARM_64,
        AMITypes_AL2_x86_64,
        AMITypes_AL2_x86_64_GPU,
        AMITypes_BOTTLEROCKET_ARM_64,
        AMITypes_BOTTLEROCKET_ARM_64_NVIDIA,
        AMITypes_BOTTLEROCKET_x86_64,
        AMITypes_BOTTLEROCKET_x86_64_NVIDIA,
        AMITypes_CUSTOM,
        AMITypes_WINDOWS_CORE_2019_x86_64,
        AMITypes_WINDOWS_CORE_2022_x86_64,
        AMITypes_WINDOWS_FULL_2019_x86_64,
        AMITypes_WINDOWS_FULL_2022_x86_64
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AMITypes = AMITypes'
  { fromAMITypes ::
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

pattern AMITypes_AL2_ARM_64 :: AMITypes
pattern AMITypes_AL2_ARM_64 = AMITypes' "AL2_ARM_64"

pattern AMITypes_AL2_x86_64 :: AMITypes
pattern AMITypes_AL2_x86_64 = AMITypes' "AL2_x86_64"

pattern AMITypes_AL2_x86_64_GPU :: AMITypes
pattern AMITypes_AL2_x86_64_GPU = AMITypes' "AL2_x86_64_GPU"

pattern AMITypes_BOTTLEROCKET_ARM_64 :: AMITypes
pattern AMITypes_BOTTLEROCKET_ARM_64 = AMITypes' "BOTTLEROCKET_ARM_64"

pattern AMITypes_BOTTLEROCKET_ARM_64_NVIDIA :: AMITypes
pattern AMITypes_BOTTLEROCKET_ARM_64_NVIDIA = AMITypes' "BOTTLEROCKET_ARM_64_NVIDIA"

pattern AMITypes_BOTTLEROCKET_x86_64 :: AMITypes
pattern AMITypes_BOTTLEROCKET_x86_64 = AMITypes' "BOTTLEROCKET_x86_64"

pattern AMITypes_BOTTLEROCKET_x86_64_NVIDIA :: AMITypes
pattern AMITypes_BOTTLEROCKET_x86_64_NVIDIA = AMITypes' "BOTTLEROCKET_x86_64_NVIDIA"

pattern AMITypes_CUSTOM :: AMITypes
pattern AMITypes_CUSTOM = AMITypes' "CUSTOM"

pattern AMITypes_WINDOWS_CORE_2019_x86_64 :: AMITypes
pattern AMITypes_WINDOWS_CORE_2019_x86_64 = AMITypes' "WINDOWS_CORE_2019_x86_64"

pattern AMITypes_WINDOWS_CORE_2022_x86_64 :: AMITypes
pattern AMITypes_WINDOWS_CORE_2022_x86_64 = AMITypes' "WINDOWS_CORE_2022_x86_64"

pattern AMITypes_WINDOWS_FULL_2019_x86_64 :: AMITypes
pattern AMITypes_WINDOWS_FULL_2019_x86_64 = AMITypes' "WINDOWS_FULL_2019_x86_64"

pattern AMITypes_WINDOWS_FULL_2022_x86_64 :: AMITypes
pattern AMITypes_WINDOWS_FULL_2022_x86_64 = AMITypes' "WINDOWS_FULL_2022_x86_64"

{-# COMPLETE
  AMITypes_AL2_ARM_64,
  AMITypes_AL2_x86_64,
  AMITypes_AL2_x86_64_GPU,
  AMITypes_BOTTLEROCKET_ARM_64,
  AMITypes_BOTTLEROCKET_ARM_64_NVIDIA,
  AMITypes_BOTTLEROCKET_x86_64,
  AMITypes_BOTTLEROCKET_x86_64_NVIDIA,
  AMITypes_CUSTOM,
  AMITypes_WINDOWS_CORE_2019_x86_64,
  AMITypes_WINDOWS_CORE_2022_x86_64,
  AMITypes_WINDOWS_FULL_2019_x86_64,
  AMITypes_WINDOWS_FULL_2022_x86_64,
  AMITypes'
  #-}
