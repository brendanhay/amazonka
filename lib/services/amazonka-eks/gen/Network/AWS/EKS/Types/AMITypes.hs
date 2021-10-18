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
-- Module      : Network.AWS.EKS.Types.AMITypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AMITypes
  ( AMITypes
      ( ..,
        AMITypes_AL2_ARM_64,
        AMITypes_AL2_x86_64,
        AMITypes_AL2_x86_64_GPU,
        AMITypes_CUSTOM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AMITypes = AMITypes'
  { fromAMITypes ::
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

pattern AMITypes_AL2_ARM_64 :: AMITypes
pattern AMITypes_AL2_ARM_64 = AMITypes' "AL2_ARM_64"

pattern AMITypes_AL2_x86_64 :: AMITypes
pattern AMITypes_AL2_x86_64 = AMITypes' "AL2_x86_64"

pattern AMITypes_AL2_x86_64_GPU :: AMITypes
pattern AMITypes_AL2_x86_64_GPU = AMITypes' "AL2_x86_64_GPU"

pattern AMITypes_CUSTOM :: AMITypes
pattern AMITypes_CUSTOM = AMITypes' "CUSTOM"

{-# COMPLETE
  AMITypes_AL2_ARM_64,
  AMITypes_AL2_x86_64,
  AMITypes_AL2_x86_64_GPU,
  AMITypes_CUSTOM,
  AMITypes'
  #-}
