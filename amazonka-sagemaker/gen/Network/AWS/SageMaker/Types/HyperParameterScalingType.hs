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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterScalingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterScalingType
  ( HyperParameterScalingType
      ( ..,
        HyperParameterScalingType_Auto,
        HyperParameterScalingType_Linear,
        HyperParameterScalingType_Logarithmic,
        HyperParameterScalingType_ReverseLogarithmic
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HyperParameterScalingType = HyperParameterScalingType'
  { fromHyperParameterScalingType ::
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

pattern HyperParameterScalingType_Auto :: HyperParameterScalingType
pattern HyperParameterScalingType_Auto = HyperParameterScalingType' "Auto"

pattern HyperParameterScalingType_Linear :: HyperParameterScalingType
pattern HyperParameterScalingType_Linear = HyperParameterScalingType' "Linear"

pattern HyperParameterScalingType_Logarithmic :: HyperParameterScalingType
pattern HyperParameterScalingType_Logarithmic = HyperParameterScalingType' "Logarithmic"

pattern HyperParameterScalingType_ReverseLogarithmic :: HyperParameterScalingType
pattern HyperParameterScalingType_ReverseLogarithmic = HyperParameterScalingType' "ReverseLogarithmic"

{-# COMPLETE
  HyperParameterScalingType_Auto,
  HyperParameterScalingType_Linear,
  HyperParameterScalingType_Logarithmic,
  HyperParameterScalingType_ReverseLogarithmic,
  HyperParameterScalingType'
  #-}
