{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterScalingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterScalingType
  ( HyperParameterScalingType
      ( HyperParameterScalingType',
        HyperParameterScalingTypeAuto,
        HyperParameterScalingTypeLinear,
        HyperParameterScalingTypeLogarithmic,
        HyperParameterScalingTypeReverseLogarithmic,
        fromHyperParameterScalingType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HyperParameterScalingType = HyperParameterScalingType'
  { fromHyperParameterScalingType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HyperParameterScalingTypeAuto :: HyperParameterScalingType
pattern HyperParameterScalingTypeAuto = HyperParameterScalingType' "Auto"

pattern HyperParameterScalingTypeLinear :: HyperParameterScalingType
pattern HyperParameterScalingTypeLinear = HyperParameterScalingType' "Linear"

pattern HyperParameterScalingTypeLogarithmic :: HyperParameterScalingType
pattern HyperParameterScalingTypeLogarithmic = HyperParameterScalingType' "Logarithmic"

pattern HyperParameterScalingTypeReverseLogarithmic :: HyperParameterScalingType
pattern HyperParameterScalingTypeReverseLogarithmic = HyperParameterScalingType' "ReverseLogarithmic"

{-# COMPLETE
  HyperParameterScalingTypeAuto,
  HyperParameterScalingTypeLinear,
  HyperParameterScalingTypeLogarithmic,
  HyperParameterScalingTypeReverseLogarithmic,
  HyperParameterScalingType'
  #-}
