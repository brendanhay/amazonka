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
        Auto,
        Linear,
        Logarithmic,
        ReverseLogarithmic
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HyperParameterScalingType = HyperParameterScalingType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Auto :: HyperParameterScalingType
pattern Auto = HyperParameterScalingType' "Auto"

pattern Linear :: HyperParameterScalingType
pattern Linear = HyperParameterScalingType' "Linear"

pattern Logarithmic :: HyperParameterScalingType
pattern Logarithmic = HyperParameterScalingType' "Logarithmic"

pattern ReverseLogarithmic :: HyperParameterScalingType
pattern ReverseLogarithmic = HyperParameterScalingType' "ReverseLogarithmic"

{-# COMPLETE
  Auto,
  Linear,
  Logarithmic,
  ReverseLogarithmic,
  HyperParameterScalingType'
  #-}
