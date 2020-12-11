-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
  ( HyperParameterTuningJobWarmStartType
      ( HyperParameterTuningJobWarmStartType',
        IdenticalDataAndAlgorithm,
        TransferLearning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HyperParameterTuningJobWarmStartType = HyperParameterTuningJobWarmStartType' Lude.Text
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

pattern IdenticalDataAndAlgorithm :: HyperParameterTuningJobWarmStartType
pattern IdenticalDataAndAlgorithm = HyperParameterTuningJobWarmStartType' "IdenticalDataAndAlgorithm"

pattern TransferLearning :: HyperParameterTuningJobWarmStartType
pattern TransferLearning = HyperParameterTuningJobWarmStartType' "TransferLearning"

{-# COMPLETE
  IdenticalDataAndAlgorithm,
  TransferLearning,
  HyperParameterTuningJobWarmStartType'
  #-}
