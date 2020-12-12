{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModelFilterVariable
  ( MLModelFilterVariable
      ( MLModelFilterVariable',
        MLMFVAlgorithm,
        MLMFVCreatedAt,
        MLMFVIAMUser,
        MLMFVLastUpdatedAt,
        MLMFVMLModelType,
        MLMFVName,
        MLMFVRealtimeEndpointStatus,
        MLMFVStatus,
        MLMFVTrainingDataSourceId,
        MLMFVTrainingDataURI
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MLModelFilterVariable = MLModelFilterVariable' Lude.Text
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

pattern MLMFVAlgorithm :: MLModelFilterVariable
pattern MLMFVAlgorithm = MLModelFilterVariable' "Algorithm"

pattern MLMFVCreatedAt :: MLModelFilterVariable
pattern MLMFVCreatedAt = MLModelFilterVariable' "CreatedAt"

pattern MLMFVIAMUser :: MLModelFilterVariable
pattern MLMFVIAMUser = MLModelFilterVariable' "IAMUser"

pattern MLMFVLastUpdatedAt :: MLModelFilterVariable
pattern MLMFVLastUpdatedAt = MLModelFilterVariable' "LastUpdatedAt"

pattern MLMFVMLModelType :: MLModelFilterVariable
pattern MLMFVMLModelType = MLModelFilterVariable' "MLModelType"

pattern MLMFVName :: MLModelFilterVariable
pattern MLMFVName = MLModelFilterVariable' "Name"

pattern MLMFVRealtimeEndpointStatus :: MLModelFilterVariable
pattern MLMFVRealtimeEndpointStatus = MLModelFilterVariable' "RealtimeEndpointStatus"

pattern MLMFVStatus :: MLModelFilterVariable
pattern MLMFVStatus = MLModelFilterVariable' "Status"

pattern MLMFVTrainingDataSourceId :: MLModelFilterVariable
pattern MLMFVTrainingDataSourceId = MLModelFilterVariable' "TrainingDataSourceId"

pattern MLMFVTrainingDataURI :: MLModelFilterVariable
pattern MLMFVTrainingDataURI = MLModelFilterVariable' "TrainingDataURI"

{-# COMPLETE
  MLMFVAlgorithm,
  MLMFVCreatedAt,
  MLMFVIAMUser,
  MLMFVLastUpdatedAt,
  MLMFVMLModelType,
  MLMFVName,
  MLMFVRealtimeEndpointStatus,
  MLMFVStatus,
  MLMFVTrainingDataSourceId,
  MLMFVTrainingDataURI,
  MLModelFilterVariable'
  #-}
