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
        MLMFVCreatedAt,
        MLMFVLastUpdatedAt,
        MLMFVStatus,
        MLMFVName,
        MLMFVIAMUser,
        MLMFVTrainingDataSourceId,
        MLMFVRealtimeEndpointStatus,
        MLMFVMLModelType,
        MLMFVAlgorithm,
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

pattern MLMFVCreatedAt :: MLModelFilterVariable
pattern MLMFVCreatedAt = MLModelFilterVariable' "CreatedAt"

pattern MLMFVLastUpdatedAt :: MLModelFilterVariable
pattern MLMFVLastUpdatedAt = MLModelFilterVariable' "LastUpdatedAt"

pattern MLMFVStatus :: MLModelFilterVariable
pattern MLMFVStatus = MLModelFilterVariable' "Status"

pattern MLMFVName :: MLModelFilterVariable
pattern MLMFVName = MLModelFilterVariable' "Name"

pattern MLMFVIAMUser :: MLModelFilterVariable
pattern MLMFVIAMUser = MLModelFilterVariable' "IAMUser"

pattern MLMFVTrainingDataSourceId :: MLModelFilterVariable
pattern MLMFVTrainingDataSourceId = MLModelFilterVariable' "TrainingDataSourceId"

pattern MLMFVRealtimeEndpointStatus :: MLModelFilterVariable
pattern MLMFVRealtimeEndpointStatus = MLModelFilterVariable' "RealtimeEndpointStatus"

pattern MLMFVMLModelType :: MLModelFilterVariable
pattern MLMFVMLModelType = MLModelFilterVariable' "MLModelType"

pattern MLMFVAlgorithm :: MLModelFilterVariable
pattern MLMFVAlgorithm = MLModelFilterVariable' "Algorithm"

pattern MLMFVTrainingDataURI :: MLModelFilterVariable
pattern MLMFVTrainingDataURI = MLModelFilterVariable' "TrainingDataURI"

{-# COMPLETE
  MLMFVCreatedAt,
  MLMFVLastUpdatedAt,
  MLMFVStatus,
  MLMFVName,
  MLMFVIAMUser,
  MLMFVTrainingDataSourceId,
  MLMFVRealtimeEndpointStatus,
  MLMFVMLModelType,
  MLMFVAlgorithm,
  MLMFVTrainingDataURI,
  MLModelFilterVariable'
  #-}
