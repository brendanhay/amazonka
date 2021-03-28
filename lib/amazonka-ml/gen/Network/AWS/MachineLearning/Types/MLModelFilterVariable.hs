{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.MLModelFilterVariable
  ( MLModelFilterVariable
    ( MLModelFilterVariable'
    , MLModelFilterVariableCreatedAt
    , MLModelFilterVariableLastUpdatedAt
    , MLModelFilterVariableStatus
    , MLModelFilterVariableName
    , MLModelFilterVariableIAMUser
    , MLModelFilterVariableTrainingDataSourceId
    , MLModelFilterVariableRealtimeEndpointStatus
    , MLModelFilterVariableMLModelType
    , MLModelFilterVariableAlgorithm
    , MLModelFilterVariableTrainingDataURI
    , fromMLModelFilterVariable
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MLModelFilterVariable = MLModelFilterVariable'{fromMLModelFilterVariable
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern MLModelFilterVariableCreatedAt :: MLModelFilterVariable
pattern MLModelFilterVariableCreatedAt = MLModelFilterVariable' "CreatedAt"

pattern MLModelFilterVariableLastUpdatedAt :: MLModelFilterVariable
pattern MLModelFilterVariableLastUpdatedAt = MLModelFilterVariable' "LastUpdatedAt"

pattern MLModelFilterVariableStatus :: MLModelFilterVariable
pattern MLModelFilterVariableStatus = MLModelFilterVariable' "Status"

pattern MLModelFilterVariableName :: MLModelFilterVariable
pattern MLModelFilterVariableName = MLModelFilterVariable' "Name"

pattern MLModelFilterVariableIAMUser :: MLModelFilterVariable
pattern MLModelFilterVariableIAMUser = MLModelFilterVariable' "IAMUser"

pattern MLModelFilterVariableTrainingDataSourceId :: MLModelFilterVariable
pattern MLModelFilterVariableTrainingDataSourceId = MLModelFilterVariable' "TrainingDataSourceId"

pattern MLModelFilterVariableRealtimeEndpointStatus :: MLModelFilterVariable
pattern MLModelFilterVariableRealtimeEndpointStatus = MLModelFilterVariable' "RealtimeEndpointStatus"

pattern MLModelFilterVariableMLModelType :: MLModelFilterVariable
pattern MLModelFilterVariableMLModelType = MLModelFilterVariable' "MLModelType"

pattern MLModelFilterVariableAlgorithm :: MLModelFilterVariable
pattern MLModelFilterVariableAlgorithm = MLModelFilterVariable' "Algorithm"

pattern MLModelFilterVariableTrainingDataURI :: MLModelFilterVariable
pattern MLModelFilterVariableTrainingDataURI = MLModelFilterVariable' "TrainingDataURI"

{-# COMPLETE 
  MLModelFilterVariableCreatedAt,

  MLModelFilterVariableLastUpdatedAt,

  MLModelFilterVariableStatus,

  MLModelFilterVariableName,

  MLModelFilterVariableIAMUser,

  MLModelFilterVariableTrainingDataSourceId,

  MLModelFilterVariableRealtimeEndpointStatus,

  MLModelFilterVariableMLModelType,

  MLModelFilterVariableAlgorithm,

  MLModelFilterVariableTrainingDataURI,
  MLModelFilterVariable'
  #-}
