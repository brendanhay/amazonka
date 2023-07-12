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
-- Module      : Amazonka.SageMaker.Types.ResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_Endpoint,
        ResourceType_Experiment,
        ResourceType_ExperimentTrial,
        ResourceType_ExperimentTrialComponent,
        ResourceType_FeatureGroup,
        ResourceType_FeatureMetadata,
        ResourceType_HyperParameterTuningJob,
        ResourceType_Model,
        ResourceType_ModelCard,
        ResourceType_ModelPackage,
        ResourceType_ModelPackageGroup,
        ResourceType_Pipeline,
        ResourceType_PipelineExecution,
        ResourceType_Project,
        ResourceType_TrainingJob
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_Endpoint :: ResourceType
pattern ResourceType_Endpoint = ResourceType' "Endpoint"

pattern ResourceType_Experiment :: ResourceType
pattern ResourceType_Experiment = ResourceType' "Experiment"

pattern ResourceType_ExperimentTrial :: ResourceType
pattern ResourceType_ExperimentTrial = ResourceType' "ExperimentTrial"

pattern ResourceType_ExperimentTrialComponent :: ResourceType
pattern ResourceType_ExperimentTrialComponent = ResourceType' "ExperimentTrialComponent"

pattern ResourceType_FeatureGroup :: ResourceType
pattern ResourceType_FeatureGroup = ResourceType' "FeatureGroup"

pattern ResourceType_FeatureMetadata :: ResourceType
pattern ResourceType_FeatureMetadata = ResourceType' "FeatureMetadata"

pattern ResourceType_HyperParameterTuningJob :: ResourceType
pattern ResourceType_HyperParameterTuningJob = ResourceType' "HyperParameterTuningJob"

pattern ResourceType_Model :: ResourceType
pattern ResourceType_Model = ResourceType' "Model"

pattern ResourceType_ModelCard :: ResourceType
pattern ResourceType_ModelCard = ResourceType' "ModelCard"

pattern ResourceType_ModelPackage :: ResourceType
pattern ResourceType_ModelPackage = ResourceType' "ModelPackage"

pattern ResourceType_ModelPackageGroup :: ResourceType
pattern ResourceType_ModelPackageGroup = ResourceType' "ModelPackageGroup"

pattern ResourceType_Pipeline :: ResourceType
pattern ResourceType_Pipeline = ResourceType' "Pipeline"

pattern ResourceType_PipelineExecution :: ResourceType
pattern ResourceType_PipelineExecution = ResourceType' "PipelineExecution"

pattern ResourceType_Project :: ResourceType
pattern ResourceType_Project = ResourceType' "Project"

pattern ResourceType_TrainingJob :: ResourceType
pattern ResourceType_TrainingJob = ResourceType' "TrainingJob"

{-# COMPLETE
  ResourceType_Endpoint,
  ResourceType_Experiment,
  ResourceType_ExperimentTrial,
  ResourceType_ExperimentTrialComponent,
  ResourceType_FeatureGroup,
  ResourceType_FeatureMetadata,
  ResourceType_HyperParameterTuningJob,
  ResourceType_Model,
  ResourceType_ModelCard,
  ResourceType_ModelPackage,
  ResourceType_ModelPackageGroup,
  ResourceType_Pipeline,
  ResourceType_PipelineExecution,
  ResourceType_Project,
  ResourceType_TrainingJob,
  ResourceType'
  #-}
