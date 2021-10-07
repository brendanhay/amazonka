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
-- Module      : Network.AWS.SageMaker.Types.ResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_Endpoint,
        ResourceType_Experiment,
        ResourceType_ExperimentTrial,
        ResourceType_ExperimentTrialComponent,
        ResourceType_FeatureGroup,
        ResourceType_ModelPackage,
        ResourceType_ModelPackageGroup,
        ResourceType_Pipeline,
        ResourceType_PipelineExecution,
        ResourceType_Project,
        ResourceType_TrainingJob
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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
  ResourceType_ModelPackage,
  ResourceType_ModelPackageGroup,
  ResourceType_Pipeline,
  ResourceType_PipelineExecution,
  ResourceType_Project,
  ResourceType_TrainingJob,
  ResourceType'
  #-}
