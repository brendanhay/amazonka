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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobSecondaryStatus
  ( AutoMLJobSecondaryStatus
      ( ..,
        AutoMLJobSecondaryStatus_AnalyzingData,
        AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated,
        AutoMLJobSecondaryStatus_Completed,
        AutoMLJobSecondaryStatus_DeployingModel,
        AutoMLJobSecondaryStatus_ExplainabilityError,
        AutoMLJobSecondaryStatus_Failed,
        AutoMLJobSecondaryStatus_FeatureEngineering,
        AutoMLJobSecondaryStatus_GeneratingExplainabilityReport,
        AutoMLJobSecondaryStatus_GeneratingModelInsightsReport,
        AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached,
        AutoMLJobSecondaryStatus_MaxCandidatesReached,
        AutoMLJobSecondaryStatus_ModelDeploymentError,
        AutoMLJobSecondaryStatus_ModelInsightsError,
        AutoMLJobSecondaryStatus_ModelTuning,
        AutoMLJobSecondaryStatus_Starting,
        AutoMLJobSecondaryStatus_Stopped,
        AutoMLJobSecondaryStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLJobSecondaryStatus = AutoMLJobSecondaryStatus'
  { fromAutoMLJobSecondaryStatus ::
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

pattern AutoMLJobSecondaryStatus_AnalyzingData :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_AnalyzingData = AutoMLJobSecondaryStatus' "AnalyzingData"

pattern AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated = AutoMLJobSecondaryStatus' "CandidateDefinitionsGenerated"

pattern AutoMLJobSecondaryStatus_Completed :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Completed = AutoMLJobSecondaryStatus' "Completed"

pattern AutoMLJobSecondaryStatus_DeployingModel :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_DeployingModel = AutoMLJobSecondaryStatus' "DeployingModel"

pattern AutoMLJobSecondaryStatus_ExplainabilityError :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_ExplainabilityError = AutoMLJobSecondaryStatus' "ExplainabilityError"

pattern AutoMLJobSecondaryStatus_Failed :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Failed = AutoMLJobSecondaryStatus' "Failed"

pattern AutoMLJobSecondaryStatus_FeatureEngineering :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_FeatureEngineering = AutoMLJobSecondaryStatus' "FeatureEngineering"

pattern AutoMLJobSecondaryStatus_GeneratingExplainabilityReport :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_GeneratingExplainabilityReport = AutoMLJobSecondaryStatus' "GeneratingExplainabilityReport"

pattern AutoMLJobSecondaryStatus_GeneratingModelInsightsReport :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_GeneratingModelInsightsReport = AutoMLJobSecondaryStatus' "GeneratingModelInsightsReport"

pattern AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached = AutoMLJobSecondaryStatus' "MaxAutoMLJobRuntimeReached"

pattern AutoMLJobSecondaryStatus_MaxCandidatesReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_MaxCandidatesReached = AutoMLJobSecondaryStatus' "MaxCandidatesReached"

pattern AutoMLJobSecondaryStatus_ModelDeploymentError :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_ModelDeploymentError = AutoMLJobSecondaryStatus' "ModelDeploymentError"

pattern AutoMLJobSecondaryStatus_ModelInsightsError :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_ModelInsightsError = AutoMLJobSecondaryStatus' "ModelInsightsError"

pattern AutoMLJobSecondaryStatus_ModelTuning :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_ModelTuning = AutoMLJobSecondaryStatus' "ModelTuning"

pattern AutoMLJobSecondaryStatus_Starting :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Starting = AutoMLJobSecondaryStatus' "Starting"

pattern AutoMLJobSecondaryStatus_Stopped :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Stopped = AutoMLJobSecondaryStatus' "Stopped"

pattern AutoMLJobSecondaryStatus_Stopping :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Stopping = AutoMLJobSecondaryStatus' "Stopping"

{-# COMPLETE
  AutoMLJobSecondaryStatus_AnalyzingData,
  AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated,
  AutoMLJobSecondaryStatus_Completed,
  AutoMLJobSecondaryStatus_DeployingModel,
  AutoMLJobSecondaryStatus_ExplainabilityError,
  AutoMLJobSecondaryStatus_Failed,
  AutoMLJobSecondaryStatus_FeatureEngineering,
  AutoMLJobSecondaryStatus_GeneratingExplainabilityReport,
  AutoMLJobSecondaryStatus_GeneratingModelInsightsReport,
  AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached,
  AutoMLJobSecondaryStatus_MaxCandidatesReached,
  AutoMLJobSecondaryStatus_ModelDeploymentError,
  AutoMLJobSecondaryStatus_ModelInsightsError,
  AutoMLJobSecondaryStatus_ModelTuning,
  AutoMLJobSecondaryStatus_Starting,
  AutoMLJobSecondaryStatus_Stopped,
  AutoMLJobSecondaryStatus_Stopping,
  AutoMLJobSecondaryStatus'
  #-}
