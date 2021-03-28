{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
  ( AutoMLJobSecondaryStatus
    ( AutoMLJobSecondaryStatus'
    , AutoMLJobSecondaryStatusStarting
    , AutoMLJobSecondaryStatusAnalyzingData
    , AutoMLJobSecondaryStatusFeatureEngineering
    , AutoMLJobSecondaryStatusModelTuning
    , AutoMLJobSecondaryStatusMaxCandidatesReached
    , AutoMLJobSecondaryStatusFailed
    , AutoMLJobSecondaryStatusStopped
    , AutoMLJobSecondaryStatusMaxAutoMLJobRuntimeReached
    , AutoMLJobSecondaryStatusStopping
    , AutoMLJobSecondaryStatusCandidateDefinitionsGenerated
    , fromAutoMLJobSecondaryStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AutoMLJobSecondaryStatus = AutoMLJobSecondaryStatus'{fromAutoMLJobSecondaryStatus
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern AutoMLJobSecondaryStatusStarting :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusStarting = AutoMLJobSecondaryStatus' "Starting"

pattern AutoMLJobSecondaryStatusAnalyzingData :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusAnalyzingData = AutoMLJobSecondaryStatus' "AnalyzingData"

pattern AutoMLJobSecondaryStatusFeatureEngineering :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusFeatureEngineering = AutoMLJobSecondaryStatus' "FeatureEngineering"

pattern AutoMLJobSecondaryStatusModelTuning :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusModelTuning = AutoMLJobSecondaryStatus' "ModelTuning"

pattern AutoMLJobSecondaryStatusMaxCandidatesReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusMaxCandidatesReached = AutoMLJobSecondaryStatus' "MaxCandidatesReached"

pattern AutoMLJobSecondaryStatusFailed :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusFailed = AutoMLJobSecondaryStatus' "Failed"

pattern AutoMLJobSecondaryStatusStopped :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusStopped = AutoMLJobSecondaryStatus' "Stopped"

pattern AutoMLJobSecondaryStatusMaxAutoMLJobRuntimeReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusMaxAutoMLJobRuntimeReached = AutoMLJobSecondaryStatus' "MaxAutoMLJobRuntimeReached"

pattern AutoMLJobSecondaryStatusStopping :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusStopping = AutoMLJobSecondaryStatus' "Stopping"

pattern AutoMLJobSecondaryStatusCandidateDefinitionsGenerated :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatusCandidateDefinitionsGenerated = AutoMLJobSecondaryStatus' "CandidateDefinitionsGenerated"

{-# COMPLETE 
  AutoMLJobSecondaryStatusStarting,

  AutoMLJobSecondaryStatusAnalyzingData,

  AutoMLJobSecondaryStatusFeatureEngineering,

  AutoMLJobSecondaryStatusModelTuning,

  AutoMLJobSecondaryStatusMaxCandidatesReached,

  AutoMLJobSecondaryStatusFailed,

  AutoMLJobSecondaryStatusStopped,

  AutoMLJobSecondaryStatusMaxAutoMLJobRuntimeReached,

  AutoMLJobSecondaryStatusStopping,

  AutoMLJobSecondaryStatusCandidateDefinitionsGenerated,
  AutoMLJobSecondaryStatus'
  #-}
