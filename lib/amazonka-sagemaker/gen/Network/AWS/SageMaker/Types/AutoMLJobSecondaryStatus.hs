{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
  ( AutoMLJobSecondaryStatus
      ( AutoMLJobSecondaryStatus',
        AMLJSSStarting,
        AMLJSSAnalyzingData,
        AMLJSSFeatureEngineering,
        AMLJSSModelTuning,
        AMLJSSMaxCandidatesReached,
        AMLJSSFailed,
        AMLJSSStopped,
        AMLJSSMaxAutoMLJobRuntimeReached,
        AMLJSSStopping,
        AMLJSSCandidateDefinitionsGenerated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoMLJobSecondaryStatus = AutoMLJobSecondaryStatus' Lude.Text
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

pattern AMLJSSStarting :: AutoMLJobSecondaryStatus
pattern AMLJSSStarting = AutoMLJobSecondaryStatus' "Starting"

pattern AMLJSSAnalyzingData :: AutoMLJobSecondaryStatus
pattern AMLJSSAnalyzingData = AutoMLJobSecondaryStatus' "AnalyzingData"

pattern AMLJSSFeatureEngineering :: AutoMLJobSecondaryStatus
pattern AMLJSSFeatureEngineering = AutoMLJobSecondaryStatus' "FeatureEngineering"

pattern AMLJSSModelTuning :: AutoMLJobSecondaryStatus
pattern AMLJSSModelTuning = AutoMLJobSecondaryStatus' "ModelTuning"

pattern AMLJSSMaxCandidatesReached :: AutoMLJobSecondaryStatus
pattern AMLJSSMaxCandidatesReached = AutoMLJobSecondaryStatus' "MaxCandidatesReached"

pattern AMLJSSFailed :: AutoMLJobSecondaryStatus
pattern AMLJSSFailed = AutoMLJobSecondaryStatus' "Failed"

pattern AMLJSSStopped :: AutoMLJobSecondaryStatus
pattern AMLJSSStopped = AutoMLJobSecondaryStatus' "Stopped"

pattern AMLJSSMaxAutoMLJobRuntimeReached :: AutoMLJobSecondaryStatus
pattern AMLJSSMaxAutoMLJobRuntimeReached = AutoMLJobSecondaryStatus' "MaxAutoMLJobRuntimeReached"

pattern AMLJSSStopping :: AutoMLJobSecondaryStatus
pattern AMLJSSStopping = AutoMLJobSecondaryStatus' "Stopping"

pattern AMLJSSCandidateDefinitionsGenerated :: AutoMLJobSecondaryStatus
pattern AMLJSSCandidateDefinitionsGenerated = AutoMLJobSecondaryStatus' "CandidateDefinitionsGenerated"

{-# COMPLETE
  AMLJSSStarting,
  AMLJSSAnalyzingData,
  AMLJSSFeatureEngineering,
  AMLJSSModelTuning,
  AMLJSSMaxCandidatesReached,
  AMLJSSFailed,
  AMLJSSStopped,
  AMLJSSMaxAutoMLJobRuntimeReached,
  AMLJSSStopping,
  AMLJSSCandidateDefinitionsGenerated,
  AutoMLJobSecondaryStatus'
  #-}
