{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
  ( AutoMLJobSecondaryStatus
      ( ..,
        AutoMLJobSecondaryStatus_AnalyzingData,
        AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated,
        AutoMLJobSecondaryStatus_Failed,
        AutoMLJobSecondaryStatus_FeatureEngineering,
        AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached,
        AutoMLJobSecondaryStatus_MaxCandidatesReached,
        AutoMLJobSecondaryStatus_ModelTuning,
        AutoMLJobSecondaryStatus_Starting,
        AutoMLJobSecondaryStatus_Stopped,
        AutoMLJobSecondaryStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AutoMLJobSecondaryStatus = AutoMLJobSecondaryStatus'
  { fromAutoMLJobSecondaryStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AutoMLJobSecondaryStatus_AnalyzingData :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_AnalyzingData = AutoMLJobSecondaryStatus' "AnalyzingData"

pattern AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_CandidateDefinitionsGenerated = AutoMLJobSecondaryStatus' "CandidateDefinitionsGenerated"

pattern AutoMLJobSecondaryStatus_Failed :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_Failed = AutoMLJobSecondaryStatus' "Failed"

pattern AutoMLJobSecondaryStatus_FeatureEngineering :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_FeatureEngineering = AutoMLJobSecondaryStatus' "FeatureEngineering"

pattern AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached = AutoMLJobSecondaryStatus' "MaxAutoMLJobRuntimeReached"

pattern AutoMLJobSecondaryStatus_MaxCandidatesReached :: AutoMLJobSecondaryStatus
pattern AutoMLJobSecondaryStatus_MaxCandidatesReached = AutoMLJobSecondaryStatus' "MaxCandidatesReached"

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
  AutoMLJobSecondaryStatus_Failed,
  AutoMLJobSecondaryStatus_FeatureEngineering,
  AutoMLJobSecondaryStatus_MaxAutoMLJobRuntimeReached,
  AutoMLJobSecondaryStatus_MaxCandidatesReached,
  AutoMLJobSecondaryStatus_ModelTuning,
  AutoMLJobSecondaryStatus_Starting,
  AutoMLJobSecondaryStatus_Stopped,
  AutoMLJobSecondaryStatus_Stopping,
  AutoMLJobSecondaryStatus'
  #-}
