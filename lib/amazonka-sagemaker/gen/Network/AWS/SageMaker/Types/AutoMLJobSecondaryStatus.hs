{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus where

import Network.AWS.Prelude

data AutoMLJobSecondaryStatus
  = AMLJSSAnalyzingData
  | AMLJSSCandidateDefinitionsGenerated
  | AMLJSSFailed
  | AMLJSSFeatureEngineering
  | AMLJSSMaxAutoMLJobRuntimeReached
  | AMLJSSMaxCandidatesReached
  | AMLJSSModelTuning
  | AMLJSSStarting
  | AMLJSSStopped
  | AMLJSSStopping
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText AutoMLJobSecondaryStatus where
  parser =
    takeLowerText >>= \case
      "analyzingdata" -> pure AMLJSSAnalyzingData
      "candidatedefinitionsgenerated" -> pure AMLJSSCandidateDefinitionsGenerated
      "failed" -> pure AMLJSSFailed
      "featureengineering" -> pure AMLJSSFeatureEngineering
      "maxautomljobruntimereached" -> pure AMLJSSMaxAutoMLJobRuntimeReached
      "maxcandidatesreached" -> pure AMLJSSMaxCandidatesReached
      "modeltuning" -> pure AMLJSSModelTuning
      "starting" -> pure AMLJSSStarting
      "stopped" -> pure AMLJSSStopped
      "stopping" -> pure AMLJSSStopping
      e ->
        fromTextError $
          "Failure parsing AutoMLJobSecondaryStatus from value: '" <> e
            <> "'. Accepted values: analyzingdata, candidatedefinitionsgenerated, failed, featureengineering, maxautomljobruntimereached, maxcandidatesreached, modeltuning, starting, stopped, stopping"

instance ToText AutoMLJobSecondaryStatus where
  toText = \case
    AMLJSSAnalyzingData -> "AnalyzingData"
    AMLJSSCandidateDefinitionsGenerated -> "CandidateDefinitionsGenerated"
    AMLJSSFailed -> "Failed"
    AMLJSSFeatureEngineering -> "FeatureEngineering"
    AMLJSSMaxAutoMLJobRuntimeReached -> "MaxAutoMLJobRuntimeReached"
    AMLJSSMaxCandidatesReached -> "MaxCandidatesReached"
    AMLJSSModelTuning -> "ModelTuning"
    AMLJSSStarting -> "Starting"
    AMLJSSStopped -> "Stopped"
    AMLJSSStopping -> "Stopping"

instance Hashable AutoMLJobSecondaryStatus

instance NFData AutoMLJobSecondaryStatus

instance ToByteString AutoMLJobSecondaryStatus

instance ToQuery AutoMLJobSecondaryStatus

instance ToHeader AutoMLJobSecondaryStatus

instance FromJSON AutoMLJobSecondaryStatus where
  parseJSON = parseJSONText "AutoMLJobSecondaryStatus"
