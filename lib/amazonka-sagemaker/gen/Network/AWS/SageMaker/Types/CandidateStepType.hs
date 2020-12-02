{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateStepType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateStepType where

import Network.AWS.Prelude

data CandidateStepType
  = AWSSageMakerProcessingJob
  | AWSSageMakerTrainingJob
  | AWSSageMakerTransformJob
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

instance FromText CandidateStepType where
  parser =
    takeLowerText >>= \case
      "aws::sagemaker::processingjob" -> pure AWSSageMakerProcessingJob
      "aws::sagemaker::trainingjob" -> pure AWSSageMakerTrainingJob
      "aws::sagemaker::transformjob" -> pure AWSSageMakerTransformJob
      e ->
        fromTextError $
          "Failure parsing CandidateStepType from value: '" <> e
            <> "'. Accepted values: aws::sagemaker::processingjob, aws::sagemaker::trainingjob, aws::sagemaker::transformjob"

instance ToText CandidateStepType where
  toText = \case
    AWSSageMakerProcessingJob -> "AWS::SageMaker::ProcessingJob"
    AWSSageMakerTrainingJob -> "AWS::SageMaker::TrainingJob"
    AWSSageMakerTransformJob -> "AWS::SageMaker::TransformJob"

instance Hashable CandidateStepType

instance NFData CandidateStepType

instance ToByteString CandidateStepType

instance ToQuery CandidateStepType

instance ToHeader CandidateStepType

instance FromJSON CandidateStepType where
  parseJSON = parseJSONText "CandidateStepType"
