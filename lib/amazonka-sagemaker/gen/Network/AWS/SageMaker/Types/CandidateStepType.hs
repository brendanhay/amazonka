{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateStepType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateStepType
  ( CandidateStepType
      ( CandidateStepType',
        CandidateStepTypeAwsSageMakerTrainingJob,
        CandidateStepTypeAwsSageMakerTransformJob,
        CandidateStepTypeAwsSageMakerProcessingJob,
        fromCandidateStepType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CandidateStepType = CandidateStepType'
  { fromCandidateStepType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CandidateStepTypeAwsSageMakerTrainingJob :: CandidateStepType
pattern CandidateStepTypeAwsSageMakerTrainingJob = CandidateStepType' "AWS::SageMaker::TrainingJob"

pattern CandidateStepTypeAwsSageMakerTransformJob :: CandidateStepType
pattern CandidateStepTypeAwsSageMakerTransformJob = CandidateStepType' "AWS::SageMaker::TransformJob"

pattern CandidateStepTypeAwsSageMakerProcessingJob :: CandidateStepType
pattern CandidateStepTypeAwsSageMakerProcessingJob = CandidateStepType' "AWS::SageMaker::ProcessingJob"

{-# COMPLETE
  CandidateStepTypeAwsSageMakerTrainingJob,
  CandidateStepTypeAwsSageMakerTransformJob,
  CandidateStepTypeAwsSageMakerProcessingJob,
  CandidateStepType'
  #-}
