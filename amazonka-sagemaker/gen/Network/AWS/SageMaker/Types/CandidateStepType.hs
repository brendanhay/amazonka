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
-- Module      : Network.AWS.SageMaker.Types.CandidateStepType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateStepType
  ( CandidateStepType
      ( ..,
        CandidateStepType_AWS__SageMaker__ProcessingJob,
        CandidateStepType_AWS__SageMaker__TrainingJob,
        CandidateStepType_AWS__SageMaker__TransformJob
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CandidateStepType = CandidateStepType'
  { fromCandidateStepType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern CandidateStepType_AWS__SageMaker__ProcessingJob :: CandidateStepType
pattern CandidateStepType_AWS__SageMaker__ProcessingJob = CandidateStepType' "AWS::SageMaker::ProcessingJob"

pattern CandidateStepType_AWS__SageMaker__TrainingJob :: CandidateStepType
pattern CandidateStepType_AWS__SageMaker__TrainingJob = CandidateStepType' "AWS::SageMaker::TrainingJob"

pattern CandidateStepType_AWS__SageMaker__TransformJob :: CandidateStepType
pattern CandidateStepType_AWS__SageMaker__TransformJob = CandidateStepType' "AWS::SageMaker::TransformJob"

{-# COMPLETE
  CandidateStepType_AWS__SageMaker__ProcessingJob,
  CandidateStepType_AWS__SageMaker__TrainingJob,
  CandidateStepType_AWS__SageMaker__TransformJob,
  CandidateStepType'
  #-}
