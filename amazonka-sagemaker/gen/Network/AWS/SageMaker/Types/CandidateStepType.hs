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
import qualified Network.AWS.Prelude as Prelude

newtype CandidateStepType = CandidateStepType'
  { fromCandidateStepType ::
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
