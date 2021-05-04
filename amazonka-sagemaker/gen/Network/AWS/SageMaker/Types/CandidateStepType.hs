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

import qualified Network.AWS.Prelude as Prelude

newtype CandidateStepType = CandidateStepType'
  { fromCandidateStepType ::
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
