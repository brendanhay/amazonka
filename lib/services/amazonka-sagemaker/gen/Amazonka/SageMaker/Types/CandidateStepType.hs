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
-- Module      : Amazonka.SageMaker.Types.CandidateStepType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateStepType
  ( CandidateStepType
      ( ..,
        CandidateStepType_AWS__SageMaker__ProcessingJob,
        CandidateStepType_AWS__SageMaker__TrainingJob,
        CandidateStepType_AWS__SageMaker__TransformJob
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CandidateStepType = CandidateStepType'
  { fromCandidateStepType ::
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
