{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidateStep where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CandidateStepType

-- | Information about the steps for a Candidate, and what step it is working
-- on.
--
-- /See:/ 'newAutoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { -- | Whether the Candidate is at the transform, training, or processing step.
    candidateStepType :: CandidateStepType,
    -- | The ARN for the Candidate\'s step.
    candidateStepArn :: Prelude.Text,
    -- | The name for the Candidate\'s step.
    candidateStepName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoMLCandidateStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateStepType', 'autoMLCandidateStep_candidateStepType' - Whether the Candidate is at the transform, training, or processing step.
--
-- 'candidateStepArn', 'autoMLCandidateStep_candidateStepArn' - The ARN for the Candidate\'s step.
--
-- 'candidateStepName', 'autoMLCandidateStep_candidateStepName' - The name for the Candidate\'s step.
newAutoMLCandidateStep ::
  -- | 'candidateStepType'
  CandidateStepType ->
  -- | 'candidateStepArn'
  Prelude.Text ->
  -- | 'candidateStepName'
  Prelude.Text ->
  AutoMLCandidateStep
newAutoMLCandidateStep
  pCandidateStepType_
  pCandidateStepArn_
  pCandidateStepName_ =
    AutoMLCandidateStep'
      { candidateStepType =
          pCandidateStepType_,
        candidateStepArn = pCandidateStepArn_,
        candidateStepName = pCandidateStepName_
      }

-- | Whether the Candidate is at the transform, training, or processing step.
autoMLCandidateStep_candidateStepType :: Lens.Lens' AutoMLCandidateStep CandidateStepType
autoMLCandidateStep_candidateStepType = Lens.lens (\AutoMLCandidateStep' {candidateStepType} -> candidateStepType) (\s@AutoMLCandidateStep' {} a -> s {candidateStepType = a} :: AutoMLCandidateStep)

-- | The ARN for the Candidate\'s step.
autoMLCandidateStep_candidateStepArn :: Lens.Lens' AutoMLCandidateStep Prelude.Text
autoMLCandidateStep_candidateStepArn = Lens.lens (\AutoMLCandidateStep' {candidateStepArn} -> candidateStepArn) (\s@AutoMLCandidateStep' {} a -> s {candidateStepArn = a} :: AutoMLCandidateStep)

-- | The name for the Candidate\'s step.
autoMLCandidateStep_candidateStepName :: Lens.Lens' AutoMLCandidateStep Prelude.Text
autoMLCandidateStep_candidateStepName = Lens.lens (\AutoMLCandidateStep' {candidateStepName} -> candidateStepName) (\s@AutoMLCandidateStep' {} a -> s {candidateStepName = a} :: AutoMLCandidateStep)

instance Prelude.FromJSON AutoMLCandidateStep where
  parseJSON =
    Prelude.withObject
      "AutoMLCandidateStep"
      ( \x ->
          AutoMLCandidateStep'
            Prelude.<$> (x Prelude..: "CandidateStepType")
            Prelude.<*> (x Prelude..: "CandidateStepArn")
            Prelude.<*> (x Prelude..: "CandidateStepName")
      )

instance Prelude.Hashable AutoMLCandidateStep

instance Prelude.NFData AutoMLCandidateStep
