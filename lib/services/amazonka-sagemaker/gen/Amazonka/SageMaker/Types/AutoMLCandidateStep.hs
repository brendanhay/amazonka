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
-- Module      : Amazonka.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLCandidateStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CandidateStepType

-- | Information about the steps for a candidate and what step it is working
-- on.
--
-- /See:/ 'newAutoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { -- | Whether the candidate is at the transform, training, or processing step.
    candidateStepType :: CandidateStepType,
    -- | The ARN for the candidate\'s step.
    candidateStepArn :: Prelude.Text,
    -- | The name for the candidate\'s step.
    candidateStepName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLCandidateStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateStepType', 'autoMLCandidateStep_candidateStepType' - Whether the candidate is at the transform, training, or processing step.
--
-- 'candidateStepArn', 'autoMLCandidateStep_candidateStepArn' - The ARN for the candidate\'s step.
--
-- 'candidateStepName', 'autoMLCandidateStep_candidateStepName' - The name for the candidate\'s step.
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

-- | Whether the candidate is at the transform, training, or processing step.
autoMLCandidateStep_candidateStepType :: Lens.Lens' AutoMLCandidateStep CandidateStepType
autoMLCandidateStep_candidateStepType = Lens.lens (\AutoMLCandidateStep' {candidateStepType} -> candidateStepType) (\s@AutoMLCandidateStep' {} a -> s {candidateStepType = a} :: AutoMLCandidateStep)

-- | The ARN for the candidate\'s step.
autoMLCandidateStep_candidateStepArn :: Lens.Lens' AutoMLCandidateStep Prelude.Text
autoMLCandidateStep_candidateStepArn = Lens.lens (\AutoMLCandidateStep' {candidateStepArn} -> candidateStepArn) (\s@AutoMLCandidateStep' {} a -> s {candidateStepArn = a} :: AutoMLCandidateStep)

-- | The name for the candidate\'s step.
autoMLCandidateStep_candidateStepName :: Lens.Lens' AutoMLCandidateStep Prelude.Text
autoMLCandidateStep_candidateStepName = Lens.lens (\AutoMLCandidateStep' {candidateStepName} -> candidateStepName) (\s@AutoMLCandidateStep' {} a -> s {candidateStepName = a} :: AutoMLCandidateStep)

instance Data.FromJSON AutoMLCandidateStep where
  parseJSON =
    Data.withObject
      "AutoMLCandidateStep"
      ( \x ->
          AutoMLCandidateStep'
            Prelude.<$> (x Data..: "CandidateStepType")
            Prelude.<*> (x Data..: "CandidateStepArn")
            Prelude.<*> (x Data..: "CandidateStepName")
      )

instance Prelude.Hashable AutoMLCandidateStep where
  hashWithSalt _salt AutoMLCandidateStep' {..} =
    _salt `Prelude.hashWithSalt` candidateStepType
      `Prelude.hashWithSalt` candidateStepArn
      `Prelude.hashWithSalt` candidateStepName

instance Prelude.NFData AutoMLCandidateStep where
  rnf AutoMLCandidateStep' {..} =
    Prelude.rnf candidateStepType
      `Prelude.seq` Prelude.rnf candidateStepArn
      `Prelude.seq` Prelude.rnf candidateStepName
