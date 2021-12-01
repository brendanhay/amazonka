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
-- Module      : Amazonka.AccessAnalyzer.Types.PolicyGeneration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.PolicyGeneration where

import Amazonka.AccessAnalyzer.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the policy generation status and properties.
--
-- /See:/ 'newPolicyGeneration' smart constructor.
data PolicyGeneration = PolicyGeneration'
  { -- | A timestamp of when the policy generation was completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
    -- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
    -- generated policies or used with @CancelPolicyGeneration@ to cancel the
    -- policy generation request.
    jobId :: Prelude.Text,
    -- | The ARN of the IAM entity (user or role) for which you are generating a
    -- policy.
    principalArn :: Prelude.Text,
    -- | A timestamp of when the policy generation started.
    startedOn :: Core.POSIX,
    -- | The status of the policy generation request.
    status :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'policyGeneration_completedOn' - A timestamp of when the policy generation was completed.
--
-- 'jobId', 'policyGeneration_jobId' - The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
--
-- 'principalArn', 'policyGeneration_principalArn' - The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
--
-- 'startedOn', 'policyGeneration_startedOn' - A timestamp of when the policy generation started.
--
-- 'status', 'policyGeneration_status' - The status of the policy generation request.
newPolicyGeneration ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'principalArn'
  Prelude.Text ->
  -- | 'startedOn'
  Prelude.UTCTime ->
  -- | 'status'
  JobStatus ->
  PolicyGeneration
newPolicyGeneration
  pJobId_
  pPrincipalArn_
  pStartedOn_
  pStatus_ =
    PolicyGeneration'
      { completedOn = Prelude.Nothing,
        jobId = pJobId_,
        principalArn = pPrincipalArn_,
        startedOn = Core._Time Lens.# pStartedOn_,
        status = pStatus_
      }

-- | A timestamp of when the policy generation was completed.
policyGeneration_completedOn :: Lens.Lens' PolicyGeneration (Prelude.Maybe Prelude.UTCTime)
policyGeneration_completedOn = Lens.lens (\PolicyGeneration' {completedOn} -> completedOn) (\s@PolicyGeneration' {} a -> s {completedOn = a} :: PolicyGeneration) Prelude.. Lens.mapping Core._Time

-- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
policyGeneration_jobId :: Lens.Lens' PolicyGeneration Prelude.Text
policyGeneration_jobId = Lens.lens (\PolicyGeneration' {jobId} -> jobId) (\s@PolicyGeneration' {} a -> s {jobId = a} :: PolicyGeneration)

-- | The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
policyGeneration_principalArn :: Lens.Lens' PolicyGeneration Prelude.Text
policyGeneration_principalArn = Lens.lens (\PolicyGeneration' {principalArn} -> principalArn) (\s@PolicyGeneration' {} a -> s {principalArn = a} :: PolicyGeneration)

-- | A timestamp of when the policy generation started.
policyGeneration_startedOn :: Lens.Lens' PolicyGeneration Prelude.UTCTime
policyGeneration_startedOn = Lens.lens (\PolicyGeneration' {startedOn} -> startedOn) (\s@PolicyGeneration' {} a -> s {startedOn = a} :: PolicyGeneration) Prelude.. Core._Time

-- | The status of the policy generation request.
policyGeneration_status :: Lens.Lens' PolicyGeneration JobStatus
policyGeneration_status = Lens.lens (\PolicyGeneration' {status} -> status) (\s@PolicyGeneration' {} a -> s {status = a} :: PolicyGeneration)

instance Core.FromJSON PolicyGeneration where
  parseJSON =
    Core.withObject
      "PolicyGeneration"
      ( \x ->
          PolicyGeneration'
            Prelude.<$> (x Core..:? "completedOn")
            Prelude.<*> (x Core..: "jobId")
            Prelude.<*> (x Core..: "principalArn")
            Prelude.<*> (x Core..: "startedOn")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable PolicyGeneration where
  hashWithSalt salt' PolicyGeneration' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` completedOn

instance Prelude.NFData PolicyGeneration where
  rnf PolicyGeneration' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf jobId
