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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.PolicyGeneration where

import Amazonka.AccessAnalyzer.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the policy generation status and properties.
--
-- /See:/ 'newPolicyGeneration' smart constructor.
data PolicyGeneration = PolicyGeneration'
  { -- | A timestamp of when the policy generation was completed.
    completedOn :: Prelude.Maybe Data.ISO8601,
    -- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
    -- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
    -- generated policies or used with @CancelPolicyGeneration@ to cancel the
    -- policy generation request.
    jobId :: Prelude.Text,
    -- | The ARN of the IAM entity (user or role) for which you are generating a
    -- policy.
    principalArn :: Prelude.Text,
    -- | The status of the policy generation request.
    status :: JobStatus,
    -- | A timestamp of when the policy generation started.
    startedOn :: Data.ISO8601
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
-- 'status', 'policyGeneration_status' - The status of the policy generation request.
--
-- 'startedOn', 'policyGeneration_startedOn' - A timestamp of when the policy generation started.
newPolicyGeneration ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'principalArn'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'startedOn'
  Prelude.UTCTime ->
  PolicyGeneration
newPolicyGeneration
  pJobId_
  pPrincipalArn_
  pStatus_
  pStartedOn_ =
    PolicyGeneration'
      { completedOn = Prelude.Nothing,
        jobId = pJobId_,
        principalArn = pPrincipalArn_,
        status = pStatus_,
        startedOn = Data._Time Lens.# pStartedOn_
      }

-- | A timestamp of when the policy generation was completed.
policyGeneration_completedOn :: Lens.Lens' PolicyGeneration (Prelude.Maybe Prelude.UTCTime)
policyGeneration_completedOn = Lens.lens (\PolicyGeneration' {completedOn} -> completedOn) (\s@PolicyGeneration' {} a -> s {completedOn = a} :: PolicyGeneration) Prelude.. Lens.mapping Data._Time

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

-- | The status of the policy generation request.
policyGeneration_status :: Lens.Lens' PolicyGeneration JobStatus
policyGeneration_status = Lens.lens (\PolicyGeneration' {status} -> status) (\s@PolicyGeneration' {} a -> s {status = a} :: PolicyGeneration)

-- | A timestamp of when the policy generation started.
policyGeneration_startedOn :: Lens.Lens' PolicyGeneration Prelude.UTCTime
policyGeneration_startedOn = Lens.lens (\PolicyGeneration' {startedOn} -> startedOn) (\s@PolicyGeneration' {} a -> s {startedOn = a} :: PolicyGeneration) Prelude.. Data._Time

instance Data.FromJSON PolicyGeneration where
  parseJSON =
    Data.withObject
      "PolicyGeneration"
      ( \x ->
          PolicyGeneration'
            Prelude.<$> (x Data..:? "completedOn")
            Prelude.<*> (x Data..: "jobId")
            Prelude.<*> (x Data..: "principalArn")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "startedOn")
      )

instance Prelude.Hashable PolicyGeneration where
  hashWithSalt _salt PolicyGeneration' {..} =
    _salt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedOn

instance Prelude.NFData PolicyGeneration where
  rnf PolicyGeneration' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedOn
