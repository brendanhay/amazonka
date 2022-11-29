{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AddJobFlowSteps adds new steps to a running cluster. A maximum of 256
-- steps are allowed in each job flow.
--
-- If your cluster is long-running (such as a Hive data warehouse) or
-- complex, you may require more than 256 steps to process your data. You
-- can bypass the 256-step limitation in various ways, including using SSH
-- to connect to the master node and submitting queries directly to the
-- software running on the master node, such as Hive and Hadoop. For more
-- information on how to do this, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster>
-- in the /Amazon EMR Management Guide/.
--
-- A step specifies the location of a JAR file stored either on the master
-- node of the cluster or in Amazon S3. Each step is performed by the main
-- function of the main class of the JAR file. The main class can be
-- specified either in the manifest of the JAR or by using the MainFunction
-- parameter of the step.
--
-- Amazon EMR executes each step in the order listed. For a step to be
-- considered complete, the main function must exit with a zero exit code
-- and all Hadoop jobs started while the step was running must have
-- completed and run successfully.
--
-- You can only add steps to a cluster that is in one of the following
-- states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.
--
-- The string values passed into @HadoopJarStep@ object cannot exceed a
-- total of 10240 characters.
module Amazonka.EMR.AddJobFlowSteps
  ( -- * Creating a Request
    AddJobFlowSteps (..),
    newAddJobFlowSteps,

    -- * Request Lenses
    addJobFlowSteps_executionRoleArn,
    addJobFlowSteps_jobFlowId,
    addJobFlowSteps_steps,

    -- * Destructuring the Response
    AddJobFlowStepsResponse (..),
    newAddJobFlowStepsResponse,

    -- * Response Lenses
    addJobFlowStepsResponse_stepIds,
    addJobFlowStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input argument to the AddJobFlowSteps operation.
--
-- /See:/ 'newAddJobFlowSteps' smart constructor.
data AddJobFlowSteps = AddJobFlowSteps'
  { -- | The Amazon Resource Name (ARN) of the runtime role for a step on the
    -- cluster. The runtime role can be a cross-account IAM role. The runtime
    -- role ARN is a combination of account ID, role name, and role type using
    -- the following format: @arn:partition:service:region:account:resource@.
    --
    -- For example, @arn:aws:iam::1234567890:role\/ReadOnly@ is a correctly
    -- formatted runtime role ARN.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A string that uniquely identifies the job flow. This identifier is
    -- returned by RunJobFlow and can also be obtained from ListClusters.
    jobFlowId :: Prelude.Text,
    -- | A list of StepConfig to be executed by the job flow.
    steps :: [StepConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddJobFlowSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionRoleArn', 'addJobFlowSteps_executionRoleArn' - The Amazon Resource Name (ARN) of the runtime role for a step on the
-- cluster. The runtime role can be a cross-account IAM role. The runtime
-- role ARN is a combination of account ID, role name, and role type using
-- the following format: @arn:partition:service:region:account:resource@.
--
-- For example, @arn:aws:iam::1234567890:role\/ReadOnly@ is a correctly
-- formatted runtime role ARN.
--
-- 'jobFlowId', 'addJobFlowSteps_jobFlowId' - A string that uniquely identifies the job flow. This identifier is
-- returned by RunJobFlow and can also be obtained from ListClusters.
--
-- 'steps', 'addJobFlowSteps_steps' - A list of StepConfig to be executed by the job flow.
newAddJobFlowSteps ::
  -- | 'jobFlowId'
  Prelude.Text ->
  AddJobFlowSteps
newAddJobFlowSteps pJobFlowId_ =
  AddJobFlowSteps'
    { executionRoleArn =
        Prelude.Nothing,
      jobFlowId = pJobFlowId_,
      steps = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the runtime role for a step on the
-- cluster. The runtime role can be a cross-account IAM role. The runtime
-- role ARN is a combination of account ID, role name, and role type using
-- the following format: @arn:partition:service:region:account:resource@.
--
-- For example, @arn:aws:iam::1234567890:role\/ReadOnly@ is a correctly
-- formatted runtime role ARN.
addJobFlowSteps_executionRoleArn :: Lens.Lens' AddJobFlowSteps (Prelude.Maybe Prelude.Text)
addJobFlowSteps_executionRoleArn = Lens.lens (\AddJobFlowSteps' {executionRoleArn} -> executionRoleArn) (\s@AddJobFlowSteps' {} a -> s {executionRoleArn = a} :: AddJobFlowSteps)

-- | A string that uniquely identifies the job flow. This identifier is
-- returned by RunJobFlow and can also be obtained from ListClusters.
addJobFlowSteps_jobFlowId :: Lens.Lens' AddJobFlowSteps Prelude.Text
addJobFlowSteps_jobFlowId = Lens.lens (\AddJobFlowSteps' {jobFlowId} -> jobFlowId) (\s@AddJobFlowSteps' {} a -> s {jobFlowId = a} :: AddJobFlowSteps)

-- | A list of StepConfig to be executed by the job flow.
addJobFlowSteps_steps :: Lens.Lens' AddJobFlowSteps [StepConfig]
addJobFlowSteps_steps = Lens.lens (\AddJobFlowSteps' {steps} -> steps) (\s@AddJobFlowSteps' {} a -> s {steps = a} :: AddJobFlowSteps) Prelude.. Lens.coerced

instance Core.AWSRequest AddJobFlowSteps where
  type
    AWSResponse AddJobFlowSteps =
      AddJobFlowStepsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddJobFlowStepsResponse'
            Prelude.<$> (x Core..?> "StepIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddJobFlowSteps where
  hashWithSalt _salt AddJobFlowSteps' {..} =
    _salt `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` jobFlowId
      `Prelude.hashWithSalt` steps

instance Prelude.NFData AddJobFlowSteps where
  rnf AddJobFlowSteps' {..} =
    Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf jobFlowId
      `Prelude.seq` Prelude.rnf steps

instance Core.ToHeaders AddJobFlowSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.AddJobFlowSteps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddJobFlowSteps where
  toJSON AddJobFlowSteps' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExecutionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            Prelude.Just ("JobFlowId" Core..= jobFlowId),
            Prelude.Just ("Steps" Core..= steps)
          ]
      )

instance Core.ToPath AddJobFlowSteps where
  toPath = Prelude.const "/"

instance Core.ToQuery AddJobFlowSteps where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the AddJobFlowSteps operation.
--
-- /See:/ 'newAddJobFlowStepsResponse' smart constructor.
data AddJobFlowStepsResponse = AddJobFlowStepsResponse'
  { -- | The identifiers of the list of steps added to the job flow.
    stepIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddJobFlowStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepIds', 'addJobFlowStepsResponse_stepIds' - The identifiers of the list of steps added to the job flow.
--
-- 'httpStatus', 'addJobFlowStepsResponse_httpStatus' - The response's http status code.
newAddJobFlowStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddJobFlowStepsResponse
newAddJobFlowStepsResponse pHttpStatus_ =
  AddJobFlowStepsResponse'
    { stepIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifiers of the list of steps added to the job flow.
addJobFlowStepsResponse_stepIds :: Lens.Lens' AddJobFlowStepsResponse (Prelude.Maybe [Prelude.Text])
addJobFlowStepsResponse_stepIds = Lens.lens (\AddJobFlowStepsResponse' {stepIds} -> stepIds) (\s@AddJobFlowStepsResponse' {} a -> s {stepIds = a} :: AddJobFlowStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addJobFlowStepsResponse_httpStatus :: Lens.Lens' AddJobFlowStepsResponse Prelude.Int
addJobFlowStepsResponse_httpStatus = Lens.lens (\AddJobFlowStepsResponse' {httpStatus} -> httpStatus) (\s@AddJobFlowStepsResponse' {} a -> s {httpStatus = a} :: AddJobFlowStepsResponse)

instance Prelude.NFData AddJobFlowStepsResponse where
  rnf AddJobFlowStepsResponse' {..} =
    Prelude.rnf stepIds
      `Prelude.seq` Prelude.rnf httpStatus
