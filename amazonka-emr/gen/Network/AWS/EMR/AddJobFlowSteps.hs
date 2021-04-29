{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EMR.AddJobFlowSteps
  ( -- * Creating a Request
    AddJobFlowSteps (..),
    newAddJobFlowSteps,

    -- * Request Lenses
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input argument to the AddJobFlowSteps operation.
--
-- /See:/ 'newAddJobFlowSteps' smart constructor.
data AddJobFlowSteps = AddJobFlowSteps'
  { -- | A string that uniquely identifies the job flow. This identifier is
    -- returned by RunJobFlow and can also be obtained from ListClusters.
    jobFlowId :: Prelude.Text,
    -- | A list of StepConfig to be executed by the job flow.
    steps :: [StepConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddJobFlowSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { jobFlowId = pJobFlowId_,
      steps = Prelude.mempty
    }

-- | A string that uniquely identifies the job flow. This identifier is
-- returned by RunJobFlow and can also be obtained from ListClusters.
addJobFlowSteps_jobFlowId :: Lens.Lens' AddJobFlowSteps Prelude.Text
addJobFlowSteps_jobFlowId = Lens.lens (\AddJobFlowSteps' {jobFlowId} -> jobFlowId) (\s@AddJobFlowSteps' {} a -> s {jobFlowId = a} :: AddJobFlowSteps)

-- | A list of StepConfig to be executed by the job flow.
addJobFlowSteps_steps :: Lens.Lens' AddJobFlowSteps [StepConfig]
addJobFlowSteps_steps = Lens.lens (\AddJobFlowSteps' {steps} -> steps) (\s@AddJobFlowSteps' {} a -> s {steps = a} :: AddJobFlowSteps) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddJobFlowSteps where
  type Rs AddJobFlowSteps = AddJobFlowStepsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddJobFlowStepsResponse'
            Prelude.<$> (x Prelude..?> "StepIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddJobFlowSteps

instance Prelude.NFData AddJobFlowSteps

instance Prelude.ToHeaders AddJobFlowSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.AddJobFlowSteps" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddJobFlowSteps where
  toJSON AddJobFlowSteps' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobFlowId" Prelude..= jobFlowId),
            Prelude.Just ("Steps" Prelude..= steps)
          ]
      )

instance Prelude.ToPath AddJobFlowSteps where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddJobFlowSteps where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
addJobFlowStepsResponse_stepIds = Lens.lens (\AddJobFlowStepsResponse' {stepIds} -> stepIds) (\s@AddJobFlowStepsResponse' {} a -> s {stepIds = a} :: AddJobFlowStepsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
addJobFlowStepsResponse_httpStatus :: Lens.Lens' AddJobFlowStepsResponse Prelude.Int
addJobFlowStepsResponse_httpStatus = Lens.lens (\AddJobFlowStepsResponse' {httpStatus} -> httpStatus) (\s@AddJobFlowStepsResponse' {} a -> s {httpStatus = a} :: AddJobFlowStepsResponse)

instance Prelude.NFData AddJobFlowStepsResponse
