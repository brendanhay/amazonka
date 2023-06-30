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
-- Module      : Amazonka.EMR.DescribeStep
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
module Amazonka.EMR.DescribeStep
  ( -- * Creating a Request
    DescribeStep (..),
    newDescribeStep,

    -- * Request Lenses
    describeStep_clusterId,
    describeStep_stepId,

    -- * Destructuring the Response
    DescribeStepResponse (..),
    newDescribeStepResponse,

    -- * Response Lenses
    describeStepResponse_step,
    describeStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input determines which step to describe.
--
-- /See:/ 'newDescribeStep' smart constructor.
data DescribeStep = DescribeStep'
  { -- | The identifier of the cluster with steps to describe.
    clusterId :: Prelude.Text,
    -- | The identifier of the step to describe.
    stepId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'describeStep_clusterId' - The identifier of the cluster with steps to describe.
--
-- 'stepId', 'describeStep_stepId' - The identifier of the step to describe.
newDescribeStep ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'stepId'
  Prelude.Text ->
  DescribeStep
newDescribeStep pClusterId_ pStepId_ =
  DescribeStep'
    { clusterId = pClusterId_,
      stepId = pStepId_
    }

-- | The identifier of the cluster with steps to describe.
describeStep_clusterId :: Lens.Lens' DescribeStep Prelude.Text
describeStep_clusterId = Lens.lens (\DescribeStep' {clusterId} -> clusterId) (\s@DescribeStep' {} a -> s {clusterId = a} :: DescribeStep)

-- | The identifier of the step to describe.
describeStep_stepId :: Lens.Lens' DescribeStep Prelude.Text
describeStep_stepId = Lens.lens (\DescribeStep' {stepId} -> stepId) (\s@DescribeStep' {} a -> s {stepId = a} :: DescribeStep)

instance Core.AWSRequest DescribeStep where
  type AWSResponse DescribeStep = DescribeStepResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStepResponse'
            Prelude.<$> (x Data..?> "Step")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStep where
  hashWithSalt _salt DescribeStep' {..} =
    _salt
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` stepId

instance Prelude.NFData DescribeStep where
  rnf DescribeStep' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf stepId

instance Data.ToHeaders DescribeStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.DescribeStep" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStep where
  toJSON DescribeStep' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just ("StepId" Data..= stepId)
          ]
      )

instance Data.ToPath DescribeStep where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStep where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the description of the cluster step.
--
-- /See:/ 'newDescribeStepResponse' smart constructor.
data DescribeStepResponse = DescribeStepResponse'
  { -- | The step details for the requested step identifier.
    step :: Prelude.Maybe Step,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'step', 'describeStepResponse_step' - The step details for the requested step identifier.
--
-- 'httpStatus', 'describeStepResponse_httpStatus' - The response's http status code.
newDescribeStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStepResponse
newDescribeStepResponse pHttpStatus_ =
  DescribeStepResponse'
    { step = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The step details for the requested step identifier.
describeStepResponse_step :: Lens.Lens' DescribeStepResponse (Prelude.Maybe Step)
describeStepResponse_step = Lens.lens (\DescribeStepResponse' {step} -> step) (\s@DescribeStepResponse' {} a -> s {step = a} :: DescribeStepResponse)

-- | The response's http status code.
describeStepResponse_httpStatus :: Lens.Lens' DescribeStepResponse Prelude.Int
describeStepResponse_httpStatus = Lens.lens (\DescribeStepResponse' {httpStatus} -> httpStatus) (\s@DescribeStepResponse' {} a -> s {httpStatus = a} :: DescribeStepResponse)

instance Prelude.NFData DescribeStepResponse where
  rnf DescribeStepResponse' {..} =
    Prelude.rnf step
      `Prelude.seq` Prelude.rnf httpStatus
