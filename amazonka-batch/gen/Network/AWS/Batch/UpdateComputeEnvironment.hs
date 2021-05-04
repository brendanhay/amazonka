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
-- Module      : Network.AWS.Batch.UpdateComputeEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AWS Batch compute environment.
module Network.AWS.Batch.UpdateComputeEnvironment
  ( -- * Creating a Request
    UpdateComputeEnvironment (..),
    newUpdateComputeEnvironment,

    -- * Request Lenses
    updateComputeEnvironment_serviceRole,
    updateComputeEnvironment_state,
    updateComputeEnvironment_computeResources,
    updateComputeEnvironment_computeEnvironment,

    -- * Destructuring the Response
    UpdateComputeEnvironmentResponse (..),
    newUpdateComputeEnvironmentResponse,

    -- * Response Lenses
    updateComputeEnvironmentResponse_computeEnvironmentName,
    updateComputeEnvironmentResponse_computeEnvironmentArn,
    updateComputeEnvironmentResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @UpdateComputeEnvironment@.
--
-- /See:/ 'newUpdateComputeEnvironment' smart constructor.
data UpdateComputeEnvironment = UpdateComputeEnvironment'
  { -- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS
    -- Batch to make calls to other AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
    -- in the /AWS Batch User Guide/.
    --
    -- If your specified role has a path other than @\/@, then you must either
    -- specify the full role ARN (this is recommended) or prefix the role name
    -- with the path.
    --
    -- Depending on how you created your AWS Batch service role, its ARN might
    -- contain the @service-role@ path prefix. When you only specify the name
    -- of the service role, AWS Batch assumes that your ARN does not use the
    -- @service-role@ path prefix. Because of this, we recommend that you
    -- specify the full ARN of your service role when you create compute
    -- environments.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The state of the compute environment. Compute environments in the
    -- @ENABLED@ state can accept jobs from a queue and scale in or out
    -- automatically based on the workload demand of its associated queues.
    --
    -- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
    -- place jobs from an associated job queue on the compute resources within
    -- the environment. If the compute environment is managed, then it can
    -- scale its instances out or in automatically, based on the job queue
    -- demand.
    --
    -- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
    -- attempt to place jobs within the environment. Jobs in a @STARTING@ or
    -- @RUNNING@ state continue to progress normally. Managed compute
    -- environments in the @DISABLED@ state don\'t scale out. However, they
    -- scale in to @minvCpus@ value after instances become idle.
    state :: Prelude.Maybe CEState,
    -- | Details of the compute resources managed by the compute environment.
    -- Required for a managed compute environment. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /AWS Batch User Guide/.
    computeResources :: Prelude.Maybe ComputeResourceUpdate,
    -- | The name or full Amazon Resource Name (ARN) of the compute environment
    -- to update.
    computeEnvironment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateComputeEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceRole', 'updateComputeEnvironment_serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS
-- Batch to make calls to other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
-- in the /AWS Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path.
--
-- Depending on how you created your AWS Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, AWS Batch assumes that your ARN does not use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
--
-- 'state', 'updateComputeEnvironment_state' - The state of the compute environment. Compute environments in the
-- @ENABLED@ state can accept jobs from a queue and scale in or out
-- automatically based on the workload demand of its associated queues.
--
-- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
-- place jobs from an associated job queue on the compute resources within
-- the environment. If the compute environment is managed, then it can
-- scale its instances out or in automatically, based on the job queue
-- demand.
--
-- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
-- attempt to place jobs within the environment. Jobs in a @STARTING@ or
-- @RUNNING@ state continue to progress normally. Managed compute
-- environments in the @DISABLED@ state don\'t scale out. However, they
-- scale in to @minvCpus@ value after instances become idle.
--
-- 'computeResources', 'updateComputeEnvironment_computeResources' - Details of the compute resources managed by the compute environment.
-- Required for a managed compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- 'computeEnvironment', 'updateComputeEnvironment_computeEnvironment' - The name or full Amazon Resource Name (ARN) of the compute environment
-- to update.
newUpdateComputeEnvironment ::
  -- | 'computeEnvironment'
  Prelude.Text ->
  UpdateComputeEnvironment
newUpdateComputeEnvironment pComputeEnvironment_ =
  UpdateComputeEnvironment'
    { serviceRole =
        Prelude.Nothing,
      state = Prelude.Nothing,
      computeResources = Prelude.Nothing,
      computeEnvironment = pComputeEnvironment_
    }

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS
-- Batch to make calls to other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
-- in the /AWS Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path.
--
-- Depending on how you created your AWS Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, AWS Batch assumes that your ARN does not use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
updateComputeEnvironment_serviceRole :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe Prelude.Text)
updateComputeEnvironment_serviceRole = Lens.lens (\UpdateComputeEnvironment' {serviceRole} -> serviceRole) (\s@UpdateComputeEnvironment' {} a -> s {serviceRole = a} :: UpdateComputeEnvironment)

-- | The state of the compute environment. Compute environments in the
-- @ENABLED@ state can accept jobs from a queue and scale in or out
-- automatically based on the workload demand of its associated queues.
--
-- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
-- place jobs from an associated job queue on the compute resources within
-- the environment. If the compute environment is managed, then it can
-- scale its instances out or in automatically, based on the job queue
-- demand.
--
-- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
-- attempt to place jobs within the environment. Jobs in a @STARTING@ or
-- @RUNNING@ state continue to progress normally. Managed compute
-- environments in the @DISABLED@ state don\'t scale out. However, they
-- scale in to @minvCpus@ value after instances become idle.
updateComputeEnvironment_state :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe CEState)
updateComputeEnvironment_state = Lens.lens (\UpdateComputeEnvironment' {state} -> state) (\s@UpdateComputeEnvironment' {} a -> s {state = a} :: UpdateComputeEnvironment)

-- | Details of the compute resources managed by the compute environment.
-- Required for a managed compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
updateComputeEnvironment_computeResources :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe ComputeResourceUpdate)
updateComputeEnvironment_computeResources = Lens.lens (\UpdateComputeEnvironment' {computeResources} -> computeResources) (\s@UpdateComputeEnvironment' {} a -> s {computeResources = a} :: UpdateComputeEnvironment)

-- | The name or full Amazon Resource Name (ARN) of the compute environment
-- to update.
updateComputeEnvironment_computeEnvironment :: Lens.Lens' UpdateComputeEnvironment Prelude.Text
updateComputeEnvironment_computeEnvironment = Lens.lens (\UpdateComputeEnvironment' {computeEnvironment} -> computeEnvironment) (\s@UpdateComputeEnvironment' {} a -> s {computeEnvironment = a} :: UpdateComputeEnvironment)

instance Prelude.AWSRequest UpdateComputeEnvironment where
  type
    Rs UpdateComputeEnvironment =
      UpdateComputeEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComputeEnvironmentResponse'
            Prelude.<$> (x Prelude..?> "computeEnvironmentName")
            Prelude.<*> (x Prelude..?> "computeEnvironmentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateComputeEnvironment

instance Prelude.NFData UpdateComputeEnvironment

instance Prelude.ToHeaders UpdateComputeEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateComputeEnvironment where
  toJSON UpdateComputeEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serviceRole" Prelude..=) Prelude.<$> serviceRole,
            ("state" Prelude..=) Prelude.<$> state,
            ("computeResources" Prelude..=)
              Prelude.<$> computeResources,
            Prelude.Just
              ( "computeEnvironment"
                  Prelude..= computeEnvironment
              )
          ]
      )

instance Prelude.ToPath UpdateComputeEnvironment where
  toPath = Prelude.const "/v1/updatecomputeenvironment"

instance Prelude.ToQuery UpdateComputeEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComputeEnvironmentResponse' smart constructor.
data UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse'
  { -- | The name of the compute environment. Up to 128 letters (uppercase and
    -- lowercase), numbers, hyphens, and underscores are allowed.
    computeEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateComputeEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironmentName', 'updateComputeEnvironmentResponse_computeEnvironmentName' - The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
--
-- 'computeEnvironmentArn', 'updateComputeEnvironmentResponse_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
--
-- 'httpStatus', 'updateComputeEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateComputeEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateComputeEnvironmentResponse
newUpdateComputeEnvironmentResponse pHttpStatus_ =
  UpdateComputeEnvironmentResponse'
    { computeEnvironmentName =
        Prelude.Nothing,
      computeEnvironmentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
updateComputeEnvironmentResponse_computeEnvironmentName :: Lens.Lens' UpdateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateComputeEnvironmentResponse_computeEnvironmentName = Lens.lens (\UpdateComputeEnvironmentResponse' {computeEnvironmentName} -> computeEnvironmentName) (\s@UpdateComputeEnvironmentResponse' {} a -> s {computeEnvironmentName = a} :: UpdateComputeEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the compute environment.
updateComputeEnvironmentResponse_computeEnvironmentArn :: Lens.Lens' UpdateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateComputeEnvironmentResponse_computeEnvironmentArn = Lens.lens (\UpdateComputeEnvironmentResponse' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@UpdateComputeEnvironmentResponse' {} a -> s {computeEnvironmentArn = a} :: UpdateComputeEnvironmentResponse)

-- | The response's http status code.
updateComputeEnvironmentResponse_httpStatus :: Lens.Lens' UpdateComputeEnvironmentResponse Prelude.Int
updateComputeEnvironmentResponse_httpStatus = Lens.lens (\UpdateComputeEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateComputeEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateComputeEnvironmentResponse)

instance
  Prelude.NFData
    UpdateComputeEnvironmentResponse
