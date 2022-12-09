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
-- Module      : Amazonka.Batch.UpdateComputeEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Batch compute environment.
module Amazonka.Batch.UpdateComputeEnvironment
  ( -- * Creating a Request
    UpdateComputeEnvironment (..),
    newUpdateComputeEnvironment,

    -- * Request Lenses
    updateComputeEnvironment_computeResources,
    updateComputeEnvironment_serviceRole,
    updateComputeEnvironment_state,
    updateComputeEnvironment_unmanagedvCpus,
    updateComputeEnvironment_updatePolicy,
    updateComputeEnvironment_computeEnvironment,

    -- * Destructuring the Response
    UpdateComputeEnvironmentResponse (..),
    newUpdateComputeEnvironmentResponse,

    -- * Response Lenses
    updateComputeEnvironmentResponse_computeEnvironmentArn,
    updateComputeEnvironmentResponse_computeEnvironmentName,
    updateComputeEnvironmentResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @UpdateComputeEnvironment@.
--
-- /See:/ 'newUpdateComputeEnvironment' smart constructor.
data UpdateComputeEnvironment = UpdateComputeEnvironment'
  { -- | Details of the compute resources managed by the compute environment.
    -- Required for a managed compute environment. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /Batch User Guide/.
    computeResources :: Prelude.Maybe ComputeResourceUpdate,
    -- | The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
    -- make calls to other Amazon Web Services services on your behalf. For
    -- more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
    -- in the /Batch User Guide/.
    --
    -- If the compute environment has a service-linked role, it can\'t be
    -- changed to use a regular IAM role. Likewise, if the compute environment
    -- has a regular IAM role, it can\'t be changed to use a service-linked
    -- role. To update the parameters for the compute environment that require
    -- an infrastructure update to change, the __AWSServiceRoleForBatch__
    -- service-linked role must be used. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    --
    -- If your specified role has a path other than @\/@, then you must either
    -- specify the full role ARN (recommended) or prefix the role name with the
    -- path.
    --
    -- Depending on how you created your Batch service role, its ARN might
    -- contain the @service-role@ path prefix. When you only specify the name
    -- of the service role, Batch assumes that your ARN doesn\'t use the
    -- @service-role@ path prefix. Because of this, we recommend that you
    -- specify the full ARN of your service role when you create compute
    -- environments.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The state of the compute environment. Compute environments in the
    -- @ENABLED@ state can accept jobs from a queue and scale in or out
    -- automatically based on the workload demand of its associated queues.
    --
    -- If the state is @ENABLED@, then the Batch scheduler can attempt to place
    -- jobs from an associated job queue on the compute resources within the
    -- environment. If the compute environment is managed, then it can scale
    -- its instances out or in automatically, based on the job queue demand.
    --
    -- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
    -- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
    -- state continue to progress normally. Managed compute environments in the
    -- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
    -- value after instances become idle.
    state :: Prelude.Maybe CEState,
    -- | The maximum number of vCPUs expected to be used for an unmanaged compute
    -- environment. Don\'t specify this parameter for a managed compute
    -- environment. This parameter is only used for fair share scheduling to
    -- reserve vCPU capacity for new share identifiers. If this parameter
    -- isn\'t provided for a fair share job queue, no vCPU capacity is
    -- reserved.
    unmanagedvCpus :: Prelude.Maybe Prelude.Int,
    -- | Specifies the updated infrastructure update policy for the compute
    -- environment. For more information about infrastructure updates, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    updatePolicy :: Prelude.Maybe UpdatePolicy,
    -- | The name or full Amazon Resource Name (ARN) of the compute environment
    -- to update.
    computeEnvironment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComputeEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeResources', 'updateComputeEnvironment_computeResources' - Details of the compute resources managed by the compute environment.
-- Required for a managed compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
--
-- 'serviceRole', 'updateComputeEnvironment_serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
-- make calls to other Amazon Web Services services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- If the compute environment has a service-linked role, it can\'t be
-- changed to use a regular IAM role. Likewise, if the compute environment
-- has a regular IAM role, it can\'t be changed to use a service-linked
-- role. To update the parameters for the compute environment that require
-- an infrastructure update to change, the __AWSServiceRoleForBatch__
-- service-linked role must be used. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (recommended) or prefix the role name with the
-- path.
--
-- Depending on how you created your Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
--
-- 'state', 'updateComputeEnvironment_state' - The state of the compute environment. Compute environments in the
-- @ENABLED@ state can accept jobs from a queue and scale in or out
-- automatically based on the workload demand of its associated queues.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically, based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
--
-- 'unmanagedvCpus', 'updateComputeEnvironment_unmanagedvCpus' - The maximum number of vCPUs expected to be used for an unmanaged compute
-- environment. Don\'t specify this parameter for a managed compute
-- environment. This parameter is only used for fair share scheduling to
-- reserve vCPU capacity for new share identifiers. If this parameter
-- isn\'t provided for a fair share job queue, no vCPU capacity is
-- reserved.
--
-- 'updatePolicy', 'updateComputeEnvironment_updatePolicy' - Specifies the updated infrastructure update policy for the compute
-- environment. For more information about infrastructure updates, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- 'computeEnvironment', 'updateComputeEnvironment_computeEnvironment' - The name or full Amazon Resource Name (ARN) of the compute environment
-- to update.
newUpdateComputeEnvironment ::
  -- | 'computeEnvironment'
  Prelude.Text ->
  UpdateComputeEnvironment
newUpdateComputeEnvironment pComputeEnvironment_ =
  UpdateComputeEnvironment'
    { computeResources =
        Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      state = Prelude.Nothing,
      unmanagedvCpus = Prelude.Nothing,
      updatePolicy = Prelude.Nothing,
      computeEnvironment = pComputeEnvironment_
    }

-- | Details of the compute resources managed by the compute environment.
-- Required for a managed compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
updateComputeEnvironment_computeResources :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe ComputeResourceUpdate)
updateComputeEnvironment_computeResources = Lens.lens (\UpdateComputeEnvironment' {computeResources} -> computeResources) (\s@UpdateComputeEnvironment' {} a -> s {computeResources = a} :: UpdateComputeEnvironment)

-- | The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
-- make calls to other Amazon Web Services services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- If the compute environment has a service-linked role, it can\'t be
-- changed to use a regular IAM role. Likewise, if the compute environment
-- has a regular IAM role, it can\'t be changed to use a service-linked
-- role. To update the parameters for the compute environment that require
-- an infrastructure update to change, the __AWSServiceRoleForBatch__
-- service-linked role must be used. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (recommended) or prefix the role name with the
-- path.
--
-- Depending on how you created your Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
updateComputeEnvironment_serviceRole :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe Prelude.Text)
updateComputeEnvironment_serviceRole = Lens.lens (\UpdateComputeEnvironment' {serviceRole} -> serviceRole) (\s@UpdateComputeEnvironment' {} a -> s {serviceRole = a} :: UpdateComputeEnvironment)

-- | The state of the compute environment. Compute environments in the
-- @ENABLED@ state can accept jobs from a queue and scale in or out
-- automatically based on the workload demand of its associated queues.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically, based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
updateComputeEnvironment_state :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe CEState)
updateComputeEnvironment_state = Lens.lens (\UpdateComputeEnvironment' {state} -> state) (\s@UpdateComputeEnvironment' {} a -> s {state = a} :: UpdateComputeEnvironment)

-- | The maximum number of vCPUs expected to be used for an unmanaged compute
-- environment. Don\'t specify this parameter for a managed compute
-- environment. This parameter is only used for fair share scheduling to
-- reserve vCPU capacity for new share identifiers. If this parameter
-- isn\'t provided for a fair share job queue, no vCPU capacity is
-- reserved.
updateComputeEnvironment_unmanagedvCpus :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe Prelude.Int)
updateComputeEnvironment_unmanagedvCpus = Lens.lens (\UpdateComputeEnvironment' {unmanagedvCpus} -> unmanagedvCpus) (\s@UpdateComputeEnvironment' {} a -> s {unmanagedvCpus = a} :: UpdateComputeEnvironment)

-- | Specifies the updated infrastructure update policy for the compute
-- environment. For more information about infrastructure updates, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
updateComputeEnvironment_updatePolicy :: Lens.Lens' UpdateComputeEnvironment (Prelude.Maybe UpdatePolicy)
updateComputeEnvironment_updatePolicy = Lens.lens (\UpdateComputeEnvironment' {updatePolicy} -> updatePolicy) (\s@UpdateComputeEnvironment' {} a -> s {updatePolicy = a} :: UpdateComputeEnvironment)

-- | The name or full Amazon Resource Name (ARN) of the compute environment
-- to update.
updateComputeEnvironment_computeEnvironment :: Lens.Lens' UpdateComputeEnvironment Prelude.Text
updateComputeEnvironment_computeEnvironment = Lens.lens (\UpdateComputeEnvironment' {computeEnvironment} -> computeEnvironment) (\s@UpdateComputeEnvironment' {} a -> s {computeEnvironment = a} :: UpdateComputeEnvironment)

instance Core.AWSRequest UpdateComputeEnvironment where
  type
    AWSResponse UpdateComputeEnvironment =
      UpdateComputeEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComputeEnvironmentResponse'
            Prelude.<$> (x Data..?> "computeEnvironmentArn")
            Prelude.<*> (x Data..?> "computeEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateComputeEnvironment where
  hashWithSalt _salt UpdateComputeEnvironment' {..} =
    _salt `Prelude.hashWithSalt` computeResources
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` unmanagedvCpus
      `Prelude.hashWithSalt` updatePolicy
      `Prelude.hashWithSalt` computeEnvironment

instance Prelude.NFData UpdateComputeEnvironment where
  rnf UpdateComputeEnvironment' {..} =
    Prelude.rnf computeResources
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf unmanagedvCpus
      `Prelude.seq` Prelude.rnf updatePolicy
      `Prelude.seq` Prelude.rnf computeEnvironment

instance Data.ToHeaders UpdateComputeEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateComputeEnvironment where
  toJSON UpdateComputeEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computeResources" Data..=)
              Prelude.<$> computeResources,
            ("serviceRole" Data..=) Prelude.<$> serviceRole,
            ("state" Data..=) Prelude.<$> state,
            ("unmanagedvCpus" Data..=)
              Prelude.<$> unmanagedvCpus,
            ("updatePolicy" Data..=) Prelude.<$> updatePolicy,
            Prelude.Just
              ("computeEnvironment" Data..= computeEnvironment)
          ]
      )

instance Data.ToPath UpdateComputeEnvironment where
  toPath = Prelude.const "/v1/updatecomputeenvironment"

instance Data.ToQuery UpdateComputeEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComputeEnvironmentResponse' smart constructor.
data UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse'
  { -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute environment. It can be up to 128 characters
    -- long. It can contain uppercase and lowercase letters, numbers, hyphens
    -- (-), and underscores (_).
    computeEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComputeEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironmentArn', 'updateComputeEnvironmentResponse_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
--
-- 'computeEnvironmentName', 'updateComputeEnvironmentResponse_computeEnvironmentName' - The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
--
-- 'httpStatus', 'updateComputeEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateComputeEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateComputeEnvironmentResponse
newUpdateComputeEnvironmentResponse pHttpStatus_ =
  UpdateComputeEnvironmentResponse'
    { computeEnvironmentArn =
        Prelude.Nothing,
      computeEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the compute environment.
updateComputeEnvironmentResponse_computeEnvironmentArn :: Lens.Lens' UpdateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateComputeEnvironmentResponse_computeEnvironmentArn = Lens.lens (\UpdateComputeEnvironmentResponse' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@UpdateComputeEnvironmentResponse' {} a -> s {computeEnvironmentArn = a} :: UpdateComputeEnvironmentResponse)

-- | The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
updateComputeEnvironmentResponse_computeEnvironmentName :: Lens.Lens' UpdateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateComputeEnvironmentResponse_computeEnvironmentName = Lens.lens (\UpdateComputeEnvironmentResponse' {computeEnvironmentName} -> computeEnvironmentName) (\s@UpdateComputeEnvironmentResponse' {} a -> s {computeEnvironmentName = a} :: UpdateComputeEnvironmentResponse)

-- | The response's http status code.
updateComputeEnvironmentResponse_httpStatus :: Lens.Lens' UpdateComputeEnvironmentResponse Prelude.Int
updateComputeEnvironmentResponse_httpStatus = Lens.lens (\UpdateComputeEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateComputeEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateComputeEnvironmentResponse)

instance
  Prelude.NFData
    UpdateComputeEnvironmentResponse
  where
  rnf UpdateComputeEnvironmentResponse' {..} =
    Prelude.rnf computeEnvironmentArn
      `Prelude.seq` Prelude.rnf computeEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
