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
-- Module      : Amazonka.Batch.Types.ComputeEnvironmentDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ComputeEnvironmentDetail where

import Amazonka.Batch.Types.CEState
import Amazonka.Batch.Types.CEStatus
import Amazonka.Batch.Types.CEType
import Amazonka.Batch.Types.ComputeResource
import Amazonka.Batch.Types.EksConfiguration
import Amazonka.Batch.Types.OrchestrationType
import Amazonka.Batch.Types.UpdatePolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch compute environment.
--
-- /See:/ 'newComputeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
  { -- | The compute resources defined for the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
    -- in the /Batch User Guide/.
    computeResources :: Prelude.Maybe ComputeResource,
    -- | The orchestration type of the compute environment. The valid values are
    -- @ECS@ (default) or @EKS@.
    containerOrchestrationType :: Prelude.Maybe OrchestrationType,
    -- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster that
    -- the compute environment uses.
    ecsClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the Amazon EKS cluster that supports the Batch
    -- compute environment. Only specify this parameter if the
    -- @containerOrchestrationType@ is @EKS@.
    eksConfiguration :: Prelude.Maybe EksConfiguration,
    -- | The service role that\'s associated with the compute environment that
    -- allows Batch to make calls to Amazon Web Services API operations on your
    -- behalf. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
    -- in the /Batch User Guide/.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The state of the compute environment. The valid values are @ENABLED@ or
    -- @DISABLED@.
    --
    -- If the state is @ENABLED@, then the Batch scheduler can attempt to place
    -- jobs from an associated job queue on the compute resources within the
    -- environment. If the compute environment is managed, then it can scale
    -- its instances out or in automatically based on the job queue demand.
    --
    -- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
    -- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
    -- state continue to progress normally. Managed compute environments in the
    -- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
    -- value after instances become idle.
    state :: Prelude.Maybe CEState,
    -- | The current status of the compute environment (for example, @CREATING@
    -- or @VALID@).
    status :: Prelude.Maybe CEStatus,
    -- | A short, human-readable string to provide additional details for the
    -- current status of the compute environment.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The tags applied to the compute environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
    -- in the /Batch User Guide/.
    type' :: Prelude.Maybe CEType,
    -- | The maximum number of VCPUs expected to be used for an unmanaged compute
    -- environment.
    unmanagedvCpus :: Prelude.Maybe Prelude.Int,
    -- | Specifies the infrastructure update policy for the compute environment.
    -- For more information about infrastructure updates, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    updatePolicy :: Prelude.Maybe UpdatePolicy,
    -- | Unique identifier for the compute environment.
    uuid :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute environment. It can be up to 128 characters
    -- long. It can contain uppercase and lowercase letters, numbers, hyphens
    -- (-), and underscores (_).
    computeEnvironmentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeEnvironmentDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeResources', 'computeEnvironmentDetail_computeResources' - The compute resources defined for the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- 'containerOrchestrationType', 'computeEnvironmentDetail_containerOrchestrationType' - The orchestration type of the compute environment. The valid values are
-- @ECS@ (default) or @EKS@.
--
-- 'ecsClusterArn', 'computeEnvironmentDetail_ecsClusterArn' - The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster that
-- the compute environment uses.
--
-- 'eksConfiguration', 'computeEnvironmentDetail_eksConfiguration' - The configuration for the Amazon EKS cluster that supports the Batch
-- compute environment. Only specify this parameter if the
-- @containerOrchestrationType@ is @EKS@.
--
-- 'serviceRole', 'computeEnvironmentDetail_serviceRole' - The service role that\'s associated with the compute environment that
-- allows Batch to make calls to Amazon Web Services API operations on your
-- behalf. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- 'state', 'computeEnvironmentDetail_state' - The state of the compute environment. The valid values are @ENABLED@ or
-- @DISABLED@.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
--
-- 'status', 'computeEnvironmentDetail_status' - The current status of the compute environment (for example, @CREATING@
-- or @VALID@).
--
-- 'statusReason', 'computeEnvironmentDetail_statusReason' - A short, human-readable string to provide additional details for the
-- current status of the compute environment.
--
-- 'tags', 'computeEnvironmentDetail_tags' - The tags applied to the compute environment.
--
-- 'type'', 'computeEnvironmentDetail_type' - The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- 'unmanagedvCpus', 'computeEnvironmentDetail_unmanagedvCpus' - The maximum number of VCPUs expected to be used for an unmanaged compute
-- environment.
--
-- 'updatePolicy', 'computeEnvironmentDetail_updatePolicy' - Specifies the infrastructure update policy for the compute environment.
-- For more information about infrastructure updates, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- 'uuid', 'computeEnvironmentDetail_uuid' - Unique identifier for the compute environment.
--
-- 'computeEnvironmentName', 'computeEnvironmentDetail_computeEnvironmentName' - The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
--
-- 'computeEnvironmentArn', 'computeEnvironmentDetail_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
newComputeEnvironmentDetail ::
  -- | 'computeEnvironmentName'
  Prelude.Text ->
  -- | 'computeEnvironmentArn'
  Prelude.Text ->
  ComputeEnvironmentDetail
newComputeEnvironmentDetail
  pComputeEnvironmentName_
  pComputeEnvironmentArn_ =
    ComputeEnvironmentDetail'
      { computeResources =
          Prelude.Nothing,
        containerOrchestrationType = Prelude.Nothing,
        ecsClusterArn = Prelude.Nothing,
        eksConfiguration = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        state = Prelude.Nothing,
        status = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        type' = Prelude.Nothing,
        unmanagedvCpus = Prelude.Nothing,
        updatePolicy = Prelude.Nothing,
        uuid = Prelude.Nothing,
        computeEnvironmentName = pComputeEnvironmentName_,
        computeEnvironmentArn = pComputeEnvironmentArn_
      }

-- | The compute resources defined for the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
computeEnvironmentDetail_computeResources :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe ComputeResource)
computeEnvironmentDetail_computeResources = Lens.lens (\ComputeEnvironmentDetail' {computeResources} -> computeResources) (\s@ComputeEnvironmentDetail' {} a -> s {computeResources = a} :: ComputeEnvironmentDetail)

-- | The orchestration type of the compute environment. The valid values are
-- @ECS@ (default) or @EKS@.
computeEnvironmentDetail_containerOrchestrationType :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe OrchestrationType)
computeEnvironmentDetail_containerOrchestrationType = Lens.lens (\ComputeEnvironmentDetail' {containerOrchestrationType} -> containerOrchestrationType) (\s@ComputeEnvironmentDetail' {} a -> s {containerOrchestrationType = a} :: ComputeEnvironmentDetail)

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster that
-- the compute environment uses.
computeEnvironmentDetail_ecsClusterArn :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_ecsClusterArn = Lens.lens (\ComputeEnvironmentDetail' {ecsClusterArn} -> ecsClusterArn) (\s@ComputeEnvironmentDetail' {} a -> s {ecsClusterArn = a} :: ComputeEnvironmentDetail)

-- | The configuration for the Amazon EKS cluster that supports the Batch
-- compute environment. Only specify this parameter if the
-- @containerOrchestrationType@ is @EKS@.
computeEnvironmentDetail_eksConfiguration :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe EksConfiguration)
computeEnvironmentDetail_eksConfiguration = Lens.lens (\ComputeEnvironmentDetail' {eksConfiguration} -> eksConfiguration) (\s@ComputeEnvironmentDetail' {} a -> s {eksConfiguration = a} :: ComputeEnvironmentDetail)

-- | The service role that\'s associated with the compute environment that
-- allows Batch to make calls to Amazon Web Services API operations on your
-- behalf. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
computeEnvironmentDetail_serviceRole :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_serviceRole = Lens.lens (\ComputeEnvironmentDetail' {serviceRole} -> serviceRole) (\s@ComputeEnvironmentDetail' {} a -> s {serviceRole = a} :: ComputeEnvironmentDetail)

-- | The state of the compute environment. The valid values are @ENABLED@ or
-- @DISABLED@.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
computeEnvironmentDetail_state :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEState)
computeEnvironmentDetail_state = Lens.lens (\ComputeEnvironmentDetail' {state} -> state) (\s@ComputeEnvironmentDetail' {} a -> s {state = a} :: ComputeEnvironmentDetail)

-- | The current status of the compute environment (for example, @CREATING@
-- or @VALID@).
computeEnvironmentDetail_status :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEStatus)
computeEnvironmentDetail_status = Lens.lens (\ComputeEnvironmentDetail' {status} -> status) (\s@ComputeEnvironmentDetail' {} a -> s {status = a} :: ComputeEnvironmentDetail)

-- | A short, human-readable string to provide additional details for the
-- current status of the compute environment.
computeEnvironmentDetail_statusReason :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_statusReason = Lens.lens (\ComputeEnvironmentDetail' {statusReason} -> statusReason) (\s@ComputeEnvironmentDetail' {} a -> s {statusReason = a} :: ComputeEnvironmentDetail)

-- | The tags applied to the compute environment.
computeEnvironmentDetail_tags :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
computeEnvironmentDetail_tags = Lens.lens (\ComputeEnvironmentDetail' {tags} -> tags) (\s@ComputeEnvironmentDetail' {} a -> s {tags = a} :: ComputeEnvironmentDetail) Prelude.. Lens.mapping Lens.coerced

-- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
computeEnvironmentDetail_type :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEType)
computeEnvironmentDetail_type = Lens.lens (\ComputeEnvironmentDetail' {type'} -> type') (\s@ComputeEnvironmentDetail' {} a -> s {type' = a} :: ComputeEnvironmentDetail)

-- | The maximum number of VCPUs expected to be used for an unmanaged compute
-- environment.
computeEnvironmentDetail_unmanagedvCpus :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Int)
computeEnvironmentDetail_unmanagedvCpus = Lens.lens (\ComputeEnvironmentDetail' {unmanagedvCpus} -> unmanagedvCpus) (\s@ComputeEnvironmentDetail' {} a -> s {unmanagedvCpus = a} :: ComputeEnvironmentDetail)

-- | Specifies the infrastructure update policy for the compute environment.
-- For more information about infrastructure updates, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
computeEnvironmentDetail_updatePolicy :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe UpdatePolicy)
computeEnvironmentDetail_updatePolicy = Lens.lens (\ComputeEnvironmentDetail' {updatePolicy} -> updatePolicy) (\s@ComputeEnvironmentDetail' {} a -> s {updatePolicy = a} :: ComputeEnvironmentDetail)

-- | Unique identifier for the compute environment.
computeEnvironmentDetail_uuid :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_uuid = Lens.lens (\ComputeEnvironmentDetail' {uuid} -> uuid) (\s@ComputeEnvironmentDetail' {} a -> s {uuid = a} :: ComputeEnvironmentDetail)

-- | The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
computeEnvironmentDetail_computeEnvironmentName :: Lens.Lens' ComputeEnvironmentDetail Prelude.Text
computeEnvironmentDetail_computeEnvironmentName = Lens.lens (\ComputeEnvironmentDetail' {computeEnvironmentName} -> computeEnvironmentName) (\s@ComputeEnvironmentDetail' {} a -> s {computeEnvironmentName = a} :: ComputeEnvironmentDetail)

-- | The Amazon Resource Name (ARN) of the compute environment.
computeEnvironmentDetail_computeEnvironmentArn :: Lens.Lens' ComputeEnvironmentDetail Prelude.Text
computeEnvironmentDetail_computeEnvironmentArn = Lens.lens (\ComputeEnvironmentDetail' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@ComputeEnvironmentDetail' {} a -> s {computeEnvironmentArn = a} :: ComputeEnvironmentDetail)

instance Data.FromJSON ComputeEnvironmentDetail where
  parseJSON =
    Data.withObject
      "ComputeEnvironmentDetail"
      ( \x ->
          ComputeEnvironmentDetail'
            Prelude.<$> (x Data..:? "computeResources")
            Prelude.<*> (x Data..:? "containerOrchestrationType")
            Prelude.<*> (x Data..:? "ecsClusterArn")
            Prelude.<*> (x Data..:? "eksConfiguration")
            Prelude.<*> (x Data..:? "serviceRole")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "unmanagedvCpus")
            Prelude.<*> (x Data..:? "updatePolicy")
            Prelude.<*> (x Data..:? "uuid")
            Prelude.<*> (x Data..: "computeEnvironmentName")
            Prelude.<*> (x Data..: "computeEnvironmentArn")
      )

instance Prelude.Hashable ComputeEnvironmentDetail where
  hashWithSalt _salt ComputeEnvironmentDetail' {..} =
    _salt
      `Prelude.hashWithSalt` computeResources
      `Prelude.hashWithSalt` containerOrchestrationType
      `Prelude.hashWithSalt` ecsClusterArn
      `Prelude.hashWithSalt` eksConfiguration
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` unmanagedvCpus
      `Prelude.hashWithSalt` updatePolicy
      `Prelude.hashWithSalt` uuid
      `Prelude.hashWithSalt` computeEnvironmentName
      `Prelude.hashWithSalt` computeEnvironmentArn

instance Prelude.NFData ComputeEnvironmentDetail where
  rnf ComputeEnvironmentDetail' {..} =
    Prelude.rnf computeResources
      `Prelude.seq` Prelude.rnf containerOrchestrationType
      `Prelude.seq` Prelude.rnf ecsClusterArn
      `Prelude.seq` Prelude.rnf eksConfiguration
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf unmanagedvCpus
      `Prelude.seq` Prelude.rnf updatePolicy
      `Prelude.seq` Prelude.rnf uuid
      `Prelude.seq` Prelude.rnf computeEnvironmentName
      `Prelude.seq` Prelude.rnf computeEnvironmentArn
