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
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeEnvironmentDetail where

import Network.AWS.Batch.Types.CEState
import Network.AWS.Batch.Types.CEStatus
import Network.AWS.Batch.Types.CEType
import Network.AWS.Batch.Types.ComputeResource
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Batch compute environment.
--
-- /See:/ 'newComputeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
  { -- | The current status of the compute environment (for example, @CREATING@
    -- or @VALID@).
    status :: Prelude.Maybe CEStatus,
    -- | The state of the compute environment. The valid values are @ENABLED@ or
    -- @DISABLED@.
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
    -- | The compute resources defined for the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /Batch User Guide/.
    computeResources :: Prelude.Maybe ComputeResource,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the compute environment.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /Batch User Guide/.
    type' :: Prelude.Maybe CEType,
    -- | The service role associated with the compute environment that allows
    -- Batch to make calls to Amazon Web Services API operations on your
    -- behalf. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
    -- in the /Batch User Guide/.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The tags applied to the compute environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the compute environment. Up to 128 letters (uppercase and
    -- lowercase), numbers, hyphens, and underscores are allowed.
    computeEnvironmentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used
    -- by the compute environment.
    ecsClusterArn :: Prelude.Text
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
-- 'status', 'computeEnvironmentDetail_status' - The current status of the compute environment (for example, @CREATING@
-- or @VALID@).
--
-- 'state', 'computeEnvironmentDetail_state' - The state of the compute environment. The valid values are @ENABLED@ or
-- @DISABLED@.
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
-- 'computeResources', 'computeEnvironmentDetail_computeResources' - The compute resources defined for the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
--
-- 'statusReason', 'computeEnvironmentDetail_statusReason' - A short, human-readable string to provide additional details about the
-- current status of the compute environment.
--
-- 'type'', 'computeEnvironmentDetail_type' - The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
--
-- 'serviceRole', 'computeEnvironmentDetail_serviceRole' - The service role associated with the compute environment that allows
-- Batch to make calls to Amazon Web Services API operations on your
-- behalf. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- 'tags', 'computeEnvironmentDetail_tags' - The tags applied to the compute environment.
--
-- 'computeEnvironmentName', 'computeEnvironmentDetail_computeEnvironmentName' - The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
--
-- 'computeEnvironmentArn', 'computeEnvironmentDetail_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
--
-- 'ecsClusterArn', 'computeEnvironmentDetail_ecsClusterArn' - The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used
-- by the compute environment.
newComputeEnvironmentDetail ::
  -- | 'computeEnvironmentName'
  Prelude.Text ->
  -- | 'computeEnvironmentArn'
  Prelude.Text ->
  -- | 'ecsClusterArn'
  Prelude.Text ->
  ComputeEnvironmentDetail
newComputeEnvironmentDetail
  pComputeEnvironmentName_
  pComputeEnvironmentArn_
  pEcsClusterArn_ =
    ComputeEnvironmentDetail'
      { status = Prelude.Nothing,
        state = Prelude.Nothing,
        computeResources = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        type' = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        tags = Prelude.Nothing,
        computeEnvironmentName = pComputeEnvironmentName_,
        computeEnvironmentArn = pComputeEnvironmentArn_,
        ecsClusterArn = pEcsClusterArn_
      }

-- | The current status of the compute environment (for example, @CREATING@
-- or @VALID@).
computeEnvironmentDetail_status :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEStatus)
computeEnvironmentDetail_status = Lens.lens (\ComputeEnvironmentDetail' {status} -> status) (\s@ComputeEnvironmentDetail' {} a -> s {status = a} :: ComputeEnvironmentDetail)

-- | The state of the compute environment. The valid values are @ENABLED@ or
-- @DISABLED@.
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
computeEnvironmentDetail_state :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEState)
computeEnvironmentDetail_state = Lens.lens (\ComputeEnvironmentDetail' {state} -> state) (\s@ComputeEnvironmentDetail' {} a -> s {state = a} :: ComputeEnvironmentDetail)

-- | The compute resources defined for the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
computeEnvironmentDetail_computeResources :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe ComputeResource)
computeEnvironmentDetail_computeResources = Lens.lens (\ComputeEnvironmentDetail' {computeResources} -> computeResources) (\s@ComputeEnvironmentDetail' {} a -> s {computeResources = a} :: ComputeEnvironmentDetail)

-- | A short, human-readable string to provide additional details about the
-- current status of the compute environment.
computeEnvironmentDetail_statusReason :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_statusReason = Lens.lens (\ComputeEnvironmentDetail' {statusReason} -> statusReason) (\s@ComputeEnvironmentDetail' {} a -> s {statusReason = a} :: ComputeEnvironmentDetail)

-- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
computeEnvironmentDetail_type :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe CEType)
computeEnvironmentDetail_type = Lens.lens (\ComputeEnvironmentDetail' {type'} -> type') (\s@ComputeEnvironmentDetail' {} a -> s {type' = a} :: ComputeEnvironmentDetail)

-- | The service role associated with the compute environment that allows
-- Batch to make calls to Amazon Web Services API operations on your
-- behalf. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
computeEnvironmentDetail_serviceRole :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe Prelude.Text)
computeEnvironmentDetail_serviceRole = Lens.lens (\ComputeEnvironmentDetail' {serviceRole} -> serviceRole) (\s@ComputeEnvironmentDetail' {} a -> s {serviceRole = a} :: ComputeEnvironmentDetail)

-- | The tags applied to the compute environment.
computeEnvironmentDetail_tags :: Lens.Lens' ComputeEnvironmentDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
computeEnvironmentDetail_tags = Lens.lens (\ComputeEnvironmentDetail' {tags} -> tags) (\s@ComputeEnvironmentDetail' {} a -> s {tags = a} :: ComputeEnvironmentDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
computeEnvironmentDetail_computeEnvironmentName :: Lens.Lens' ComputeEnvironmentDetail Prelude.Text
computeEnvironmentDetail_computeEnvironmentName = Lens.lens (\ComputeEnvironmentDetail' {computeEnvironmentName} -> computeEnvironmentName) (\s@ComputeEnvironmentDetail' {} a -> s {computeEnvironmentName = a} :: ComputeEnvironmentDetail)

-- | The Amazon Resource Name (ARN) of the compute environment.
computeEnvironmentDetail_computeEnvironmentArn :: Lens.Lens' ComputeEnvironmentDetail Prelude.Text
computeEnvironmentDetail_computeEnvironmentArn = Lens.lens (\ComputeEnvironmentDetail' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@ComputeEnvironmentDetail' {} a -> s {computeEnvironmentArn = a} :: ComputeEnvironmentDetail)

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used
-- by the compute environment.
computeEnvironmentDetail_ecsClusterArn :: Lens.Lens' ComputeEnvironmentDetail Prelude.Text
computeEnvironmentDetail_ecsClusterArn = Lens.lens (\ComputeEnvironmentDetail' {ecsClusterArn} -> ecsClusterArn) (\s@ComputeEnvironmentDetail' {} a -> s {ecsClusterArn = a} :: ComputeEnvironmentDetail)

instance Core.FromJSON ComputeEnvironmentDetail where
  parseJSON =
    Core.withObject
      "ComputeEnvironmentDetail"
      ( \x ->
          ComputeEnvironmentDetail'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "computeResources")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "serviceRole")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "computeEnvironmentName")
            Prelude.<*> (x Core..: "computeEnvironmentArn")
            Prelude.<*> (x Core..: "ecsClusterArn")
      )

instance Prelude.Hashable ComputeEnvironmentDetail

instance Prelude.NFData ComputeEnvironmentDetail
