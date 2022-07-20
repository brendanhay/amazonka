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
-- Module      : Amazonka.EC2.CreateReplaceRootVolumeTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a root volume replacement task for an Amazon EC2 instance. The
-- root volume can either be restored to its initial launch state, or it
-- can be restored using a specific snapshot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-restoring-volume.html#replace-root Replace a root volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateReplaceRootVolumeTask
  ( -- * Creating a Request
    CreateReplaceRootVolumeTask (..),
    newCreateReplaceRootVolumeTask,

    -- * Request Lenses
    createReplaceRootVolumeTask_clientToken,
    createReplaceRootVolumeTask_snapshotId,
    createReplaceRootVolumeTask_dryRun,
    createReplaceRootVolumeTask_tagSpecifications,
    createReplaceRootVolumeTask_instanceId,

    -- * Destructuring the Response
    CreateReplaceRootVolumeTaskResponse (..),
    newCreateReplaceRootVolumeTaskResponse,

    -- * Response Lenses
    createReplaceRootVolumeTaskResponse_replaceRootVolumeTask,
    createReplaceRootVolumeTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateReplaceRootVolumeTask' smart constructor.
data CreateReplaceRootVolumeTask = CreateReplaceRootVolumeTask'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, a randomly
    -- generated token is used for the request to ensure idempotency. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot from which to restore the replacement root
    -- volume. If you want to restore the volume to the initial launch state,
    -- omit this parameter.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the root volume replacement task.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the instance for which to replace the root volume.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplaceRootVolumeTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createReplaceRootVolumeTask_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, a randomly
-- generated token is used for the request to ensure idempotency. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- 'snapshotId', 'createReplaceRootVolumeTask_snapshotId' - The ID of the snapshot from which to restore the replacement root
-- volume. If you want to restore the volume to the initial launch state,
-- omit this parameter.
--
-- 'dryRun', 'createReplaceRootVolumeTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createReplaceRootVolumeTask_tagSpecifications' - The tags to apply to the root volume replacement task.
--
-- 'instanceId', 'createReplaceRootVolumeTask_instanceId' - The ID of the instance for which to replace the root volume.
newCreateReplaceRootVolumeTask ::
  -- | 'instanceId'
  Prelude.Text ->
  CreateReplaceRootVolumeTask
newCreateReplaceRootVolumeTask pInstanceId_ =
  CreateReplaceRootVolumeTask'
    { clientToken =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, a randomly
-- generated token is used for the request to ensure idempotency. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
createReplaceRootVolumeTask_clientToken :: Lens.Lens' CreateReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
createReplaceRootVolumeTask_clientToken = Lens.lens (\CreateReplaceRootVolumeTask' {clientToken} -> clientToken) (\s@CreateReplaceRootVolumeTask' {} a -> s {clientToken = a} :: CreateReplaceRootVolumeTask)

-- | The ID of the snapshot from which to restore the replacement root
-- volume. If you want to restore the volume to the initial launch state,
-- omit this parameter.
createReplaceRootVolumeTask_snapshotId :: Lens.Lens' CreateReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
createReplaceRootVolumeTask_snapshotId = Lens.lens (\CreateReplaceRootVolumeTask' {snapshotId} -> snapshotId) (\s@CreateReplaceRootVolumeTask' {} a -> s {snapshotId = a} :: CreateReplaceRootVolumeTask)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createReplaceRootVolumeTask_dryRun :: Lens.Lens' CreateReplaceRootVolumeTask (Prelude.Maybe Prelude.Bool)
createReplaceRootVolumeTask_dryRun = Lens.lens (\CreateReplaceRootVolumeTask' {dryRun} -> dryRun) (\s@CreateReplaceRootVolumeTask' {} a -> s {dryRun = a} :: CreateReplaceRootVolumeTask)

-- | The tags to apply to the root volume replacement task.
createReplaceRootVolumeTask_tagSpecifications :: Lens.Lens' CreateReplaceRootVolumeTask (Prelude.Maybe [TagSpecification])
createReplaceRootVolumeTask_tagSpecifications = Lens.lens (\CreateReplaceRootVolumeTask' {tagSpecifications} -> tagSpecifications) (\s@CreateReplaceRootVolumeTask' {} a -> s {tagSpecifications = a} :: CreateReplaceRootVolumeTask) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the instance for which to replace the root volume.
createReplaceRootVolumeTask_instanceId :: Lens.Lens' CreateReplaceRootVolumeTask Prelude.Text
createReplaceRootVolumeTask_instanceId = Lens.lens (\CreateReplaceRootVolumeTask' {instanceId} -> instanceId) (\s@CreateReplaceRootVolumeTask' {} a -> s {instanceId = a} :: CreateReplaceRootVolumeTask)

instance Core.AWSRequest CreateReplaceRootVolumeTask where
  type
    AWSResponse CreateReplaceRootVolumeTask =
      CreateReplaceRootVolumeTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateReplaceRootVolumeTaskResponse'
            Prelude.<$> (x Core..@? "replaceRootVolumeTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplaceRootVolumeTask where
  hashWithSalt _salt CreateReplaceRootVolumeTask' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData CreateReplaceRootVolumeTask where
  rnf CreateReplaceRootVolumeTask' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders CreateReplaceRootVolumeTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateReplaceRootVolumeTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateReplaceRootVolumeTask where
  toQuery CreateReplaceRootVolumeTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateReplaceRootVolumeTask" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Core.=: clientToken,
        "SnapshotId" Core.=: snapshotId,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newCreateReplaceRootVolumeTaskResponse' smart constructor.
data CreateReplaceRootVolumeTaskResponse = CreateReplaceRootVolumeTaskResponse'
  { -- | Information about the root volume replacement task.
    replaceRootVolumeTask :: Prelude.Maybe ReplaceRootVolumeTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplaceRootVolumeTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replaceRootVolumeTask', 'createReplaceRootVolumeTaskResponse_replaceRootVolumeTask' - Information about the root volume replacement task.
--
-- 'httpStatus', 'createReplaceRootVolumeTaskResponse_httpStatus' - The response's http status code.
newCreateReplaceRootVolumeTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReplaceRootVolumeTaskResponse
newCreateReplaceRootVolumeTaskResponse pHttpStatus_ =
  CreateReplaceRootVolumeTaskResponse'
    { replaceRootVolumeTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the root volume replacement task.
createReplaceRootVolumeTaskResponse_replaceRootVolumeTask :: Lens.Lens' CreateReplaceRootVolumeTaskResponse (Prelude.Maybe ReplaceRootVolumeTask)
createReplaceRootVolumeTaskResponse_replaceRootVolumeTask = Lens.lens (\CreateReplaceRootVolumeTaskResponse' {replaceRootVolumeTask} -> replaceRootVolumeTask) (\s@CreateReplaceRootVolumeTaskResponse' {} a -> s {replaceRootVolumeTask = a} :: CreateReplaceRootVolumeTaskResponse)

-- | The response's http status code.
createReplaceRootVolumeTaskResponse_httpStatus :: Lens.Lens' CreateReplaceRootVolumeTaskResponse Prelude.Int
createReplaceRootVolumeTaskResponse_httpStatus = Lens.lens (\CreateReplaceRootVolumeTaskResponse' {httpStatus} -> httpStatus) (\s@CreateReplaceRootVolumeTaskResponse' {} a -> s {httpStatus = a} :: CreateReplaceRootVolumeTaskResponse)

instance
  Prelude.NFData
    CreateReplaceRootVolumeTaskResponse
  where
  rnf CreateReplaceRootVolumeTaskResponse' {..} =
    Prelude.rnf replaceRootVolumeTask
      `Prelude.seq` Prelude.rnf httpStatus
