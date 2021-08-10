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
-- Module      : Network.AWS.DMS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a
-- replication instance).
module Network.AWS.DMS.ApplyPendingMaintenanceAction
  ( -- * Creating a Request
    ApplyPendingMaintenanceAction (..),
    newApplyPendingMaintenanceAction,

    -- * Request Lenses
    applyPendingMaintenanceAction_replicationInstanceArn,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,

    -- * Destructuring the Response
    ApplyPendingMaintenanceActionResponse (..),
    newApplyPendingMaintenanceActionResponse,

    -- * Response Lenses
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { -- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending
    -- maintenance action applies to.
    replicationInstanceArn :: Prelude.Text,
    -- | The pending maintenance action to apply to this resource.
    applyAction :: Prelude.Text,
    -- | A value that specifies the type of opt-in request, or undoes an opt-in
    -- request. You can\'t undo an opt-in request of type @immediate@.
    --
    -- Valid values:
    --
    -- -   @immediate@ - Apply the maintenance action immediately.
    --
    -- -   @next-maintenance@ - Apply the maintenance action during the next
    --     maintenance window for the resource.
    --
    -- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
    --     requests.
    optInType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstanceArn', 'applyPendingMaintenanceAction_replicationInstanceArn' - The Amazon Resource Name (ARN) of the AWS DMS resource that the pending
-- maintenance action applies to.
--
-- 'applyAction', 'applyPendingMaintenanceAction_applyAction' - The pending maintenance action to apply to this resource.
--
-- 'optInType', 'applyPendingMaintenanceAction_optInType' - A value that specifies the type of opt-in request, or undoes an opt-in
-- request. You can\'t undo an opt-in request of type @immediate@.
--
-- Valid values:
--
-- -   @immediate@ - Apply the maintenance action immediately.
--
-- -   @next-maintenance@ - Apply the maintenance action during the next
--     maintenance window for the resource.
--
-- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
--     requests.
newApplyPendingMaintenanceAction ::
  -- | 'replicationInstanceArn'
  Prelude.Text ->
  -- | 'applyAction'
  Prelude.Text ->
  -- | 'optInType'
  Prelude.Text ->
  ApplyPendingMaintenanceAction
newApplyPendingMaintenanceAction
  pReplicationInstanceArn_
  pApplyAction_
  pOptInType_ =
    ApplyPendingMaintenanceAction'
      { replicationInstanceArn =
          pReplicationInstanceArn_,
        applyAction = pApplyAction_,
        optInType = pOptInType_
      }

-- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending
-- maintenance action applies to.
applyPendingMaintenanceAction_replicationInstanceArn :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_replicationInstanceArn = Lens.lens (\ApplyPendingMaintenanceAction' {replicationInstanceArn} -> replicationInstanceArn) (\s@ApplyPendingMaintenanceAction' {} a -> s {replicationInstanceArn = a} :: ApplyPendingMaintenanceAction)

-- | The pending maintenance action to apply to this resource.
applyPendingMaintenanceAction_applyAction :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_applyAction = Lens.lens (\ApplyPendingMaintenanceAction' {applyAction} -> applyAction) (\s@ApplyPendingMaintenanceAction' {} a -> s {applyAction = a} :: ApplyPendingMaintenanceAction)

-- | A value that specifies the type of opt-in request, or undoes an opt-in
-- request. You can\'t undo an opt-in request of type @immediate@.
--
-- Valid values:
--
-- -   @immediate@ - Apply the maintenance action immediately.
--
-- -   @next-maintenance@ - Apply the maintenance action during the next
--     maintenance window for the resource.
--
-- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
--     requests.
applyPendingMaintenanceAction_optInType :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_optInType = Lens.lens (\ApplyPendingMaintenanceAction' {optInType} -> optInType) (\s@ApplyPendingMaintenanceAction' {} a -> s {optInType = a} :: ApplyPendingMaintenanceAction)

instance
  Core.AWSRequest
    ApplyPendingMaintenanceAction
  where
  type
    AWSResponse ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Prelude.<$> (x Core..?> "ResourcePendingMaintenanceActions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ApplyPendingMaintenanceAction

instance Prelude.NFData ApplyPendingMaintenanceAction

instance Core.ToHeaders ApplyPendingMaintenanceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ApplyPendingMaintenanceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ApplyPendingMaintenanceAction where
  toJSON ApplyPendingMaintenanceAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              ),
            Prelude.Just ("ApplyAction" Core..= applyAction),
            Prelude.Just ("OptInType" Core..= optInType)
          ]
      )

instance Core.ToPath ApplyPendingMaintenanceAction where
  toPath = Prelude.const "/"

instance Core.ToQuery ApplyPendingMaintenanceAction where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { -- | The AWS DMS resource that the pending maintenance action will be applied
    -- to.
    resourcePendingMaintenanceActions :: Prelude.Maybe ResourcePendingMaintenanceActions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePendingMaintenanceActions', 'applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions' - The AWS DMS resource that the pending maintenance action will be applied
-- to.
--
-- 'httpStatus', 'applyPendingMaintenanceActionResponse_httpStatus' - The response's http status code.
newApplyPendingMaintenanceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ApplyPendingMaintenanceActionResponse
newApplyPendingMaintenanceActionResponse pHttpStatus_ =
  ApplyPendingMaintenanceActionResponse'
    { resourcePendingMaintenanceActions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AWS DMS resource that the pending maintenance action will be applied
-- to.
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Prelude.Maybe ResourcePendingMaintenanceActions)
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions = Lens.lens (\ApplyPendingMaintenanceActionResponse' {resourcePendingMaintenanceActions} -> resourcePendingMaintenanceActions) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {resourcePendingMaintenanceActions = a} :: ApplyPendingMaintenanceActionResponse)

-- | The response's http status code.
applyPendingMaintenanceActionResponse_httpStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Prelude.Int
applyPendingMaintenanceActionResponse_httpStatus = Lens.lens (\ApplyPendingMaintenanceActionResponse' {httpStatus} -> httpStatus) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {httpStatus = a} :: ApplyPendingMaintenanceActionResponse)

instance
  Prelude.NFData
    ApplyPendingMaintenanceActionResponse
