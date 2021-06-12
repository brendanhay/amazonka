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
-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB
-- instance).
module Network.AWS.RDS.ApplyPendingMaintenanceAction
  ( -- * Creating a Request
    ApplyPendingMaintenanceAction (..),
    newApplyPendingMaintenanceAction,

    -- * Request Lenses
    applyPendingMaintenanceAction_resourceIdentifier,
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
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { -- | The RDS Amazon Resource Name (ARN) of the resource that the pending
    -- maintenance action applies to. For information about creating an ARN,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
    resourceIdentifier :: Core.Text,
    -- | The pending maintenance action to apply to this resource.
    --
    -- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
    -- @ca-certificate-rotation@
    applyAction :: Core.Text,
    -- | A value that specifies the type of opt-in request, or undoes an opt-in
    -- request. An opt-in request of type @immediate@ can\'t be undone.
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
    optInType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'applyPendingMaintenanceAction_resourceIdentifier' - The RDS Amazon Resource Name (ARN) of the resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
--
-- 'applyAction', 'applyPendingMaintenanceAction_applyAction' - The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
-- @ca-certificate-rotation@
--
-- 'optInType', 'applyPendingMaintenanceAction_optInType' - A value that specifies the type of opt-in request, or undoes an opt-in
-- request. An opt-in request of type @immediate@ can\'t be undone.
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
  -- | 'resourceIdentifier'
  Core.Text ->
  -- | 'applyAction'
  Core.Text ->
  -- | 'optInType'
  Core.Text ->
  ApplyPendingMaintenanceAction
newApplyPendingMaintenanceAction
  pResourceIdentifier_
  pApplyAction_
  pOptInType_ =
    ApplyPendingMaintenanceAction'
      { resourceIdentifier =
          pResourceIdentifier_,
        applyAction = pApplyAction_,
        optInType = pOptInType_
      }

-- | The RDS Amazon Resource Name (ARN) of the resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
applyPendingMaintenanceAction_resourceIdentifier :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
applyPendingMaintenanceAction_resourceIdentifier = Lens.lens (\ApplyPendingMaintenanceAction' {resourceIdentifier} -> resourceIdentifier) (\s@ApplyPendingMaintenanceAction' {} a -> s {resourceIdentifier = a} :: ApplyPendingMaintenanceAction)

-- | The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
-- @ca-certificate-rotation@
applyPendingMaintenanceAction_applyAction :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
applyPendingMaintenanceAction_applyAction = Lens.lens (\ApplyPendingMaintenanceAction' {applyAction} -> applyAction) (\s@ApplyPendingMaintenanceAction' {} a -> s {applyAction = a} :: ApplyPendingMaintenanceAction)

-- | A value that specifies the type of opt-in request, or undoes an opt-in
-- request. An opt-in request of type @immediate@ can\'t be undone.
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
applyPendingMaintenanceAction_optInType :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
applyPendingMaintenanceAction_optInType = Lens.lens (\ApplyPendingMaintenanceAction' {optInType} -> optInType) (\s@ApplyPendingMaintenanceAction' {} a -> s {optInType = a} :: ApplyPendingMaintenanceAction)

instance
  Core.AWSRequest
    ApplyPendingMaintenanceAction
  where
  type
    AWSResponse ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ApplyPendingMaintenanceActionResult"
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Core.<$> (x Core..@? "ResourcePendingMaintenanceActions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ApplyPendingMaintenanceAction

instance Core.NFData ApplyPendingMaintenanceAction

instance Core.ToHeaders ApplyPendingMaintenanceAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ApplyPendingMaintenanceAction where
  toPath = Core.const "/"

instance Core.ToQuery ApplyPendingMaintenanceAction where
  toQuery ApplyPendingMaintenanceAction' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ApplyPendingMaintenanceAction" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ResourceIdentifier" Core.=: resourceIdentifier,
        "ApplyAction" Core.=: applyAction,
        "OptInType" Core.=: optInType
      ]

-- | /See:/ 'newApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { resourcePendingMaintenanceActions :: Core.Maybe ResourcePendingMaintenanceActions,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePendingMaintenanceActions', 'applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions' - Undocumented member.
--
-- 'httpStatus', 'applyPendingMaintenanceActionResponse_httpStatus' - The response's http status code.
newApplyPendingMaintenanceActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ApplyPendingMaintenanceActionResponse
newApplyPendingMaintenanceActionResponse pHttpStatus_ =
  ApplyPendingMaintenanceActionResponse'
    { resourcePendingMaintenanceActions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Core.Maybe ResourcePendingMaintenanceActions)
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions = Lens.lens (\ApplyPendingMaintenanceActionResponse' {resourcePendingMaintenanceActions} -> resourcePendingMaintenanceActions) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {resourcePendingMaintenanceActions = a} :: ApplyPendingMaintenanceActionResponse)

-- | The response's http status code.
applyPendingMaintenanceActionResponse_httpStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Core.Int
applyPendingMaintenanceActionResponse_httpStatus = Lens.lens (\ApplyPendingMaintenanceActionResponse' {httpStatus} -> httpStatus) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {httpStatus = a} :: ApplyPendingMaintenanceActionResponse)

instance
  Core.NFData
    ApplyPendingMaintenanceActionResponse
