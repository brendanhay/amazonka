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
-- Module      : Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a scheduled managed action immediately. A managed action can be
-- applied only if its status is @Scheduled@. Get the status and action ID
-- of a managed action with DescribeEnvironmentManagedActions.
module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
  ( -- * Creating a Request
    ApplyEnvironmentManagedAction (..),
    newApplyEnvironmentManagedAction,

    -- * Request Lenses
    applyEnvironmentManagedAction_environmentId,
    applyEnvironmentManagedAction_environmentName,
    applyEnvironmentManagedAction_actionId,

    -- * Destructuring the Response
    ApplyEnvironmentManagedActionResponse (..),
    newApplyEnvironmentManagedActionResponse,

    -- * Response Lenses
    applyEnvironmentManagedActionResponse_status,
    applyEnvironmentManagedActionResponse_actionType,
    applyEnvironmentManagedActionResponse_actionId,
    applyEnvironmentManagedActionResponse_actionDescription,
    applyEnvironmentManagedActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to execute a scheduled managed action immediately.
--
-- /See:/ 'newApplyEnvironmentManagedAction' smart constructor.
data ApplyEnvironmentManagedAction = ApplyEnvironmentManagedAction'
  { -- | The environment ID of the target environment.
    environmentId :: Core.Maybe Core.Text,
    -- | The name of the target environment.
    environmentName :: Core.Maybe Core.Text,
    -- | The action ID of the scheduled managed action to execute.
    actionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplyEnvironmentManagedAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'applyEnvironmentManagedAction_environmentId' - The environment ID of the target environment.
--
-- 'environmentName', 'applyEnvironmentManagedAction_environmentName' - The name of the target environment.
--
-- 'actionId', 'applyEnvironmentManagedAction_actionId' - The action ID of the scheduled managed action to execute.
newApplyEnvironmentManagedAction ::
  -- | 'actionId'
  Core.Text ->
  ApplyEnvironmentManagedAction
newApplyEnvironmentManagedAction pActionId_ =
  ApplyEnvironmentManagedAction'
    { environmentId =
        Core.Nothing,
      environmentName = Core.Nothing,
      actionId = pActionId_
    }

-- | The environment ID of the target environment.
applyEnvironmentManagedAction_environmentId :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Core.Text)
applyEnvironmentManagedAction_environmentId = Lens.lens (\ApplyEnvironmentManagedAction' {environmentId} -> environmentId) (\s@ApplyEnvironmentManagedAction' {} a -> s {environmentId = a} :: ApplyEnvironmentManagedAction)

-- | The name of the target environment.
applyEnvironmentManagedAction_environmentName :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Core.Text)
applyEnvironmentManagedAction_environmentName = Lens.lens (\ApplyEnvironmentManagedAction' {environmentName} -> environmentName) (\s@ApplyEnvironmentManagedAction' {} a -> s {environmentName = a} :: ApplyEnvironmentManagedAction)

-- | The action ID of the scheduled managed action to execute.
applyEnvironmentManagedAction_actionId :: Lens.Lens' ApplyEnvironmentManagedAction Core.Text
applyEnvironmentManagedAction_actionId = Lens.lens (\ApplyEnvironmentManagedAction' {actionId} -> actionId) (\s@ApplyEnvironmentManagedAction' {} a -> s {actionId = a} :: ApplyEnvironmentManagedAction)

instance
  Core.AWSRequest
    ApplyEnvironmentManagedAction
  where
  type
    AWSResponse ApplyEnvironmentManagedAction =
      ApplyEnvironmentManagedActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ApplyEnvironmentManagedActionResult"
      ( \s h x ->
          ApplyEnvironmentManagedActionResponse'
            Core.<$> (x Core..@? "Status")
            Core.<*> (x Core..@? "ActionType")
            Core.<*> (x Core..@? "ActionId")
            Core.<*> (x Core..@? "ActionDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ApplyEnvironmentManagedAction

instance Core.NFData ApplyEnvironmentManagedAction

instance Core.ToHeaders ApplyEnvironmentManagedAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ApplyEnvironmentManagedAction where
  toPath = Core.const "/"

instance Core.ToQuery ApplyEnvironmentManagedAction where
  toQuery ApplyEnvironmentManagedAction' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ApplyEnvironmentManagedAction" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "EnvironmentId" Core.=: environmentId,
        "EnvironmentName" Core.=: environmentName,
        "ActionId" Core.=: actionId
      ]

-- | The result message containing information about the managed action.
--
-- /See:/ 'newApplyEnvironmentManagedActionResponse' smart constructor.
data ApplyEnvironmentManagedActionResponse = ApplyEnvironmentManagedActionResponse'
  { -- | The status of the managed action.
    status :: Core.Maybe Core.Text,
    -- | The type of managed action.
    actionType :: Core.Maybe ActionType,
    -- | The action ID of the managed action.
    actionId :: Core.Maybe Core.Text,
    -- | A description of the managed action.
    actionDescription :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplyEnvironmentManagedActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'applyEnvironmentManagedActionResponse_status' - The status of the managed action.
--
-- 'actionType', 'applyEnvironmentManagedActionResponse_actionType' - The type of managed action.
--
-- 'actionId', 'applyEnvironmentManagedActionResponse_actionId' - The action ID of the managed action.
--
-- 'actionDescription', 'applyEnvironmentManagedActionResponse_actionDescription' - A description of the managed action.
--
-- 'httpStatus', 'applyEnvironmentManagedActionResponse_httpStatus' - The response's http status code.
newApplyEnvironmentManagedActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ApplyEnvironmentManagedActionResponse
newApplyEnvironmentManagedActionResponse pHttpStatus_ =
  ApplyEnvironmentManagedActionResponse'
    { status =
        Core.Nothing,
      actionType = Core.Nothing,
      actionId = Core.Nothing,
      actionDescription = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the managed action.
applyEnvironmentManagedActionResponse_status :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
applyEnvironmentManagedActionResponse_status = Lens.lens (\ApplyEnvironmentManagedActionResponse' {status} -> status) (\s@ApplyEnvironmentManagedActionResponse' {} a -> s {status = a} :: ApplyEnvironmentManagedActionResponse)

-- | The type of managed action.
applyEnvironmentManagedActionResponse_actionType :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe ActionType)
applyEnvironmentManagedActionResponse_actionType = Lens.lens (\ApplyEnvironmentManagedActionResponse' {actionType} -> actionType) (\s@ApplyEnvironmentManagedActionResponse' {} a -> s {actionType = a} :: ApplyEnvironmentManagedActionResponse)

-- | The action ID of the managed action.
applyEnvironmentManagedActionResponse_actionId :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
applyEnvironmentManagedActionResponse_actionId = Lens.lens (\ApplyEnvironmentManagedActionResponse' {actionId} -> actionId) (\s@ApplyEnvironmentManagedActionResponse' {} a -> s {actionId = a} :: ApplyEnvironmentManagedActionResponse)

-- | A description of the managed action.
applyEnvironmentManagedActionResponse_actionDescription :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
applyEnvironmentManagedActionResponse_actionDescription = Lens.lens (\ApplyEnvironmentManagedActionResponse' {actionDescription} -> actionDescription) (\s@ApplyEnvironmentManagedActionResponse' {} a -> s {actionDescription = a} :: ApplyEnvironmentManagedActionResponse)

-- | The response's http status code.
applyEnvironmentManagedActionResponse_httpStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse Core.Int
applyEnvironmentManagedActionResponse_httpStatus = Lens.lens (\ApplyEnvironmentManagedActionResponse' {httpStatus} -> httpStatus) (\s@ApplyEnvironmentManagedActionResponse' {} a -> s {httpStatus = a} :: ApplyEnvironmentManagedActionResponse)

instance
  Core.NFData
    ApplyEnvironmentManagedActionResponse
