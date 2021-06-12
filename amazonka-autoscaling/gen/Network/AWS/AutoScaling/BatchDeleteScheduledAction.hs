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
-- Module      : Network.AWS.AutoScaling.BatchDeleteScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more scheduled actions for the specified Auto Scaling
-- group.
module Network.AWS.AutoScaling.BatchDeleteScheduledAction
  ( -- * Creating a Request
    BatchDeleteScheduledAction (..),
    newBatchDeleteScheduledAction,

    -- * Request Lenses
    batchDeleteScheduledAction_autoScalingGroupName,
    batchDeleteScheduledAction_scheduledActionNames,

    -- * Destructuring the Response
    BatchDeleteScheduledActionResponse (..),
    newBatchDeleteScheduledActionResponse,

    -- * Response Lenses
    batchDeleteScheduledActionResponse_failedScheduledActions,
    batchDeleteScheduledActionResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteScheduledAction' smart constructor.
data BatchDeleteScheduledAction = BatchDeleteScheduledAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The names of the scheduled actions to delete. The maximum number allowed
    -- is 50.
    scheduledActionNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'batchDeleteScheduledAction_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'scheduledActionNames', 'batchDeleteScheduledAction_scheduledActionNames' - The names of the scheduled actions to delete. The maximum number allowed
-- is 50.
newBatchDeleteScheduledAction ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  BatchDeleteScheduledAction
newBatchDeleteScheduledAction pAutoScalingGroupName_ =
  BatchDeleteScheduledAction'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      scheduledActionNames = Core.mempty
    }

-- | The name of the Auto Scaling group.
batchDeleteScheduledAction_autoScalingGroupName :: Lens.Lens' BatchDeleteScheduledAction Core.Text
batchDeleteScheduledAction_autoScalingGroupName = Lens.lens (\BatchDeleteScheduledAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@BatchDeleteScheduledAction' {} a -> s {autoScalingGroupName = a} :: BatchDeleteScheduledAction)

-- | The names of the scheduled actions to delete. The maximum number allowed
-- is 50.
batchDeleteScheduledAction_scheduledActionNames :: Lens.Lens' BatchDeleteScheduledAction [Core.Text]
batchDeleteScheduledAction_scheduledActionNames = Lens.lens (\BatchDeleteScheduledAction' {scheduledActionNames} -> scheduledActionNames) (\s@BatchDeleteScheduledAction' {} a -> s {scheduledActionNames = a} :: BatchDeleteScheduledAction) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeleteScheduledAction where
  type
    AWSResponse BatchDeleteScheduledAction =
      BatchDeleteScheduledActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchDeleteScheduledActionResult"
      ( \s h x ->
          BatchDeleteScheduledActionResponse'
            Core.<$> ( x Core..@? "FailedScheduledActions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeleteScheduledAction

instance Core.NFData BatchDeleteScheduledAction

instance Core.ToHeaders BatchDeleteScheduledAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BatchDeleteScheduledAction where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteScheduledAction where
  toQuery BatchDeleteScheduledAction' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BatchDeleteScheduledAction" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "ScheduledActionNames"
          Core.=: Core.toQueryList "member" scheduledActionNames
      ]

-- | /See:/ 'newBatchDeleteScheduledActionResponse' smart constructor.
data BatchDeleteScheduledActionResponse = BatchDeleteScheduledActionResponse'
  { -- | The names of the scheduled actions that could not be deleted, including
    -- an error message.
    failedScheduledActions :: Core.Maybe [FailedScheduledUpdateGroupActionRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedScheduledActions', 'batchDeleteScheduledActionResponse_failedScheduledActions' - The names of the scheduled actions that could not be deleted, including
-- an error message.
--
-- 'httpStatus', 'batchDeleteScheduledActionResponse_httpStatus' - The response's http status code.
newBatchDeleteScheduledActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDeleteScheduledActionResponse
newBatchDeleteScheduledActionResponse pHttpStatus_ =
  BatchDeleteScheduledActionResponse'
    { failedScheduledActions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the scheduled actions that could not be deleted, including
-- an error message.
batchDeleteScheduledActionResponse_failedScheduledActions :: Lens.Lens' BatchDeleteScheduledActionResponse (Core.Maybe [FailedScheduledUpdateGroupActionRequest])
batchDeleteScheduledActionResponse_failedScheduledActions = Lens.lens (\BatchDeleteScheduledActionResponse' {failedScheduledActions} -> failedScheduledActions) (\s@BatchDeleteScheduledActionResponse' {} a -> s {failedScheduledActions = a} :: BatchDeleteScheduledActionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteScheduledActionResponse_httpStatus :: Lens.Lens' BatchDeleteScheduledActionResponse Core.Int
batchDeleteScheduledActionResponse_httpStatus = Lens.lens (\BatchDeleteScheduledActionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteScheduledActionResponse' {} a -> s {httpStatus = a} :: BatchDeleteScheduledActionResponse)

instance
  Core.NFData
    BatchDeleteScheduledActionResponse
