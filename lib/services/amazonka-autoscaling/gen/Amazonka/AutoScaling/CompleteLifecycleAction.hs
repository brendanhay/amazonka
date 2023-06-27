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
-- Module      : Amazonka.AutoScaling.CompleteLifecycleAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes the lifecycle action for the specified token or instance with
-- the specified result.
--
-- This step is a part of the procedure for adding a lifecycle hook to an
-- Auto Scaling group:
--
-- 1.  (Optional) Create a launch template or launch configuration with a
--     user data script that runs while an instance is in a wait state due
--     to a lifecycle hook.
--
-- 2.  (Optional) Create a Lambda function and a rule that allows Amazon
--     EventBridge to invoke your Lambda function when an instance is put
--     into a wait state due to a lifecycle hook.
--
-- 3.  (Optional) Create a notification target and an IAM role. The target
--     can be either an Amazon SQS queue or an Amazon SNS topic. The role
--     allows Amazon EC2 Auto Scaling to publish lifecycle notifications to
--     the target.
--
-- 4.  Create the lifecycle hook. Specify whether the hook is used when the
--     instances launch or terminate.
--
-- 5.  If you need more time, record the lifecycle action heartbeat to keep
--     the instance in a wait state.
--
-- 6.  __If you finish before the timeout period ends, send a callback by
--     using the CompleteLifecycleAction API call.__
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.CompleteLifecycleAction
  ( -- * Creating a Request
    CompleteLifecycleAction (..),
    newCompleteLifecycleAction,

    -- * Request Lenses
    completeLifecycleAction_instanceId,
    completeLifecycleAction_lifecycleActionToken,
    completeLifecycleAction_lifecycleHookName,
    completeLifecycleAction_autoScalingGroupName,
    completeLifecycleAction_lifecycleActionResult,

    -- * Destructuring the Response
    CompleteLifecycleActionResponse (..),
    newCompleteLifecycleActionResponse,

    -- * Response Lenses
    completeLifecycleActionResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCompleteLifecycleAction' smart constructor.
data CompleteLifecycleAction = CompleteLifecycleAction'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A universally unique identifier (UUID) that identifies a specific
    -- lifecycle action associated with an instance. Amazon EC2 Auto Scaling
    -- sends this token to the notification target you specified when you
    -- created the lifecycle hook.
    lifecycleActionToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The action for the group to take. You can specify either @CONTINUE@ or
    -- @ABANDON@.
    lifecycleActionResult :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteLifecycleAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'completeLifecycleAction_instanceId' - The ID of the instance.
--
-- 'lifecycleActionToken', 'completeLifecycleAction_lifecycleActionToken' - A universally unique identifier (UUID) that identifies a specific
-- lifecycle action associated with an instance. Amazon EC2 Auto Scaling
-- sends this token to the notification target you specified when you
-- created the lifecycle hook.
--
-- 'lifecycleHookName', 'completeLifecycleAction_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'autoScalingGroupName', 'completeLifecycleAction_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'lifecycleActionResult', 'completeLifecycleAction_lifecycleActionResult' - The action for the group to take. You can specify either @CONTINUE@ or
-- @ABANDON@.
newCompleteLifecycleAction ::
  -- | 'lifecycleHookName'
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'lifecycleActionResult'
  Prelude.Text ->
  CompleteLifecycleAction
newCompleteLifecycleAction
  pLifecycleHookName_
  pAutoScalingGroupName_
  pLifecycleActionResult_ =
    CompleteLifecycleAction'
      { instanceId =
          Prelude.Nothing,
        lifecycleActionToken = Prelude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        autoScalingGroupName = pAutoScalingGroupName_,
        lifecycleActionResult = pLifecycleActionResult_
      }

-- | The ID of the instance.
completeLifecycleAction_instanceId :: Lens.Lens' CompleteLifecycleAction (Prelude.Maybe Prelude.Text)
completeLifecycleAction_instanceId = Lens.lens (\CompleteLifecycleAction' {instanceId} -> instanceId) (\s@CompleteLifecycleAction' {} a -> s {instanceId = a} :: CompleteLifecycleAction)

-- | A universally unique identifier (UUID) that identifies a specific
-- lifecycle action associated with an instance. Amazon EC2 Auto Scaling
-- sends this token to the notification target you specified when you
-- created the lifecycle hook.
completeLifecycleAction_lifecycleActionToken :: Lens.Lens' CompleteLifecycleAction (Prelude.Maybe Prelude.Text)
completeLifecycleAction_lifecycleActionToken = Lens.lens (\CompleteLifecycleAction' {lifecycleActionToken} -> lifecycleActionToken) (\s@CompleteLifecycleAction' {} a -> s {lifecycleActionToken = a} :: CompleteLifecycleAction)

-- | The name of the lifecycle hook.
completeLifecycleAction_lifecycleHookName :: Lens.Lens' CompleteLifecycleAction Prelude.Text
completeLifecycleAction_lifecycleHookName = Lens.lens (\CompleteLifecycleAction' {lifecycleHookName} -> lifecycleHookName) (\s@CompleteLifecycleAction' {} a -> s {lifecycleHookName = a} :: CompleteLifecycleAction)

-- | The name of the Auto Scaling group.
completeLifecycleAction_autoScalingGroupName :: Lens.Lens' CompleteLifecycleAction Prelude.Text
completeLifecycleAction_autoScalingGroupName = Lens.lens (\CompleteLifecycleAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@CompleteLifecycleAction' {} a -> s {autoScalingGroupName = a} :: CompleteLifecycleAction)

-- | The action for the group to take. You can specify either @CONTINUE@ or
-- @ABANDON@.
completeLifecycleAction_lifecycleActionResult :: Lens.Lens' CompleteLifecycleAction Prelude.Text
completeLifecycleAction_lifecycleActionResult = Lens.lens (\CompleteLifecycleAction' {lifecycleActionResult} -> lifecycleActionResult) (\s@CompleteLifecycleAction' {} a -> s {lifecycleActionResult = a} :: CompleteLifecycleAction)

instance Core.AWSRequest CompleteLifecycleAction where
  type
    AWSResponse CompleteLifecycleAction =
      CompleteLifecycleActionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CompleteLifecycleActionResult"
      ( \s h x ->
          CompleteLifecycleActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CompleteLifecycleAction where
  hashWithSalt _salt CompleteLifecycleAction' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` lifecycleActionToken
      `Prelude.hashWithSalt` lifecycleHookName
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` lifecycleActionResult

instance Prelude.NFData CompleteLifecycleAction where
  rnf CompleteLifecycleAction' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf lifecycleActionToken
      `Prelude.seq` Prelude.rnf lifecycleHookName
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf lifecycleActionResult

instance Data.ToHeaders CompleteLifecycleAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CompleteLifecycleAction where
  toPath = Prelude.const "/"

instance Data.ToQuery CompleteLifecycleAction where
  toQuery CompleteLifecycleAction' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CompleteLifecycleAction" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceId" Data.=: instanceId,
        "LifecycleActionToken" Data.=: lifecycleActionToken,
        "LifecycleHookName" Data.=: lifecycleHookName,
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "LifecycleActionResult"
          Data.=: lifecycleActionResult
      ]

-- | /See:/ 'newCompleteLifecycleActionResponse' smart constructor.
data CompleteLifecycleActionResponse = CompleteLifecycleActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteLifecycleActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'completeLifecycleActionResponse_httpStatus' - The response's http status code.
newCompleteLifecycleActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CompleteLifecycleActionResponse
newCompleteLifecycleActionResponse pHttpStatus_ =
  CompleteLifecycleActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
completeLifecycleActionResponse_httpStatus :: Lens.Lens' CompleteLifecycleActionResponse Prelude.Int
completeLifecycleActionResponse_httpStatus = Lens.lens (\CompleteLifecycleActionResponse' {httpStatus} -> httpStatus) (\s@CompleteLifecycleActionResponse' {} a -> s {httpStatus = a} :: CompleteLifecycleActionResponse)

instance
  Prelude.NFData
    CompleteLifecycleActionResponse
  where
  rnf CompleteLifecycleActionResponse' {..} =
    Prelude.rnf httpStatus
