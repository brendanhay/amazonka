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
-- Module      : Amazonka.AutoScaling.PutLifecycleHook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle hook for the specified Auto Scaling
-- group.
--
-- Lifecycle hooks let you create solutions that are aware of events in the
-- Auto Scaling instance lifecycle, and then perform a custom action on
-- instances when the corresponding lifecycle event occurs.
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
-- 4.  __Create the lifecycle hook. Specify whether the hook is used when
--     the instances launch or terminate.__
--
-- 5.  If you need more time, record the lifecycle action heartbeat to keep
--     the instance in a wait state using the
--     RecordLifecycleActionHeartbeat API call.
--
-- 6.  If you finish before the timeout period ends, send a callback by
--     using the CompleteLifecycleAction API call.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you exceed your maximum limit of lifecycle hooks, which by default is
-- 50 per Auto Scaling group, the call fails.
--
-- You can view the lifecycle hooks for an Auto Scaling group using the
-- DescribeLifecycleHooks API call. If you are no longer using a lifecycle
-- hook, you can delete it by calling the DeleteLifecycleHook API.
module Amazonka.AutoScaling.PutLifecycleHook
  ( -- * Creating a Request
    PutLifecycleHook (..),
    newPutLifecycleHook,

    -- * Request Lenses
    putLifecycleHook_roleARN,
    putLifecycleHook_notificationMetadata,
    putLifecycleHook_defaultResult,
    putLifecycleHook_notificationTargetARN,
    putLifecycleHook_lifecycleTransition,
    putLifecycleHook_heartbeatTimeout,
    putLifecycleHook_lifecycleHookName,
    putLifecycleHook_autoScalingGroupName,

    -- * Destructuring the Response
    PutLifecycleHookResponse (..),
    newPutLifecycleHookResponse,

    -- * Response Lenses
    putLifecycleHookResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLifecycleHook' smart constructor.
data PutLifecycleHook = PutLifecycleHook'
  { -- | The ARN of the IAM role that allows the Auto Scaling group to publish to
    -- the specified notification target.
    --
    -- Valid only if the notification target is an Amazon SNS topic or an
    -- Amazon SQS queue. Required for new lifecycle hooks, but optional when
    -- updating existing hooks.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Additional information that you want to include any time Amazon EC2 Auto
    -- Scaling sends a message to the notification target.
    notificationMetadata :: Prelude.Maybe Prelude.Text,
    -- | The action the Auto Scaling group takes when the lifecycle hook timeout
    -- elapses or if an unexpected failure occurs. The default value is
    -- @ABANDON@.
    --
    -- Valid values: @CONTINUE@ | @ABANDON@
    defaultResult :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the notification target that Amazon
    -- EC2 Auto Scaling uses to notify you when an instance is in a wait state
    -- for the lifecycle hook. You can specify either an Amazon SNS topic or an
    -- Amazon SQS queue.
    --
    -- If you specify an empty string, this overrides the current ARN.
    --
    -- This operation uses the JSON format when sending notifications to an
    -- Amazon SQS queue, and an email key-value pair format when sending
    -- notifications to an Amazon SNS topic.
    --
    -- When you specify a notification target, Amazon EC2 Auto Scaling sends it
    -- a test message. Test messages contain the following additional key-value
    -- pair: @\"Event\": \"autoscaling:TEST_NOTIFICATION\"@.
    notificationTargetARN :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle transition. For Auto Scaling groups, there are two major
    -- lifecycle transitions.
    --
    -- -   To create a lifecycle hook for scale-out events, specify
    --     @autoscaling:EC2_INSTANCE_LAUNCHING@.
    --
    -- -   To create a lifecycle hook for scale-in events, specify
    --     @autoscaling:EC2_INSTANCE_TERMINATING@.
    --
    -- Required for new lifecycle hooks, but optional when updating existing
    -- hooks.
    lifecycleTransition :: Prelude.Maybe Prelude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook
    -- times out. The range is from @30@ to @7200@ seconds. The default value
    -- is @3600@ seconds (1 hour).
    heartbeatTimeout :: Prelude.Maybe Prelude.Int,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLifecycleHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'putLifecycleHook_roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
--
-- Valid only if the notification target is an Amazon SNS topic or an
-- Amazon SQS queue. Required for new lifecycle hooks, but optional when
-- updating existing hooks.
--
-- 'notificationMetadata', 'putLifecycleHook_notificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
--
-- 'defaultResult', 'putLifecycleHook_defaultResult' - The action the Auto Scaling group takes when the lifecycle hook timeout
-- elapses or if an unexpected failure occurs. The default value is
-- @ABANDON@.
--
-- Valid values: @CONTINUE@ | @ABANDON@
--
-- 'notificationTargetARN', 'putLifecycleHook_notificationTargetARN' - The Amazon Resource Name (ARN) of the notification target that Amazon
-- EC2 Auto Scaling uses to notify you when an instance is in a wait state
-- for the lifecycle hook. You can specify either an Amazon SNS topic or an
-- Amazon SQS queue.
--
-- If you specify an empty string, this overrides the current ARN.
--
-- This operation uses the JSON format when sending notifications to an
-- Amazon SQS queue, and an email key-value pair format when sending
-- notifications to an Amazon SNS topic.
--
-- When you specify a notification target, Amazon EC2 Auto Scaling sends it
-- a test message. Test messages contain the following additional key-value
-- pair: @\"Event\": \"autoscaling:TEST_NOTIFICATION\"@.
--
-- 'lifecycleTransition', 'putLifecycleHook_lifecycleTransition' - The lifecycle transition. For Auto Scaling groups, there are two major
-- lifecycle transitions.
--
-- -   To create a lifecycle hook for scale-out events, specify
--     @autoscaling:EC2_INSTANCE_LAUNCHING@.
--
-- -   To create a lifecycle hook for scale-in events, specify
--     @autoscaling:EC2_INSTANCE_TERMINATING@.
--
-- Required for new lifecycle hooks, but optional when updating existing
-- hooks.
--
-- 'heartbeatTimeout', 'putLifecycleHook_heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. The range is from @30@ to @7200@ seconds. The default value
-- is @3600@ seconds (1 hour).
--
-- 'lifecycleHookName', 'putLifecycleHook_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'autoScalingGroupName', 'putLifecycleHook_autoScalingGroupName' - The name of the Auto Scaling group.
newPutLifecycleHook ::
  -- | 'lifecycleHookName'
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  PutLifecycleHook
newPutLifecycleHook
  pLifecycleHookName_
  pAutoScalingGroupName_ =
    PutLifecycleHook'
      { roleARN = Prelude.Nothing,
        notificationMetadata = Prelude.Nothing,
        defaultResult = Prelude.Nothing,
        notificationTargetARN = Prelude.Nothing,
        lifecycleTransition = Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        autoScalingGroupName = pAutoScalingGroupName_
      }

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
--
-- Valid only if the notification target is an Amazon SNS topic or an
-- Amazon SQS queue. Required for new lifecycle hooks, but optional when
-- updating existing hooks.
putLifecycleHook_roleARN :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Text)
putLifecycleHook_roleARN = Lens.lens (\PutLifecycleHook' {roleARN} -> roleARN) (\s@PutLifecycleHook' {} a -> s {roleARN = a} :: PutLifecycleHook)

-- | Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
putLifecycleHook_notificationMetadata :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Text)
putLifecycleHook_notificationMetadata = Lens.lens (\PutLifecycleHook' {notificationMetadata} -> notificationMetadata) (\s@PutLifecycleHook' {} a -> s {notificationMetadata = a} :: PutLifecycleHook)

-- | The action the Auto Scaling group takes when the lifecycle hook timeout
-- elapses or if an unexpected failure occurs. The default value is
-- @ABANDON@.
--
-- Valid values: @CONTINUE@ | @ABANDON@
putLifecycleHook_defaultResult :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Text)
putLifecycleHook_defaultResult = Lens.lens (\PutLifecycleHook' {defaultResult} -> defaultResult) (\s@PutLifecycleHook' {} a -> s {defaultResult = a} :: PutLifecycleHook)

-- | The Amazon Resource Name (ARN) of the notification target that Amazon
-- EC2 Auto Scaling uses to notify you when an instance is in a wait state
-- for the lifecycle hook. You can specify either an Amazon SNS topic or an
-- Amazon SQS queue.
--
-- If you specify an empty string, this overrides the current ARN.
--
-- This operation uses the JSON format when sending notifications to an
-- Amazon SQS queue, and an email key-value pair format when sending
-- notifications to an Amazon SNS topic.
--
-- When you specify a notification target, Amazon EC2 Auto Scaling sends it
-- a test message. Test messages contain the following additional key-value
-- pair: @\"Event\": \"autoscaling:TEST_NOTIFICATION\"@.
putLifecycleHook_notificationTargetARN :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Text)
putLifecycleHook_notificationTargetARN = Lens.lens (\PutLifecycleHook' {notificationTargetARN} -> notificationTargetARN) (\s@PutLifecycleHook' {} a -> s {notificationTargetARN = a} :: PutLifecycleHook)

-- | The lifecycle transition. For Auto Scaling groups, there are two major
-- lifecycle transitions.
--
-- -   To create a lifecycle hook for scale-out events, specify
--     @autoscaling:EC2_INSTANCE_LAUNCHING@.
--
-- -   To create a lifecycle hook for scale-in events, specify
--     @autoscaling:EC2_INSTANCE_TERMINATING@.
--
-- Required for new lifecycle hooks, but optional when updating existing
-- hooks.
putLifecycleHook_lifecycleTransition :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Text)
putLifecycleHook_lifecycleTransition = Lens.lens (\PutLifecycleHook' {lifecycleTransition} -> lifecycleTransition) (\s@PutLifecycleHook' {} a -> s {lifecycleTransition = a} :: PutLifecycleHook)

-- | The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. The range is from @30@ to @7200@ seconds. The default value
-- is @3600@ seconds (1 hour).
putLifecycleHook_heartbeatTimeout :: Lens.Lens' PutLifecycleHook (Prelude.Maybe Prelude.Int)
putLifecycleHook_heartbeatTimeout = Lens.lens (\PutLifecycleHook' {heartbeatTimeout} -> heartbeatTimeout) (\s@PutLifecycleHook' {} a -> s {heartbeatTimeout = a} :: PutLifecycleHook)

-- | The name of the lifecycle hook.
putLifecycleHook_lifecycleHookName :: Lens.Lens' PutLifecycleHook Prelude.Text
putLifecycleHook_lifecycleHookName = Lens.lens (\PutLifecycleHook' {lifecycleHookName} -> lifecycleHookName) (\s@PutLifecycleHook' {} a -> s {lifecycleHookName = a} :: PutLifecycleHook)

-- | The name of the Auto Scaling group.
putLifecycleHook_autoScalingGroupName :: Lens.Lens' PutLifecycleHook Prelude.Text
putLifecycleHook_autoScalingGroupName = Lens.lens (\PutLifecycleHook' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutLifecycleHook' {} a -> s {autoScalingGroupName = a} :: PutLifecycleHook)

instance Core.AWSRequest PutLifecycleHook where
  type
    AWSResponse PutLifecycleHook =
      PutLifecycleHookResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutLifecycleHookResult"
      ( \s h x ->
          PutLifecycleHookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLifecycleHook where
  hashWithSalt _salt PutLifecycleHook' {..} =
    _salt `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` notificationMetadata
      `Prelude.hashWithSalt` defaultResult
      `Prelude.hashWithSalt` notificationTargetARN
      `Prelude.hashWithSalt` lifecycleTransition
      `Prelude.hashWithSalt` heartbeatTimeout
      `Prelude.hashWithSalt` lifecycleHookName
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData PutLifecycleHook where
  rnf PutLifecycleHook' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf notificationMetadata
      `Prelude.seq` Prelude.rnf defaultResult
      `Prelude.seq` Prelude.rnf notificationTargetARN
      `Prelude.seq` Prelude.rnf lifecycleTransition
      `Prelude.seq` Prelude.rnf heartbeatTimeout
      `Prelude.seq` Prelude.rnf lifecycleHookName
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders PutLifecycleHook where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutLifecycleHook where
  toPath = Prelude.const "/"

instance Data.ToQuery PutLifecycleHook where
  toQuery PutLifecycleHook' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutLifecycleHook" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "RoleARN" Data.=: roleARN,
        "NotificationMetadata" Data.=: notificationMetadata,
        "DefaultResult" Data.=: defaultResult,
        "NotificationTargetARN"
          Data.=: notificationTargetARN,
        "LifecycleTransition" Data.=: lifecycleTransition,
        "HeartbeatTimeout" Data.=: heartbeatTimeout,
        "LifecycleHookName" Data.=: lifecycleHookName,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newPutLifecycleHookResponse' smart constructor.
data PutLifecycleHookResponse = PutLifecycleHookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLifecycleHookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putLifecycleHookResponse_httpStatus' - The response's http status code.
newPutLifecycleHookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLifecycleHookResponse
newPutLifecycleHookResponse pHttpStatus_ =
  PutLifecycleHookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putLifecycleHookResponse_httpStatus :: Lens.Lens' PutLifecycleHookResponse Prelude.Int
putLifecycleHookResponse_httpStatus = Lens.lens (\PutLifecycleHookResponse' {httpStatus} -> httpStatus) (\s@PutLifecycleHookResponse' {} a -> s {httpStatus = a} :: PutLifecycleHookResponse)

instance Prelude.NFData PutLifecycleHookResponse where
  rnf PutLifecycleHookResponse' {..} =
    Prelude.rnf httpStatus
