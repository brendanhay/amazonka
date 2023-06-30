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
-- Module      : Amazonka.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a heartbeat for the lifecycle action associated with the
-- specified token or instance. This extends the timeout by the length of
-- time defined using the PutLifecycleHook API call.
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
-- 5.  __If you need more time, record the lifecycle action heartbeat to
--     keep the instance in a wait state.__
--
-- 6.  If you finish before the timeout period ends, send a callback by
--     using the CompleteLifecycleAction API call.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.RecordLifecycleActionHeartbeat
  ( -- * Creating a Request
    RecordLifecycleActionHeartbeat (..),
    newRecordLifecycleActionHeartbeat,

    -- * Request Lenses
    recordLifecycleActionHeartbeat_instanceId,
    recordLifecycleActionHeartbeat_lifecycleActionToken,
    recordLifecycleActionHeartbeat_lifecycleHookName,
    recordLifecycleActionHeartbeat_autoScalingGroupName,

    -- * Destructuring the Response
    RecordLifecycleActionHeartbeatResponse (..),
    newRecordLifecycleActionHeartbeatResponse,

    -- * Response Lenses
    recordLifecycleActionHeartbeatResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRecordLifecycleActionHeartbeat' smart constructor.
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A token that uniquely identifies a specific lifecycle action associated
    -- with an instance. Amazon EC2 Auto Scaling sends this token to the
    -- notification target that you specified when you created the lifecycle
    -- hook.
    lifecycleActionToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordLifecycleActionHeartbeat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'recordLifecycleActionHeartbeat_instanceId' - The ID of the instance.
--
-- 'lifecycleActionToken', 'recordLifecycleActionHeartbeat_lifecycleActionToken' - A token that uniquely identifies a specific lifecycle action associated
-- with an instance. Amazon EC2 Auto Scaling sends this token to the
-- notification target that you specified when you created the lifecycle
-- hook.
--
-- 'lifecycleHookName', 'recordLifecycleActionHeartbeat_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'autoScalingGroupName', 'recordLifecycleActionHeartbeat_autoScalingGroupName' - The name of the Auto Scaling group.
newRecordLifecycleActionHeartbeat ::
  -- | 'lifecycleHookName'
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  RecordLifecycleActionHeartbeat
newRecordLifecycleActionHeartbeat
  pLifecycleHookName_
  pAutoScalingGroupName_ =
    RecordLifecycleActionHeartbeat'
      { instanceId =
          Prelude.Nothing,
        lifecycleActionToken = Prelude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        autoScalingGroupName =
          pAutoScalingGroupName_
      }

-- | The ID of the instance.
recordLifecycleActionHeartbeat_instanceId :: Lens.Lens' RecordLifecycleActionHeartbeat (Prelude.Maybe Prelude.Text)
recordLifecycleActionHeartbeat_instanceId = Lens.lens (\RecordLifecycleActionHeartbeat' {instanceId} -> instanceId) (\s@RecordLifecycleActionHeartbeat' {} a -> s {instanceId = a} :: RecordLifecycleActionHeartbeat)

-- | A token that uniquely identifies a specific lifecycle action associated
-- with an instance. Amazon EC2 Auto Scaling sends this token to the
-- notification target that you specified when you created the lifecycle
-- hook.
recordLifecycleActionHeartbeat_lifecycleActionToken :: Lens.Lens' RecordLifecycleActionHeartbeat (Prelude.Maybe Prelude.Text)
recordLifecycleActionHeartbeat_lifecycleActionToken = Lens.lens (\RecordLifecycleActionHeartbeat' {lifecycleActionToken} -> lifecycleActionToken) (\s@RecordLifecycleActionHeartbeat' {} a -> s {lifecycleActionToken = a} :: RecordLifecycleActionHeartbeat)

-- | The name of the lifecycle hook.
recordLifecycleActionHeartbeat_lifecycleHookName :: Lens.Lens' RecordLifecycleActionHeartbeat Prelude.Text
recordLifecycleActionHeartbeat_lifecycleHookName = Lens.lens (\RecordLifecycleActionHeartbeat' {lifecycleHookName} -> lifecycleHookName) (\s@RecordLifecycleActionHeartbeat' {} a -> s {lifecycleHookName = a} :: RecordLifecycleActionHeartbeat)

-- | The name of the Auto Scaling group.
recordLifecycleActionHeartbeat_autoScalingGroupName :: Lens.Lens' RecordLifecycleActionHeartbeat Prelude.Text
recordLifecycleActionHeartbeat_autoScalingGroupName = Lens.lens (\RecordLifecycleActionHeartbeat' {autoScalingGroupName} -> autoScalingGroupName) (\s@RecordLifecycleActionHeartbeat' {} a -> s {autoScalingGroupName = a} :: RecordLifecycleActionHeartbeat)

instance
  Core.AWSRequest
    RecordLifecycleActionHeartbeat
  where
  type
    AWSResponse RecordLifecycleActionHeartbeat =
      RecordLifecycleActionHeartbeatResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RecordLifecycleActionHeartbeatResult"
      ( \s h x ->
          RecordLifecycleActionHeartbeatResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RecordLifecycleActionHeartbeat
  where
  hashWithSalt
    _salt
    RecordLifecycleActionHeartbeat' {..} =
      _salt
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` lifecycleActionToken
        `Prelude.hashWithSalt` lifecycleHookName
        `Prelude.hashWithSalt` autoScalingGroupName

instance
  Prelude.NFData
    RecordLifecycleActionHeartbeat
  where
  rnf RecordLifecycleActionHeartbeat' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf lifecycleActionToken
      `Prelude.seq` Prelude.rnf lifecycleHookName
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance
  Data.ToHeaders
    RecordLifecycleActionHeartbeat
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RecordLifecycleActionHeartbeat where
  toPath = Prelude.const "/"

instance Data.ToQuery RecordLifecycleActionHeartbeat where
  toQuery RecordLifecycleActionHeartbeat' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RecordLifecycleActionHeartbeat" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceId" Data.=: instanceId,
        "LifecycleActionToken" Data.=: lifecycleActionToken,
        "LifecycleHookName" Data.=: lifecycleHookName,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newRecordLifecycleActionHeartbeatResponse' smart constructor.
data RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordLifecycleActionHeartbeatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'recordLifecycleActionHeartbeatResponse_httpStatus' - The response's http status code.
newRecordLifecycleActionHeartbeatResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RecordLifecycleActionHeartbeatResponse
newRecordLifecycleActionHeartbeatResponse
  pHttpStatus_ =
    RecordLifecycleActionHeartbeatResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
recordLifecycleActionHeartbeatResponse_httpStatus :: Lens.Lens' RecordLifecycleActionHeartbeatResponse Prelude.Int
recordLifecycleActionHeartbeatResponse_httpStatus = Lens.lens (\RecordLifecycleActionHeartbeatResponse' {httpStatus} -> httpStatus) (\s@RecordLifecycleActionHeartbeatResponse' {} a -> s {httpStatus = a} :: RecordLifecycleActionHeartbeatResponse)

instance
  Prelude.NFData
    RecordLifecycleActionHeartbeatResponse
  where
  rnf RecordLifecycleActionHeartbeatResponse' {..} =
    Prelude.rnf httpStatus
