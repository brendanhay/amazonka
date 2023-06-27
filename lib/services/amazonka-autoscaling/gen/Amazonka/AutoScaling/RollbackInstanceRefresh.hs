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
-- Module      : Amazonka.AutoScaling.RollbackInstanceRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an instance refresh that is in progress and rolls back any
-- changes that it made. Amazon EC2 Auto Scaling replaces any instances
-- that were replaced during the instance refresh. This restores your Auto
-- Scaling group to the configuration that it was using before the start of
-- the instance refresh.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html instance refresh feature>
-- in Amazon EC2 Auto Scaling, which helps you update instances in your
-- Auto Scaling group after you make configuration changes.
--
-- A rollback is not supported in the following situations:
--
-- -   There is no desired configuration specified for the instance
--     refresh.
--
-- -   The Auto Scaling group has a launch template that uses an Amazon Web
--     Services Systems Manager parameter instead of an AMI ID for the
--     @ImageId@ property.
--
-- -   The Auto Scaling group uses the launch template\'s @$Latest@ or
--     @$Default@ version.
--
-- When you receive a successful response from this operation, Amazon EC2
-- Auto Scaling immediately begins replacing instances. You can check the
-- status of this operation through the DescribeInstanceRefreshes API
-- operation.
module Amazonka.AutoScaling.RollbackInstanceRefresh
  ( -- * Creating a Request
    RollbackInstanceRefresh (..),
    newRollbackInstanceRefresh,

    -- * Request Lenses
    rollbackInstanceRefresh_autoScalingGroupName,

    -- * Destructuring the Response
    RollbackInstanceRefreshResponse (..),
    newRollbackInstanceRefreshResponse,

    -- * Response Lenses
    rollbackInstanceRefreshResponse_instanceRefreshId,
    rollbackInstanceRefreshResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRollbackInstanceRefresh' smart constructor.
data RollbackInstanceRefresh = RollbackInstanceRefresh'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackInstanceRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'rollbackInstanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
newRollbackInstanceRefresh ::
  RollbackInstanceRefresh
newRollbackInstanceRefresh =
  RollbackInstanceRefresh'
    { autoScalingGroupName =
        Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
rollbackInstanceRefresh_autoScalingGroupName :: Lens.Lens' RollbackInstanceRefresh (Prelude.Maybe Prelude.Text)
rollbackInstanceRefresh_autoScalingGroupName = Lens.lens (\RollbackInstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@RollbackInstanceRefresh' {} a -> s {autoScalingGroupName = a} :: RollbackInstanceRefresh)

instance Core.AWSRequest RollbackInstanceRefresh where
  type
    AWSResponse RollbackInstanceRefresh =
      RollbackInstanceRefreshResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RollbackInstanceRefreshResult"
      ( \s h x ->
          RollbackInstanceRefreshResponse'
            Prelude.<$> (x Data..@? "InstanceRefreshId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RollbackInstanceRefresh where
  hashWithSalt _salt RollbackInstanceRefresh' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData RollbackInstanceRefresh where
  rnf RollbackInstanceRefresh' {..} =
    Prelude.rnf autoScalingGroupName

instance Data.ToHeaders RollbackInstanceRefresh where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RollbackInstanceRefresh where
  toPath = Prelude.const "/"

instance Data.ToQuery RollbackInstanceRefresh where
  toQuery RollbackInstanceRefresh' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RollbackInstanceRefresh" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newRollbackInstanceRefreshResponse' smart constructor.
data RollbackInstanceRefreshResponse = RollbackInstanceRefreshResponse'
  { -- | The instance refresh ID associated with the request. This is the unique
    -- ID assigned to the instance refresh when it was started.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackInstanceRefreshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRefreshId', 'rollbackInstanceRefreshResponse_instanceRefreshId' - The instance refresh ID associated with the request. This is the unique
-- ID assigned to the instance refresh when it was started.
--
-- 'httpStatus', 'rollbackInstanceRefreshResponse_httpStatus' - The response's http status code.
newRollbackInstanceRefreshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RollbackInstanceRefreshResponse
newRollbackInstanceRefreshResponse pHttpStatus_ =
  RollbackInstanceRefreshResponse'
    { instanceRefreshId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance refresh ID associated with the request. This is the unique
-- ID assigned to the instance refresh when it was started.
rollbackInstanceRefreshResponse_instanceRefreshId :: Lens.Lens' RollbackInstanceRefreshResponse (Prelude.Maybe Prelude.Text)
rollbackInstanceRefreshResponse_instanceRefreshId = Lens.lens (\RollbackInstanceRefreshResponse' {instanceRefreshId} -> instanceRefreshId) (\s@RollbackInstanceRefreshResponse' {} a -> s {instanceRefreshId = a} :: RollbackInstanceRefreshResponse)

-- | The response's http status code.
rollbackInstanceRefreshResponse_httpStatus :: Lens.Lens' RollbackInstanceRefreshResponse Prelude.Int
rollbackInstanceRefreshResponse_httpStatus = Lens.lens (\RollbackInstanceRefreshResponse' {httpStatus} -> httpStatus) (\s@RollbackInstanceRefreshResponse' {} a -> s {httpStatus = a} :: RollbackInstanceRefreshResponse)

instance
  Prelude.NFData
    RollbackInstanceRefreshResponse
  where
  rnf RollbackInstanceRefreshResponse' {..} =
    Prelude.rnf instanceRefreshId
      `Prelude.seq` Prelude.rnf httpStatus
