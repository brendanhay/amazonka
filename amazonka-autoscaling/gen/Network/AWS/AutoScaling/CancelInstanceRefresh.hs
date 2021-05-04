{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.CancelInstanceRefresh
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an instance refresh operation in progress. Cancellation does not
-- roll back any replacements that have already been completed, but it
-- prevents new replacements from being started.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh>.
module Network.AWS.AutoScaling.CancelInstanceRefresh
  ( -- * Creating a Request
    CancelInstanceRefresh (..),
    newCancelInstanceRefresh,

    -- * Request Lenses
    cancelInstanceRefresh_autoScalingGroupName,

    -- * Destructuring the Response
    CancelInstanceRefreshResponse (..),
    newCancelInstanceRefreshResponse,

    -- * Response Lenses
    cancelInstanceRefreshResponse_instanceRefreshId,
    cancelInstanceRefreshResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelInstanceRefresh' smart constructor.
data CancelInstanceRefresh = CancelInstanceRefresh'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelInstanceRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'cancelInstanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
newCancelInstanceRefresh ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  CancelInstanceRefresh
newCancelInstanceRefresh pAutoScalingGroupName_ =
  CancelInstanceRefresh'
    { autoScalingGroupName =
        pAutoScalingGroupName_
    }

-- | The name of the Auto Scaling group.
cancelInstanceRefresh_autoScalingGroupName :: Lens.Lens' CancelInstanceRefresh Prelude.Text
cancelInstanceRefresh_autoScalingGroupName = Lens.lens (\CancelInstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@CancelInstanceRefresh' {} a -> s {autoScalingGroupName = a} :: CancelInstanceRefresh)

instance Prelude.AWSRequest CancelInstanceRefresh where
  type
    Rs CancelInstanceRefresh =
      CancelInstanceRefreshResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CancelInstanceRefreshResult"
      ( \s h x ->
          CancelInstanceRefreshResponse'
            Prelude.<$> (x Prelude..@? "InstanceRefreshId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelInstanceRefresh

instance Prelude.NFData CancelInstanceRefresh

instance Prelude.ToHeaders CancelInstanceRefresh where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelInstanceRefresh where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelInstanceRefresh where
  toQuery CancelInstanceRefresh' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelInstanceRefresh" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newCancelInstanceRefreshResponse' smart constructor.
data CancelInstanceRefreshResponse = CancelInstanceRefreshResponse'
  { -- | The instance refresh ID.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelInstanceRefreshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRefreshId', 'cancelInstanceRefreshResponse_instanceRefreshId' - The instance refresh ID.
--
-- 'httpStatus', 'cancelInstanceRefreshResponse_httpStatus' - The response's http status code.
newCancelInstanceRefreshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelInstanceRefreshResponse
newCancelInstanceRefreshResponse pHttpStatus_ =
  CancelInstanceRefreshResponse'
    { instanceRefreshId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance refresh ID.
cancelInstanceRefreshResponse_instanceRefreshId :: Lens.Lens' CancelInstanceRefreshResponse (Prelude.Maybe Prelude.Text)
cancelInstanceRefreshResponse_instanceRefreshId = Lens.lens (\CancelInstanceRefreshResponse' {instanceRefreshId} -> instanceRefreshId) (\s@CancelInstanceRefreshResponse' {} a -> s {instanceRefreshId = a} :: CancelInstanceRefreshResponse)

-- | The response's http status code.
cancelInstanceRefreshResponse_httpStatus :: Lens.Lens' CancelInstanceRefreshResponse Prelude.Int
cancelInstanceRefreshResponse_httpStatus = Lens.lens (\CancelInstanceRefreshResponse' {httpStatus} -> httpStatus) (\s@CancelInstanceRefreshResponse' {} a -> s {httpStatus = a} :: CancelInstanceRefreshResponse)

instance Prelude.NFData CancelInstanceRefreshResponse
