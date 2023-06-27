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
-- Module      : Amazonka.AutoScaling.CancelInstanceRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an instance refresh or rollback that is in progress. If an
-- instance refresh or rollback is not in progress, an
-- @ActiveInstanceRefreshNotFound@ error occurs.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html instance refresh feature>
-- in Amazon EC2 Auto Scaling, which helps you update instances in your
-- Auto Scaling group after you make configuration changes.
--
-- When you cancel an instance refresh, this does not roll back any changes
-- that it made. Use the RollbackInstanceRefresh API to roll back instead.
module Amazonka.AutoScaling.CancelInstanceRefresh
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelInstanceRefresh' smart constructor.
data CancelInstanceRefresh = CancelInstanceRefresh'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CancelInstanceRefresh where
  type
    AWSResponse CancelInstanceRefresh =
      CancelInstanceRefreshResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CancelInstanceRefreshResult"
      ( \s h x ->
          CancelInstanceRefreshResponse'
            Prelude.<$> (x Data..@? "InstanceRefreshId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelInstanceRefresh where
  hashWithSalt _salt CancelInstanceRefresh' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData CancelInstanceRefresh where
  rnf CancelInstanceRefresh' {..} =
    Prelude.rnf autoScalingGroupName

instance Data.ToHeaders CancelInstanceRefresh where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelInstanceRefresh where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelInstanceRefresh where
  toQuery CancelInstanceRefresh' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelInstanceRefresh" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newCancelInstanceRefreshResponse' smart constructor.
data CancelInstanceRefreshResponse = CancelInstanceRefreshResponse'
  { -- | The instance refresh ID associated with the request. This is the unique
    -- ID assigned to the instance refresh when it was started.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelInstanceRefreshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRefreshId', 'cancelInstanceRefreshResponse_instanceRefreshId' - The instance refresh ID associated with the request. This is the unique
-- ID assigned to the instance refresh when it was started.
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

-- | The instance refresh ID associated with the request. This is the unique
-- ID assigned to the instance refresh when it was started.
cancelInstanceRefreshResponse_instanceRefreshId :: Lens.Lens' CancelInstanceRefreshResponse (Prelude.Maybe Prelude.Text)
cancelInstanceRefreshResponse_instanceRefreshId = Lens.lens (\CancelInstanceRefreshResponse' {instanceRefreshId} -> instanceRefreshId) (\s@CancelInstanceRefreshResponse' {} a -> s {instanceRefreshId = a} :: CancelInstanceRefreshResponse)

-- | The response's http status code.
cancelInstanceRefreshResponse_httpStatus :: Lens.Lens' CancelInstanceRefreshResponse Prelude.Int
cancelInstanceRefreshResponse_httpStatus = Lens.lens (\CancelInstanceRefreshResponse' {httpStatus} -> httpStatus) (\s@CancelInstanceRefreshResponse' {} a -> s {httpStatus = a} :: CancelInstanceRefreshResponse)

instance Prelude.NFData CancelInstanceRefreshResponse where
  rnf CancelInstanceRefreshResponse' {..} =
    Prelude.rnf instanceRefreshId
      `Prelude.seq` Prelude.rnf httpStatus
