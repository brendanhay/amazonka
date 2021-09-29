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
-- Module      : Network.AWS.EC2.AssociateInstanceEventWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more targets with an event window. Only one type of
-- target (instance IDs, Dedicated Host IDs, or tags) can be specified with
-- an event window.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/event-windows.html Define event windows for scheduled events>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.AssociateInstanceEventWindow
  ( -- * Creating a Request
    AssociateInstanceEventWindow (..),
    newAssociateInstanceEventWindow,

    -- * Request Lenses
    associateInstanceEventWindow_dryRun,
    associateInstanceEventWindow_instanceEventWindowId,
    associateInstanceEventWindow_associationTarget,

    -- * Destructuring the Response
    AssociateInstanceEventWindowResponse (..),
    newAssociateInstanceEventWindowResponse,

    -- * Response Lenses
    associateInstanceEventWindowResponse_instanceEventWindow,
    associateInstanceEventWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateInstanceEventWindow' smart constructor.
data AssociateInstanceEventWindow = AssociateInstanceEventWindow'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the event window.
    instanceEventWindowId :: Prelude.Text,
    -- | One or more targets associated with the specified event window.
    associationTarget :: InstanceEventWindowAssociationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateInstanceEventWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateInstanceEventWindow_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceEventWindowId', 'associateInstanceEventWindow_instanceEventWindowId' - The ID of the event window.
--
-- 'associationTarget', 'associateInstanceEventWindow_associationTarget' - One or more targets associated with the specified event window.
newAssociateInstanceEventWindow ::
  -- | 'instanceEventWindowId'
  Prelude.Text ->
  -- | 'associationTarget'
  InstanceEventWindowAssociationRequest ->
  AssociateInstanceEventWindow
newAssociateInstanceEventWindow
  pInstanceEventWindowId_
  pAssociationTarget_ =
    AssociateInstanceEventWindow'
      { dryRun =
          Prelude.Nothing,
        instanceEventWindowId =
          pInstanceEventWindowId_,
        associationTarget = pAssociationTarget_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateInstanceEventWindow_dryRun :: Lens.Lens' AssociateInstanceEventWindow (Prelude.Maybe Prelude.Bool)
associateInstanceEventWindow_dryRun = Lens.lens (\AssociateInstanceEventWindow' {dryRun} -> dryRun) (\s@AssociateInstanceEventWindow' {} a -> s {dryRun = a} :: AssociateInstanceEventWindow)

-- | The ID of the event window.
associateInstanceEventWindow_instanceEventWindowId :: Lens.Lens' AssociateInstanceEventWindow Prelude.Text
associateInstanceEventWindow_instanceEventWindowId = Lens.lens (\AssociateInstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@AssociateInstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: AssociateInstanceEventWindow)

-- | One or more targets associated with the specified event window.
associateInstanceEventWindow_associationTarget :: Lens.Lens' AssociateInstanceEventWindow InstanceEventWindowAssociationRequest
associateInstanceEventWindow_associationTarget = Lens.lens (\AssociateInstanceEventWindow' {associationTarget} -> associationTarget) (\s@AssociateInstanceEventWindow' {} a -> s {associationTarget = a} :: AssociateInstanceEventWindow)

instance Core.AWSRequest AssociateInstanceEventWindow where
  type
    AWSResponse AssociateInstanceEventWindow =
      AssociateInstanceEventWindowResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateInstanceEventWindowResponse'
            Prelude.<$> (x Core..@? "instanceEventWindow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateInstanceEventWindow

instance Prelude.NFData AssociateInstanceEventWindow

instance Core.ToHeaders AssociateInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AssociateInstanceEventWindow where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateInstanceEventWindow where
  toQuery AssociateInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AssociateInstanceEventWindow" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceEventWindowId"
          Core.=: instanceEventWindowId,
        "AssociationTarget" Core.=: associationTarget
      ]

-- | /See:/ 'newAssociateInstanceEventWindowResponse' smart constructor.
data AssociateInstanceEventWindowResponse = AssociateInstanceEventWindowResponse'
  { -- | Information about the event window.
    instanceEventWindow :: Prelude.Maybe InstanceEventWindow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateInstanceEventWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceEventWindow', 'associateInstanceEventWindowResponse_instanceEventWindow' - Information about the event window.
--
-- 'httpStatus', 'associateInstanceEventWindowResponse_httpStatus' - The response's http status code.
newAssociateInstanceEventWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateInstanceEventWindowResponse
newAssociateInstanceEventWindowResponse pHttpStatus_ =
  AssociateInstanceEventWindowResponse'
    { instanceEventWindow =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the event window.
associateInstanceEventWindowResponse_instanceEventWindow :: Lens.Lens' AssociateInstanceEventWindowResponse (Prelude.Maybe InstanceEventWindow)
associateInstanceEventWindowResponse_instanceEventWindow = Lens.lens (\AssociateInstanceEventWindowResponse' {instanceEventWindow} -> instanceEventWindow) (\s@AssociateInstanceEventWindowResponse' {} a -> s {instanceEventWindow = a} :: AssociateInstanceEventWindowResponse)

-- | The response's http status code.
associateInstanceEventWindowResponse_httpStatus :: Lens.Lens' AssociateInstanceEventWindowResponse Prelude.Int
associateInstanceEventWindowResponse_httpStatus = Lens.lens (\AssociateInstanceEventWindowResponse' {httpStatus} -> httpStatus) (\s@AssociateInstanceEventWindowResponse' {} a -> s {httpStatus = a} :: AssociateInstanceEventWindowResponse)

instance
  Prelude.NFData
    AssociateInstanceEventWindowResponse
