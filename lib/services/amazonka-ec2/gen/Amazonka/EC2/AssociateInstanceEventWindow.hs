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
-- Module      : Amazonka.EC2.AssociateInstanceEventWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.AssociateInstanceEventWindow
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateInstanceEventWindowResponse'
            Prelude.<$> (x Data..@? "instanceEventWindow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateInstanceEventWindow
  where
  hashWithSalt _salt AssociateInstanceEventWindow' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceEventWindowId
      `Prelude.hashWithSalt` associationTarget

instance Prelude.NFData AssociateInstanceEventWindow where
  rnf AssociateInstanceEventWindow' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf instanceEventWindowId `Prelude.seq`
        Prelude.rnf associationTarget

instance Data.ToHeaders AssociateInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateInstanceEventWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateInstanceEventWindow where
  toQuery AssociateInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateInstanceEventWindow" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceEventWindowId"
          Data.=: instanceEventWindowId,
        "AssociationTarget" Data.=: associationTarget
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
  where
  rnf AssociateInstanceEventWindowResponse' {..} =
    Prelude.rnf instanceEventWindow `Prelude.seq`
      Prelude.rnf httpStatus
