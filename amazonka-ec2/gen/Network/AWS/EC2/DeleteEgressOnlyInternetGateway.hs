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
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
  ( -- * Creating a Request
    DeleteEgressOnlyInternetGateway (..),
    newDeleteEgressOnlyInternetGateway,

    -- * Request Lenses
    deleteEgressOnlyInternetGateway_dryRun,
    deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId,

    -- * Destructuring the Response
    DeleteEgressOnlyInternetGatewayResponse (..),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- * Response Lenses
    deleteEgressOnlyInternetGatewayResponse_returnCode,
    deleteEgressOnlyInternetGatewayResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEgressOnlyInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteEgressOnlyInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'egressOnlyInternetGatewayId', 'deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
newDeleteEgressOnlyInternetGateway ::
  -- | 'egressOnlyInternetGatewayId'
  Prelude.Text ->
  DeleteEgressOnlyInternetGateway
newDeleteEgressOnlyInternetGateway
  pEgressOnlyInternetGatewayId_ =
    DeleteEgressOnlyInternetGateway'
      { dryRun =
          Prelude.Nothing,
        egressOnlyInternetGatewayId =
          pEgressOnlyInternetGatewayId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteEgressOnlyInternetGateway_dryRun :: Lens.Lens' DeleteEgressOnlyInternetGateway (Prelude.Maybe Prelude.Bool)
deleteEgressOnlyInternetGateway_dryRun = Lens.lens (\DeleteEgressOnlyInternetGateway' {dryRun} -> dryRun) (\s@DeleteEgressOnlyInternetGateway' {} a -> s {dryRun = a} :: DeleteEgressOnlyInternetGateway)

-- | The ID of the egress-only internet gateway.
deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId :: Lens.Lens' DeleteEgressOnlyInternetGateway Prelude.Text
deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId = Lens.lens (\DeleteEgressOnlyInternetGateway' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@DeleteEgressOnlyInternetGateway' {} a -> s {egressOnlyInternetGatewayId = a} :: DeleteEgressOnlyInternetGateway)

instance
  Prelude.AWSRequest
    DeleteEgressOnlyInternetGateway
  where
  type
    Rs DeleteEgressOnlyInternetGateway =
      DeleteEgressOnlyInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteEgressOnlyInternetGatewayResponse'
            Prelude.<$> (x Prelude..@? "returnCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteEgressOnlyInternetGateway

instance
  Prelude.NFData
    DeleteEgressOnlyInternetGateway

instance
  Prelude.ToHeaders
    DeleteEgressOnlyInternetGateway
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteEgressOnlyInternetGateway
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteEgressOnlyInternetGateway
  where
  toQuery DeleteEgressOnlyInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteEgressOnlyInternetGateway" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "EgressOnlyInternetGatewayId"
          Prelude.=: egressOnlyInternetGatewayId
      ]

-- | /See:/ 'newDeleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnCode :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEgressOnlyInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnCode', 'deleteEgressOnlyInternetGatewayResponse_returnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'deleteEgressOnlyInternetGatewayResponse_httpStatus' - The response's http status code.
newDeleteEgressOnlyInternetGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEgressOnlyInternetGatewayResponse
newDeleteEgressOnlyInternetGatewayResponse
  pHttpStatus_ =
    DeleteEgressOnlyInternetGatewayResponse'
      { returnCode =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
deleteEgressOnlyInternetGatewayResponse_returnCode :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse (Prelude.Maybe Prelude.Bool)
deleteEgressOnlyInternetGatewayResponse_returnCode = Lens.lens (\DeleteEgressOnlyInternetGatewayResponse' {returnCode} -> returnCode) (\s@DeleteEgressOnlyInternetGatewayResponse' {} a -> s {returnCode = a} :: DeleteEgressOnlyInternetGatewayResponse)

-- | The response's http status code.
deleteEgressOnlyInternetGatewayResponse_httpStatus :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse Prelude.Int
deleteEgressOnlyInternetGatewayResponse_httpStatus = Lens.lens (\DeleteEgressOnlyInternetGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteEgressOnlyInternetGatewayResponse' {} a -> s {httpStatus = a} :: DeleteEgressOnlyInternetGatewayResponse)

instance
  Prelude.NFData
    DeleteEgressOnlyInternetGatewayResponse
