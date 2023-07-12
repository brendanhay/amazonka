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
-- Module      : Amazonka.EC2.DeleteVerifiedAccessEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Amazon Web Services Verified Access endpoint.
module Amazonka.EC2.DeleteVerifiedAccessEndpoint
  ( -- * Creating a Request
    DeleteVerifiedAccessEndpoint (..),
    newDeleteVerifiedAccessEndpoint,

    -- * Request Lenses
    deleteVerifiedAccessEndpoint_clientToken,
    deleteVerifiedAccessEndpoint_dryRun,
    deleteVerifiedAccessEndpoint_verifiedAccessEndpointId,

    -- * Destructuring the Response
    DeleteVerifiedAccessEndpointResponse (..),
    newDeleteVerifiedAccessEndpointResponse,

    -- * Response Lenses
    deleteVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    deleteVerifiedAccessEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVerifiedAccessEndpoint' smart constructor.
data DeleteVerifiedAccessEndpoint = DeleteVerifiedAccessEndpoint'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteVerifiedAccessEndpoint_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'deleteVerifiedAccessEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessEndpointId', 'deleteVerifiedAccessEndpoint_verifiedAccessEndpointId' - The ID of the Amazon Web Services Verified Access endpoint.
newDeleteVerifiedAccessEndpoint ::
  -- | 'verifiedAccessEndpointId'
  Prelude.Text ->
  DeleteVerifiedAccessEndpoint
newDeleteVerifiedAccessEndpoint
  pVerifiedAccessEndpointId_ =
    DeleteVerifiedAccessEndpoint'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessEndpointId =
          pVerifiedAccessEndpointId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
deleteVerifiedAccessEndpoint_clientToken :: Lens.Lens' DeleteVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
deleteVerifiedAccessEndpoint_clientToken = Lens.lens (\DeleteVerifiedAccessEndpoint' {clientToken} -> clientToken) (\s@DeleteVerifiedAccessEndpoint' {} a -> s {clientToken = a} :: DeleteVerifiedAccessEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVerifiedAccessEndpoint_dryRun :: Lens.Lens' DeleteVerifiedAccessEndpoint (Prelude.Maybe Prelude.Bool)
deleteVerifiedAccessEndpoint_dryRun = Lens.lens (\DeleteVerifiedAccessEndpoint' {dryRun} -> dryRun) (\s@DeleteVerifiedAccessEndpoint' {} a -> s {dryRun = a} :: DeleteVerifiedAccessEndpoint)

-- | The ID of the Amazon Web Services Verified Access endpoint.
deleteVerifiedAccessEndpoint_verifiedAccessEndpointId :: Lens.Lens' DeleteVerifiedAccessEndpoint Prelude.Text
deleteVerifiedAccessEndpoint_verifiedAccessEndpointId = Lens.lens (\DeleteVerifiedAccessEndpoint' {verifiedAccessEndpointId} -> verifiedAccessEndpointId) (\s@DeleteVerifiedAccessEndpoint' {} a -> s {verifiedAccessEndpointId = a} :: DeleteVerifiedAccessEndpoint)

instance Core.AWSRequest DeleteVerifiedAccessEndpoint where
  type
    AWSResponse DeleteVerifiedAccessEndpoint =
      DeleteVerifiedAccessEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVerifiedAccessEndpointResponse'
            Prelude.<$> (x Data..@? "verifiedAccessEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVerifiedAccessEndpoint
  where
  hashWithSalt _salt DeleteVerifiedAccessEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessEndpointId

instance Prelude.NFData DeleteVerifiedAccessEndpoint where
  rnf DeleteVerifiedAccessEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessEndpointId

instance Data.ToHeaders DeleteVerifiedAccessEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVerifiedAccessEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVerifiedAccessEndpoint where
  toQuery DeleteVerifiedAccessEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteVerifiedAccessEndpoint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessEndpointId"
          Data.=: verifiedAccessEndpointId
      ]

-- | /See:/ 'newDeleteVerifiedAccessEndpointResponse' smart constructor.
data DeleteVerifiedAccessEndpointResponse = DeleteVerifiedAccessEndpointResponse'
  { -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpoint :: Prelude.Maybe VerifiedAccessEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessEndpoint', 'deleteVerifiedAccessEndpointResponse_verifiedAccessEndpoint' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'httpStatus', 'deleteVerifiedAccessEndpointResponse_httpStatus' - The response's http status code.
newDeleteVerifiedAccessEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVerifiedAccessEndpointResponse
newDeleteVerifiedAccessEndpointResponse pHttpStatus_ =
  DeleteVerifiedAccessEndpointResponse'
    { verifiedAccessEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access endpoint.
deleteVerifiedAccessEndpointResponse_verifiedAccessEndpoint :: Lens.Lens' DeleteVerifiedAccessEndpointResponse (Prelude.Maybe VerifiedAccessEndpoint)
deleteVerifiedAccessEndpointResponse_verifiedAccessEndpoint = Lens.lens (\DeleteVerifiedAccessEndpointResponse' {verifiedAccessEndpoint} -> verifiedAccessEndpoint) (\s@DeleteVerifiedAccessEndpointResponse' {} a -> s {verifiedAccessEndpoint = a} :: DeleteVerifiedAccessEndpointResponse)

-- | The response's http status code.
deleteVerifiedAccessEndpointResponse_httpStatus :: Lens.Lens' DeleteVerifiedAccessEndpointResponse Prelude.Int
deleteVerifiedAccessEndpointResponse_httpStatus = Lens.lens (\DeleteVerifiedAccessEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteVerifiedAccessEndpointResponse' {} a -> s {httpStatus = a} :: DeleteVerifiedAccessEndpointResponse)

instance
  Prelude.NFData
    DeleteVerifiedAccessEndpointResponse
  where
  rnf DeleteVerifiedAccessEndpointResponse' {..} =
    Prelude.rnf verifiedAccessEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
