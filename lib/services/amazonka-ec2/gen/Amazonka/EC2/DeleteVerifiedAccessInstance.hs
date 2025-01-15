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
-- Module      : Amazonka.EC2.DeleteVerifiedAccessInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Amazon Web Services Verified Access instance.
module Amazonka.EC2.DeleteVerifiedAccessInstance
  ( -- * Creating a Request
    DeleteVerifiedAccessInstance (..),
    newDeleteVerifiedAccessInstance,

    -- * Request Lenses
    deleteVerifiedAccessInstance_clientToken,
    deleteVerifiedAccessInstance_dryRun,
    deleteVerifiedAccessInstance_verifiedAccessInstanceId,

    -- * Destructuring the Response
    DeleteVerifiedAccessInstanceResponse (..),
    newDeleteVerifiedAccessInstanceResponse,

    -- * Response Lenses
    deleteVerifiedAccessInstanceResponse_verifiedAccessInstance,
    deleteVerifiedAccessInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVerifiedAccessInstance' smart constructor.
data DeleteVerifiedAccessInstance = DeleteVerifiedAccessInstance'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteVerifiedAccessInstance_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'deleteVerifiedAccessInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'deleteVerifiedAccessInstance_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newDeleteVerifiedAccessInstance ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  DeleteVerifiedAccessInstance
newDeleteVerifiedAccessInstance
  pVerifiedAccessInstanceId_ =
    DeleteVerifiedAccessInstance'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessInstanceId =
          pVerifiedAccessInstanceId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
deleteVerifiedAccessInstance_clientToken :: Lens.Lens' DeleteVerifiedAccessInstance (Prelude.Maybe Prelude.Text)
deleteVerifiedAccessInstance_clientToken = Lens.lens (\DeleteVerifiedAccessInstance' {clientToken} -> clientToken) (\s@DeleteVerifiedAccessInstance' {} a -> s {clientToken = a} :: DeleteVerifiedAccessInstance)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVerifiedAccessInstance_dryRun :: Lens.Lens' DeleteVerifiedAccessInstance (Prelude.Maybe Prelude.Bool)
deleteVerifiedAccessInstance_dryRun = Lens.lens (\DeleteVerifiedAccessInstance' {dryRun} -> dryRun) (\s@DeleteVerifiedAccessInstance' {} a -> s {dryRun = a} :: DeleteVerifiedAccessInstance)

-- | The ID of the Amazon Web Services Verified Access instance.
deleteVerifiedAccessInstance_verifiedAccessInstanceId :: Lens.Lens' DeleteVerifiedAccessInstance Prelude.Text
deleteVerifiedAccessInstance_verifiedAccessInstanceId = Lens.lens (\DeleteVerifiedAccessInstance' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@DeleteVerifiedAccessInstance' {} a -> s {verifiedAccessInstanceId = a} :: DeleteVerifiedAccessInstance)

instance Core.AWSRequest DeleteVerifiedAccessInstance where
  type
    AWSResponse DeleteVerifiedAccessInstance =
      DeleteVerifiedAccessInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVerifiedAccessInstanceResponse'
            Prelude.<$> (x Data..@? "verifiedAccessInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVerifiedAccessInstance
  where
  hashWithSalt _salt DeleteVerifiedAccessInstance' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData DeleteVerifiedAccessInstance where
  rnf DeleteVerifiedAccessInstance' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf verifiedAccessInstanceId

instance Data.ToHeaders DeleteVerifiedAccessInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVerifiedAccessInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVerifiedAccessInstance where
  toQuery DeleteVerifiedAccessInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteVerifiedAccessInstance" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId
      ]

-- | /See:/ 'newDeleteVerifiedAccessInstanceResponse' smart constructor.
data DeleteVerifiedAccessInstanceResponse = DeleteVerifiedAccessInstanceResponse'
  { -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstance :: Prelude.Maybe VerifiedAccessInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessInstance', 'deleteVerifiedAccessInstanceResponse_verifiedAccessInstance' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'httpStatus', 'deleteVerifiedAccessInstanceResponse_httpStatus' - The response's http status code.
newDeleteVerifiedAccessInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVerifiedAccessInstanceResponse
newDeleteVerifiedAccessInstanceResponse pHttpStatus_ =
  DeleteVerifiedAccessInstanceResponse'
    { verifiedAccessInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access instance.
deleteVerifiedAccessInstanceResponse_verifiedAccessInstance :: Lens.Lens' DeleteVerifiedAccessInstanceResponse (Prelude.Maybe VerifiedAccessInstance)
deleteVerifiedAccessInstanceResponse_verifiedAccessInstance = Lens.lens (\DeleteVerifiedAccessInstanceResponse' {verifiedAccessInstance} -> verifiedAccessInstance) (\s@DeleteVerifiedAccessInstanceResponse' {} a -> s {verifiedAccessInstance = a} :: DeleteVerifiedAccessInstanceResponse)

-- | The response's http status code.
deleteVerifiedAccessInstanceResponse_httpStatus :: Lens.Lens' DeleteVerifiedAccessInstanceResponse Prelude.Int
deleteVerifiedAccessInstanceResponse_httpStatus = Lens.lens (\DeleteVerifiedAccessInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteVerifiedAccessInstanceResponse' {} a -> s {httpStatus = a} :: DeleteVerifiedAccessInstanceResponse)

instance
  Prelude.NFData
    DeleteVerifiedAccessInstanceResponse
  where
  rnf DeleteVerifiedAccessInstanceResponse' {..} =
    Prelude.rnf verifiedAccessInstance `Prelude.seq`
      Prelude.rnf httpStatus
