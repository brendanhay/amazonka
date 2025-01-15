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
-- Module      : Amazonka.EC2.DeleteVerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Amazon Web Services Verified Access trust provider.
module Amazonka.EC2.DeleteVerifiedAccessTrustProvider
  ( -- * Creating a Request
    DeleteVerifiedAccessTrustProvider (..),
    newDeleteVerifiedAccessTrustProvider,

    -- * Request Lenses
    deleteVerifiedAccessTrustProvider_clientToken,
    deleteVerifiedAccessTrustProvider_dryRun,
    deleteVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- * Destructuring the Response
    DeleteVerifiedAccessTrustProviderResponse (..),
    newDeleteVerifiedAccessTrustProviderResponse,

    -- * Response Lenses
    deleteVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    deleteVerifiedAccessTrustProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVerifiedAccessTrustProvider' smart constructor.
data DeleteVerifiedAccessTrustProvider = DeleteVerifiedAccessTrustProvider'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProviderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteVerifiedAccessTrustProvider_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'deleteVerifiedAccessTrustProvider_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessTrustProviderId', 'deleteVerifiedAccessTrustProvider_verifiedAccessTrustProviderId' - The ID of the Amazon Web Services Verified Access trust provider.
newDeleteVerifiedAccessTrustProvider ::
  -- | 'verifiedAccessTrustProviderId'
  Prelude.Text ->
  DeleteVerifiedAccessTrustProvider
newDeleteVerifiedAccessTrustProvider
  pVerifiedAccessTrustProviderId_ =
    DeleteVerifiedAccessTrustProvider'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessTrustProviderId =
          pVerifiedAccessTrustProviderId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
deleteVerifiedAccessTrustProvider_clientToken :: Lens.Lens' DeleteVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
deleteVerifiedAccessTrustProvider_clientToken = Lens.lens (\DeleteVerifiedAccessTrustProvider' {clientToken} -> clientToken) (\s@DeleteVerifiedAccessTrustProvider' {} a -> s {clientToken = a} :: DeleteVerifiedAccessTrustProvider)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVerifiedAccessTrustProvider_dryRun :: Lens.Lens' DeleteVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Bool)
deleteVerifiedAccessTrustProvider_dryRun = Lens.lens (\DeleteVerifiedAccessTrustProvider' {dryRun} -> dryRun) (\s@DeleteVerifiedAccessTrustProvider' {} a -> s {dryRun = a} :: DeleteVerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access trust provider.
deleteVerifiedAccessTrustProvider_verifiedAccessTrustProviderId :: Lens.Lens' DeleteVerifiedAccessTrustProvider Prelude.Text
deleteVerifiedAccessTrustProvider_verifiedAccessTrustProviderId = Lens.lens (\DeleteVerifiedAccessTrustProvider' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@DeleteVerifiedAccessTrustProvider' {} a -> s {verifiedAccessTrustProviderId = a} :: DeleteVerifiedAccessTrustProvider)

instance
  Core.AWSRequest
    DeleteVerifiedAccessTrustProvider
  where
  type
    AWSResponse DeleteVerifiedAccessTrustProvider =
      DeleteVerifiedAccessTrustProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVerifiedAccessTrustProviderResponse'
            Prelude.<$> (x Data..@? "verifiedAccessTrustProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVerifiedAccessTrustProvider
  where
  hashWithSalt
    _salt
    DeleteVerifiedAccessTrustProvider' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance
  Prelude.NFData
    DeleteVerifiedAccessTrustProvider
  where
  rnf DeleteVerifiedAccessTrustProvider' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf verifiedAccessTrustProviderId

instance
  Data.ToHeaders
    DeleteVerifiedAccessTrustProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVerifiedAccessTrustProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteVerifiedAccessTrustProvider
  where
  toQuery DeleteVerifiedAccessTrustProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteVerifiedAccessTrustProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessTrustProviderId"
          Data.=: verifiedAccessTrustProviderId
      ]

-- | /See:/ 'newDeleteVerifiedAccessTrustProviderResponse' smart constructor.
data DeleteVerifiedAccessTrustProviderResponse = DeleteVerifiedAccessTrustProviderResponse'
  { -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProvider :: Prelude.Maybe VerifiedAccessTrustProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedAccessTrustProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessTrustProvider', 'deleteVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider' - The ID of the Amazon Web Services Verified Access trust provider.
--
-- 'httpStatus', 'deleteVerifiedAccessTrustProviderResponse_httpStatus' - The response's http status code.
newDeleteVerifiedAccessTrustProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVerifiedAccessTrustProviderResponse
newDeleteVerifiedAccessTrustProviderResponse
  pHttpStatus_ =
    DeleteVerifiedAccessTrustProviderResponse'
      { verifiedAccessTrustProvider =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Amazon Web Services Verified Access trust provider.
deleteVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider :: Lens.Lens' DeleteVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessTrustProvider)
deleteVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider = Lens.lens (\DeleteVerifiedAccessTrustProviderResponse' {verifiedAccessTrustProvider} -> verifiedAccessTrustProvider) (\s@DeleteVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessTrustProvider = a} :: DeleteVerifiedAccessTrustProviderResponse)

-- | The response's http status code.
deleteVerifiedAccessTrustProviderResponse_httpStatus :: Lens.Lens' DeleteVerifiedAccessTrustProviderResponse Prelude.Int
deleteVerifiedAccessTrustProviderResponse_httpStatus = Lens.lens (\DeleteVerifiedAccessTrustProviderResponse' {httpStatus} -> httpStatus) (\s@DeleteVerifiedAccessTrustProviderResponse' {} a -> s {httpStatus = a} :: DeleteVerifiedAccessTrustProviderResponse)

instance
  Prelude.NFData
    DeleteVerifiedAccessTrustProviderResponse
  where
  rnf DeleteVerifiedAccessTrustProviderResponse' {..} =
    Prelude.rnf verifiedAccessTrustProvider `Prelude.seq`
      Prelude.rnf httpStatus
