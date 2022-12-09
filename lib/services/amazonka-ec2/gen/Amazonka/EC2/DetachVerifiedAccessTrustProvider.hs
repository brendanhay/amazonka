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
-- Module      : Amazonka.EC2.DetachVerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detach a trust provider from an Amazon Web Services Verified Access
-- instance.
module Amazonka.EC2.DetachVerifiedAccessTrustProvider
  ( -- * Creating a Request
    DetachVerifiedAccessTrustProvider (..),
    newDetachVerifiedAccessTrustProvider,

    -- * Request Lenses
    detachVerifiedAccessTrustProvider_clientToken,
    detachVerifiedAccessTrustProvider_dryRun,
    detachVerifiedAccessTrustProvider_verifiedAccessInstanceId,
    detachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- * Destructuring the Response
    DetachVerifiedAccessTrustProviderResponse (..),
    newDetachVerifiedAccessTrustProviderResponse,

    -- * Response Lenses
    detachVerifiedAccessTrustProviderResponse_verifiedAccessInstance,
    detachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    detachVerifiedAccessTrustProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachVerifiedAccessTrustProvider' smart constructor.
data DetachVerifiedAccessTrustProvider = DetachVerifiedAccessTrustProvider'
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
    verifiedAccessInstanceId :: Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProviderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachVerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'detachVerifiedAccessTrustProvider_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'detachVerifiedAccessTrustProvider_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'detachVerifiedAccessTrustProvider_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessTrustProviderId', 'detachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId' - The ID of the Amazon Web Services Verified Access trust provider.
newDetachVerifiedAccessTrustProvider ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  -- | 'verifiedAccessTrustProviderId'
  Prelude.Text ->
  DetachVerifiedAccessTrustProvider
newDetachVerifiedAccessTrustProvider
  pVerifiedAccessInstanceId_
  pVerifiedAccessTrustProviderId_ =
    DetachVerifiedAccessTrustProvider'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessInstanceId =
          pVerifiedAccessInstanceId_,
        verifiedAccessTrustProviderId =
          pVerifiedAccessTrustProviderId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
detachVerifiedAccessTrustProvider_clientToken :: Lens.Lens' DetachVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
detachVerifiedAccessTrustProvider_clientToken = Lens.lens (\DetachVerifiedAccessTrustProvider' {clientToken} -> clientToken) (\s@DetachVerifiedAccessTrustProvider' {} a -> s {clientToken = a} :: DetachVerifiedAccessTrustProvider)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachVerifiedAccessTrustProvider_dryRun :: Lens.Lens' DetachVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Bool)
detachVerifiedAccessTrustProvider_dryRun = Lens.lens (\DetachVerifiedAccessTrustProvider' {dryRun} -> dryRun) (\s@DetachVerifiedAccessTrustProvider' {} a -> s {dryRun = a} :: DetachVerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access instance.
detachVerifiedAccessTrustProvider_verifiedAccessInstanceId :: Lens.Lens' DetachVerifiedAccessTrustProvider Prelude.Text
detachVerifiedAccessTrustProvider_verifiedAccessInstanceId = Lens.lens (\DetachVerifiedAccessTrustProvider' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@DetachVerifiedAccessTrustProvider' {} a -> s {verifiedAccessInstanceId = a} :: DetachVerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access trust provider.
detachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId :: Lens.Lens' DetachVerifiedAccessTrustProvider Prelude.Text
detachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId = Lens.lens (\DetachVerifiedAccessTrustProvider' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@DetachVerifiedAccessTrustProvider' {} a -> s {verifiedAccessTrustProviderId = a} :: DetachVerifiedAccessTrustProvider)

instance
  Core.AWSRequest
    DetachVerifiedAccessTrustProvider
  where
  type
    AWSResponse DetachVerifiedAccessTrustProvider =
      DetachVerifiedAccessTrustProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DetachVerifiedAccessTrustProviderResponse'
            Prelude.<$> (x Data..@? "verifiedAccessInstance")
              Prelude.<*> (x Data..@? "verifiedAccessTrustProvider")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachVerifiedAccessTrustProvider
  where
  hashWithSalt
    _salt
    DetachVerifiedAccessTrustProvider' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` verifiedAccessInstanceId
        `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance
  Prelude.NFData
    DetachVerifiedAccessTrustProvider
  where
  rnf DetachVerifiedAccessTrustProvider' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderId

instance
  Data.ToHeaders
    DetachVerifiedAccessTrustProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DetachVerifiedAccessTrustProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DetachVerifiedAccessTrustProvider
  where
  toQuery DetachVerifiedAccessTrustProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DetachVerifiedAccessTrustProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId,
        "VerifiedAccessTrustProviderId"
          Data.=: verifiedAccessTrustProviderId
      ]

-- | /See:/ 'newDetachVerifiedAccessTrustProviderResponse' smart constructor.
data DetachVerifiedAccessTrustProviderResponse = DetachVerifiedAccessTrustProviderResponse'
  { -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstance :: Prelude.Maybe VerifiedAccessInstance,
    -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProvider :: Prelude.Maybe VerifiedAccessTrustProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachVerifiedAccessTrustProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessInstance', 'detachVerifiedAccessTrustProviderResponse_verifiedAccessInstance' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessTrustProvider', 'detachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider' - The ID of the Amazon Web Services Verified Access trust provider.
--
-- 'httpStatus', 'detachVerifiedAccessTrustProviderResponse_httpStatus' - The response's http status code.
newDetachVerifiedAccessTrustProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachVerifiedAccessTrustProviderResponse
newDetachVerifiedAccessTrustProviderResponse
  pHttpStatus_ =
    DetachVerifiedAccessTrustProviderResponse'
      { verifiedAccessInstance =
          Prelude.Nothing,
        verifiedAccessTrustProvider =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Amazon Web Services Verified Access instance.
detachVerifiedAccessTrustProviderResponse_verifiedAccessInstance :: Lens.Lens' DetachVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessInstance)
detachVerifiedAccessTrustProviderResponse_verifiedAccessInstance = Lens.lens (\DetachVerifiedAccessTrustProviderResponse' {verifiedAccessInstance} -> verifiedAccessInstance) (\s@DetachVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessInstance = a} :: DetachVerifiedAccessTrustProviderResponse)

-- | The ID of the Amazon Web Services Verified Access trust provider.
detachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider :: Lens.Lens' DetachVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessTrustProvider)
detachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider = Lens.lens (\DetachVerifiedAccessTrustProviderResponse' {verifiedAccessTrustProvider} -> verifiedAccessTrustProvider) (\s@DetachVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessTrustProvider = a} :: DetachVerifiedAccessTrustProviderResponse)

-- | The response's http status code.
detachVerifiedAccessTrustProviderResponse_httpStatus :: Lens.Lens' DetachVerifiedAccessTrustProviderResponse Prelude.Int
detachVerifiedAccessTrustProviderResponse_httpStatus = Lens.lens (\DetachVerifiedAccessTrustProviderResponse' {httpStatus} -> httpStatus) (\s@DetachVerifiedAccessTrustProviderResponse' {} a -> s {httpStatus = a} :: DetachVerifiedAccessTrustProviderResponse)

instance
  Prelude.NFData
    DetachVerifiedAccessTrustProviderResponse
  where
  rnf DetachVerifiedAccessTrustProviderResponse' {..} =
    Prelude.rnf verifiedAccessInstance
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProvider
      `Prelude.seq` Prelude.rnf httpStatus
