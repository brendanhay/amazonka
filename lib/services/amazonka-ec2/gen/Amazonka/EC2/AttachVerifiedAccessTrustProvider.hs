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
-- Module      : Amazonka.EC2.AttachVerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A trust provider is a third-party entity that creates, maintains, and
-- manages identity information for users and devices. One or more trust
-- providers can be attached to an Amazon Web Services Verified Access
-- instance.
module Amazonka.EC2.AttachVerifiedAccessTrustProvider
  ( -- * Creating a Request
    AttachVerifiedAccessTrustProvider (..),
    newAttachVerifiedAccessTrustProvider,

    -- * Request Lenses
    attachVerifiedAccessTrustProvider_clientToken,
    attachVerifiedAccessTrustProvider_dryRun,
    attachVerifiedAccessTrustProvider_verifiedAccessInstanceId,
    attachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- * Destructuring the Response
    AttachVerifiedAccessTrustProviderResponse (..),
    newAttachVerifiedAccessTrustProviderResponse,

    -- * Response Lenses
    attachVerifiedAccessTrustProviderResponse_verifiedAccessInstance,
    attachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    attachVerifiedAccessTrustProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachVerifiedAccessTrustProvider' smart constructor.
data AttachVerifiedAccessTrustProvider = AttachVerifiedAccessTrustProvider'
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
-- Create a value of 'AttachVerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'attachVerifiedAccessTrustProvider_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'attachVerifiedAccessTrustProvider_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'attachVerifiedAccessTrustProvider_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessTrustProviderId', 'attachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId' - The ID of the Amazon Web Services Verified Access trust provider.
newAttachVerifiedAccessTrustProvider ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  -- | 'verifiedAccessTrustProviderId'
  Prelude.Text ->
  AttachVerifiedAccessTrustProvider
newAttachVerifiedAccessTrustProvider
  pVerifiedAccessInstanceId_
  pVerifiedAccessTrustProviderId_ =
    AttachVerifiedAccessTrustProvider'
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
attachVerifiedAccessTrustProvider_clientToken :: Lens.Lens' AttachVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
attachVerifiedAccessTrustProvider_clientToken = Lens.lens (\AttachVerifiedAccessTrustProvider' {clientToken} -> clientToken) (\s@AttachVerifiedAccessTrustProvider' {} a -> s {clientToken = a} :: AttachVerifiedAccessTrustProvider)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachVerifiedAccessTrustProvider_dryRun :: Lens.Lens' AttachVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Bool)
attachVerifiedAccessTrustProvider_dryRun = Lens.lens (\AttachVerifiedAccessTrustProvider' {dryRun} -> dryRun) (\s@AttachVerifiedAccessTrustProvider' {} a -> s {dryRun = a} :: AttachVerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access instance.
attachVerifiedAccessTrustProvider_verifiedAccessInstanceId :: Lens.Lens' AttachVerifiedAccessTrustProvider Prelude.Text
attachVerifiedAccessTrustProvider_verifiedAccessInstanceId = Lens.lens (\AttachVerifiedAccessTrustProvider' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@AttachVerifiedAccessTrustProvider' {} a -> s {verifiedAccessInstanceId = a} :: AttachVerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access trust provider.
attachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId :: Lens.Lens' AttachVerifiedAccessTrustProvider Prelude.Text
attachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId = Lens.lens (\AttachVerifiedAccessTrustProvider' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@AttachVerifiedAccessTrustProvider' {} a -> s {verifiedAccessTrustProviderId = a} :: AttachVerifiedAccessTrustProvider)

instance
  Core.AWSRequest
    AttachVerifiedAccessTrustProvider
  where
  type
    AWSResponse AttachVerifiedAccessTrustProvider =
      AttachVerifiedAccessTrustProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AttachVerifiedAccessTrustProviderResponse'
            Prelude.<$> (x Data..@? "verifiedAccessInstance")
              Prelude.<*> (x Data..@? "verifiedAccessTrustProvider")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachVerifiedAccessTrustProvider
  where
  hashWithSalt
    _salt
    AttachVerifiedAccessTrustProvider' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` verifiedAccessInstanceId
        `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance
  Prelude.NFData
    AttachVerifiedAccessTrustProvider
  where
  rnf AttachVerifiedAccessTrustProvider' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderId

instance
  Data.ToHeaders
    AttachVerifiedAccessTrustProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AttachVerifiedAccessTrustProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AttachVerifiedAccessTrustProvider
  where
  toQuery AttachVerifiedAccessTrustProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AttachVerifiedAccessTrustProvider" ::
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

-- | /See:/ 'newAttachVerifiedAccessTrustProviderResponse' smart constructor.
data AttachVerifiedAccessTrustProviderResponse = AttachVerifiedAccessTrustProviderResponse'
  { -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstance :: Prelude.Maybe VerifiedAccessInstance,
    -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProvider :: Prelude.Maybe VerifiedAccessTrustProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachVerifiedAccessTrustProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessInstance', 'attachVerifiedAccessTrustProviderResponse_verifiedAccessInstance' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessTrustProvider', 'attachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider' - The ID of the Amazon Web Services Verified Access trust provider.
--
-- 'httpStatus', 'attachVerifiedAccessTrustProviderResponse_httpStatus' - The response's http status code.
newAttachVerifiedAccessTrustProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachVerifiedAccessTrustProviderResponse
newAttachVerifiedAccessTrustProviderResponse
  pHttpStatus_ =
    AttachVerifiedAccessTrustProviderResponse'
      { verifiedAccessInstance =
          Prelude.Nothing,
        verifiedAccessTrustProvider =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Amazon Web Services Verified Access instance.
attachVerifiedAccessTrustProviderResponse_verifiedAccessInstance :: Lens.Lens' AttachVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessInstance)
attachVerifiedAccessTrustProviderResponse_verifiedAccessInstance = Lens.lens (\AttachVerifiedAccessTrustProviderResponse' {verifiedAccessInstance} -> verifiedAccessInstance) (\s@AttachVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessInstance = a} :: AttachVerifiedAccessTrustProviderResponse)

-- | The ID of the Amazon Web Services Verified Access trust provider.
attachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider :: Lens.Lens' AttachVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessTrustProvider)
attachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider = Lens.lens (\AttachVerifiedAccessTrustProviderResponse' {verifiedAccessTrustProvider} -> verifiedAccessTrustProvider) (\s@AttachVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessTrustProvider = a} :: AttachVerifiedAccessTrustProviderResponse)

-- | The response's http status code.
attachVerifiedAccessTrustProviderResponse_httpStatus :: Lens.Lens' AttachVerifiedAccessTrustProviderResponse Prelude.Int
attachVerifiedAccessTrustProviderResponse_httpStatus = Lens.lens (\AttachVerifiedAccessTrustProviderResponse' {httpStatus} -> httpStatus) (\s@AttachVerifiedAccessTrustProviderResponse' {} a -> s {httpStatus = a} :: AttachVerifiedAccessTrustProviderResponse)

instance
  Prelude.NFData
    AttachVerifiedAccessTrustProviderResponse
  where
  rnf AttachVerifiedAccessTrustProviderResponse' {..} =
    Prelude.rnf verifiedAccessInstance
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProvider
      `Prelude.seq` Prelude.rnf httpStatus
