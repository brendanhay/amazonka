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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of the specified Amazon Web Services Verified
-- Access trust provider.
module Amazonka.EC2.ModifyVerifiedAccessTrustProvider
  ( -- * Creating a Request
    ModifyVerifiedAccessTrustProvider (..),
    newModifyVerifiedAccessTrustProvider,

    -- * Request Lenses
    modifyVerifiedAccessTrustProvider_clientToken,
    modifyVerifiedAccessTrustProvider_description,
    modifyVerifiedAccessTrustProvider_dryRun,
    modifyVerifiedAccessTrustProvider_oidcOptions,
    modifyVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- * Destructuring the Response
    ModifyVerifiedAccessTrustProviderResponse (..),
    newModifyVerifiedAccessTrustProviderResponse,

    -- * Response Lenses
    modifyVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    modifyVerifiedAccessTrustProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessTrustProvider' smart constructor.
data ModifyVerifiedAccessTrustProvider = ModifyVerifiedAccessTrustProvider'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Verified Access trust provider.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The options for an OpenID Connect-compatible user-identity trust
    -- provider.
    oidcOptions :: Prelude.Maybe ModifyVerifiedAccessTrustProviderOidcOptions,
    -- | The ID of the Verified Access trust provider.
    verifiedAccessTrustProviderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessTrustProvider_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'modifyVerifiedAccessTrustProvider_description' - A description for the Verified Access trust provider.
--
-- 'dryRun', 'modifyVerifiedAccessTrustProvider_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'oidcOptions', 'modifyVerifiedAccessTrustProvider_oidcOptions' - The options for an OpenID Connect-compatible user-identity trust
-- provider.
--
-- 'verifiedAccessTrustProviderId', 'modifyVerifiedAccessTrustProvider_verifiedAccessTrustProviderId' - The ID of the Verified Access trust provider.
newModifyVerifiedAccessTrustProvider ::
  -- | 'verifiedAccessTrustProviderId'
  Prelude.Text ->
  ModifyVerifiedAccessTrustProvider
newModifyVerifiedAccessTrustProvider
  pVerifiedAccessTrustProviderId_ =
    ModifyVerifiedAccessTrustProvider'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        oidcOptions = Prelude.Nothing,
        verifiedAccessTrustProviderId =
          pVerifiedAccessTrustProviderId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessTrustProvider_clientToken :: Lens.Lens' ModifyVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProvider_clientToken = Lens.lens (\ModifyVerifiedAccessTrustProvider' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessTrustProvider' {} a -> s {clientToken = a} :: ModifyVerifiedAccessTrustProvider)

-- | A description for the Verified Access trust provider.
modifyVerifiedAccessTrustProvider_description :: Lens.Lens' ModifyVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProvider_description = Lens.lens (\ModifyVerifiedAccessTrustProvider' {description} -> description) (\s@ModifyVerifiedAccessTrustProvider' {} a -> s {description = a} :: ModifyVerifiedAccessTrustProvider)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessTrustProvider_dryRun :: Lens.Lens' ModifyVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessTrustProvider_dryRun = Lens.lens (\ModifyVerifiedAccessTrustProvider' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessTrustProvider' {} a -> s {dryRun = a} :: ModifyVerifiedAccessTrustProvider)

-- | The options for an OpenID Connect-compatible user-identity trust
-- provider.
modifyVerifiedAccessTrustProvider_oidcOptions :: Lens.Lens' ModifyVerifiedAccessTrustProvider (Prelude.Maybe ModifyVerifiedAccessTrustProviderOidcOptions)
modifyVerifiedAccessTrustProvider_oidcOptions = Lens.lens (\ModifyVerifiedAccessTrustProvider' {oidcOptions} -> oidcOptions) (\s@ModifyVerifiedAccessTrustProvider' {} a -> s {oidcOptions = a} :: ModifyVerifiedAccessTrustProvider)

-- | The ID of the Verified Access trust provider.
modifyVerifiedAccessTrustProvider_verifiedAccessTrustProviderId :: Lens.Lens' ModifyVerifiedAccessTrustProvider Prelude.Text
modifyVerifiedAccessTrustProvider_verifiedAccessTrustProviderId = Lens.lens (\ModifyVerifiedAccessTrustProvider' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@ModifyVerifiedAccessTrustProvider' {} a -> s {verifiedAccessTrustProviderId = a} :: ModifyVerifiedAccessTrustProvider)

instance
  Core.AWSRequest
    ModifyVerifiedAccessTrustProvider
  where
  type
    AWSResponse ModifyVerifiedAccessTrustProvider =
      ModifyVerifiedAccessTrustProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessTrustProviderResponse'
            Prelude.<$> (x Data..@? "verifiedAccessTrustProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessTrustProvider
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessTrustProvider' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` oidcOptions
        `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance
  Prelude.NFData
    ModifyVerifiedAccessTrustProvider
  where
  rnf ModifyVerifiedAccessTrustProvider' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf oidcOptions
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderId

instance
  Data.ToHeaders
    ModifyVerifiedAccessTrustProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVerifiedAccessTrustProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVerifiedAccessTrustProvider
  where
  toQuery ModifyVerifiedAccessTrustProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVerifiedAccessTrustProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "OidcOptions" Data.=: oidcOptions,
        "VerifiedAccessTrustProviderId"
          Data.=: verifiedAccessTrustProviderId
      ]

-- | /See:/ 'newModifyVerifiedAccessTrustProviderResponse' smart constructor.
data ModifyVerifiedAccessTrustProviderResponse = ModifyVerifiedAccessTrustProviderResponse'
  { -- | The ID of the Verified Access trust provider.
    verifiedAccessTrustProvider :: Prelude.Maybe VerifiedAccessTrustProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessTrustProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessTrustProvider', 'modifyVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider' - The ID of the Verified Access trust provider.
--
-- 'httpStatus', 'modifyVerifiedAccessTrustProviderResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessTrustProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessTrustProviderResponse
newModifyVerifiedAccessTrustProviderResponse
  pHttpStatus_ =
    ModifyVerifiedAccessTrustProviderResponse'
      { verifiedAccessTrustProvider =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Verified Access trust provider.
modifyVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider :: Lens.Lens' ModifyVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessTrustProvider)
modifyVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider = Lens.lens (\ModifyVerifiedAccessTrustProviderResponse' {verifiedAccessTrustProvider} -> verifiedAccessTrustProvider) (\s@ModifyVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessTrustProvider = a} :: ModifyVerifiedAccessTrustProviderResponse)

-- | The response's http status code.
modifyVerifiedAccessTrustProviderResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessTrustProviderResponse Prelude.Int
modifyVerifiedAccessTrustProviderResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessTrustProviderResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessTrustProviderResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessTrustProviderResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessTrustProviderResponse
  where
  rnf ModifyVerifiedAccessTrustProviderResponse' {..} =
    Prelude.rnf verifiedAccessTrustProvider
      `Prelude.seq` Prelude.rnf httpStatus
