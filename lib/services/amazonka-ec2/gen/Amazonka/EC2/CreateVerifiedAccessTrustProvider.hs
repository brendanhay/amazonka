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
-- Module      : Amazonka.EC2.CreateVerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A trust provider is a third-party entity that creates, maintains, and
-- manages identity information for users and devices. When an application
-- request is made, the identity information sent by the trust provider
-- will be evaluated by Amazon Web Services Verified Access, before
-- allowing or denying the application request.
module Amazonka.EC2.CreateVerifiedAccessTrustProvider
  ( -- * Creating a Request
    CreateVerifiedAccessTrustProvider (..),
    newCreateVerifiedAccessTrustProvider,

    -- * Request Lenses
    createVerifiedAccessTrustProvider_clientToken,
    createVerifiedAccessTrustProvider_description,
    createVerifiedAccessTrustProvider_deviceOptions,
    createVerifiedAccessTrustProvider_deviceTrustProviderType,
    createVerifiedAccessTrustProvider_dryRun,
    createVerifiedAccessTrustProvider_oidcOptions,
    createVerifiedAccessTrustProvider_tagSpecifications,
    createVerifiedAccessTrustProvider_userTrustProviderType,
    createVerifiedAccessTrustProvider_trustProviderType,
    createVerifiedAccessTrustProvider_policyReferenceName,

    -- * Destructuring the Response
    CreateVerifiedAccessTrustProviderResponse (..),
    newCreateVerifiedAccessTrustProviderResponse,

    -- * Response Lenses
    createVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    createVerifiedAccessTrustProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVerifiedAccessTrustProvider' smart constructor.
data CreateVerifiedAccessTrustProvider = CreateVerifiedAccessTrustProvider'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access trust
    -- provider.
    description :: Prelude.Maybe Prelude.Text,
    -- | The options for device identity based trust providers.
    deviceOptions :: Prelude.Maybe CreateVerifiedAccessTrustProviderDeviceOptions,
    -- | The type of device-based trust provider.
    deviceTrustProviderType :: Prelude.Maybe DeviceTrustProviderType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The OpenID Connect details for an @oidc@-type, user-identity based trust
    -- provider.
    oidcOptions :: Prelude.Maybe CreateVerifiedAccessTrustProviderOidcOptions,
    -- | The tags to assign to the Amazon Web Services Verified Access trust
    -- provider.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The type of user-based trust provider.
    userTrustProviderType :: Prelude.Maybe UserTrustProviderType,
    -- | The type of trust provider can be either user or device-based.
    trustProviderType :: TrustProviderType,
    -- | The identifier to be used when working with policy rules.
    policyReferenceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVerifiedAccessTrustProvider_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createVerifiedAccessTrustProvider_description' - A description for the Amazon Web Services Verified Access trust
-- provider.
--
-- 'deviceOptions', 'createVerifiedAccessTrustProvider_deviceOptions' - The options for device identity based trust providers.
--
-- 'deviceTrustProviderType', 'createVerifiedAccessTrustProvider_deviceTrustProviderType' - The type of device-based trust provider.
--
-- 'dryRun', 'createVerifiedAccessTrustProvider_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'oidcOptions', 'createVerifiedAccessTrustProvider_oidcOptions' - The OpenID Connect details for an @oidc@-type, user-identity based trust
-- provider.
--
-- 'tagSpecifications', 'createVerifiedAccessTrustProvider_tagSpecifications' - The tags to assign to the Amazon Web Services Verified Access trust
-- provider.
--
-- 'userTrustProviderType', 'createVerifiedAccessTrustProvider_userTrustProviderType' - The type of user-based trust provider.
--
-- 'trustProviderType', 'createVerifiedAccessTrustProvider_trustProviderType' - The type of trust provider can be either user or device-based.
--
-- 'policyReferenceName', 'createVerifiedAccessTrustProvider_policyReferenceName' - The identifier to be used when working with policy rules.
newCreateVerifiedAccessTrustProvider ::
  -- | 'trustProviderType'
  TrustProviderType ->
  -- | 'policyReferenceName'
  Prelude.Text ->
  CreateVerifiedAccessTrustProvider
newCreateVerifiedAccessTrustProvider
  pTrustProviderType_
  pPolicyReferenceName_ =
    CreateVerifiedAccessTrustProvider'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        deviceOptions = Prelude.Nothing,
        deviceTrustProviderType =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        oidcOptions = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        userTrustProviderType = Prelude.Nothing,
        trustProviderType = pTrustProviderType_,
        policyReferenceName =
          pPolicyReferenceName_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createVerifiedAccessTrustProvider_clientToken :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProvider_clientToken = Lens.lens (\CreateVerifiedAccessTrustProvider' {clientToken} -> clientToken) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {clientToken = a} :: CreateVerifiedAccessTrustProvider)

-- | A description for the Amazon Web Services Verified Access trust
-- provider.
createVerifiedAccessTrustProvider_description :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProvider_description = Lens.lens (\CreateVerifiedAccessTrustProvider' {description} -> description) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {description = a} :: CreateVerifiedAccessTrustProvider)

-- | The options for device identity based trust providers.
createVerifiedAccessTrustProvider_deviceOptions :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe CreateVerifiedAccessTrustProviderDeviceOptions)
createVerifiedAccessTrustProvider_deviceOptions = Lens.lens (\CreateVerifiedAccessTrustProvider' {deviceOptions} -> deviceOptions) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {deviceOptions = a} :: CreateVerifiedAccessTrustProvider)

-- | The type of device-based trust provider.
createVerifiedAccessTrustProvider_deviceTrustProviderType :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe DeviceTrustProviderType)
createVerifiedAccessTrustProvider_deviceTrustProviderType = Lens.lens (\CreateVerifiedAccessTrustProvider' {deviceTrustProviderType} -> deviceTrustProviderType) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {deviceTrustProviderType = a} :: CreateVerifiedAccessTrustProvider)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVerifiedAccessTrustProvider_dryRun :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe Prelude.Bool)
createVerifiedAccessTrustProvider_dryRun = Lens.lens (\CreateVerifiedAccessTrustProvider' {dryRun} -> dryRun) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {dryRun = a} :: CreateVerifiedAccessTrustProvider)

-- | The OpenID Connect details for an @oidc@-type, user-identity based trust
-- provider.
createVerifiedAccessTrustProvider_oidcOptions :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe CreateVerifiedAccessTrustProviderOidcOptions)
createVerifiedAccessTrustProvider_oidcOptions = Lens.lens (\CreateVerifiedAccessTrustProvider' {oidcOptions} -> oidcOptions) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {oidcOptions = a} :: CreateVerifiedAccessTrustProvider)

-- | The tags to assign to the Amazon Web Services Verified Access trust
-- provider.
createVerifiedAccessTrustProvider_tagSpecifications :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe [TagSpecification])
createVerifiedAccessTrustProvider_tagSpecifications = Lens.lens (\CreateVerifiedAccessTrustProvider' {tagSpecifications} -> tagSpecifications) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {tagSpecifications = a} :: CreateVerifiedAccessTrustProvider) Prelude.. Lens.mapping Lens.coerced

-- | The type of user-based trust provider.
createVerifiedAccessTrustProvider_userTrustProviderType :: Lens.Lens' CreateVerifiedAccessTrustProvider (Prelude.Maybe UserTrustProviderType)
createVerifiedAccessTrustProvider_userTrustProviderType = Lens.lens (\CreateVerifiedAccessTrustProvider' {userTrustProviderType} -> userTrustProviderType) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {userTrustProviderType = a} :: CreateVerifiedAccessTrustProvider)

-- | The type of trust provider can be either user or device-based.
createVerifiedAccessTrustProvider_trustProviderType :: Lens.Lens' CreateVerifiedAccessTrustProvider TrustProviderType
createVerifiedAccessTrustProvider_trustProviderType = Lens.lens (\CreateVerifiedAccessTrustProvider' {trustProviderType} -> trustProviderType) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {trustProviderType = a} :: CreateVerifiedAccessTrustProvider)

-- | The identifier to be used when working with policy rules.
createVerifiedAccessTrustProvider_policyReferenceName :: Lens.Lens' CreateVerifiedAccessTrustProvider Prelude.Text
createVerifiedAccessTrustProvider_policyReferenceName = Lens.lens (\CreateVerifiedAccessTrustProvider' {policyReferenceName} -> policyReferenceName) (\s@CreateVerifiedAccessTrustProvider' {} a -> s {policyReferenceName = a} :: CreateVerifiedAccessTrustProvider)

instance
  Core.AWSRequest
    CreateVerifiedAccessTrustProvider
  where
  type
    AWSResponse CreateVerifiedAccessTrustProvider =
      CreateVerifiedAccessTrustProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVerifiedAccessTrustProviderResponse'
            Prelude.<$> (x Data..@? "verifiedAccessTrustProvider")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVerifiedAccessTrustProvider
  where
  hashWithSalt
    _salt
    CreateVerifiedAccessTrustProvider' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` deviceOptions
        `Prelude.hashWithSalt` deviceTrustProviderType
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` oidcOptions
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` userTrustProviderType
        `Prelude.hashWithSalt` trustProviderType
        `Prelude.hashWithSalt` policyReferenceName

instance
  Prelude.NFData
    CreateVerifiedAccessTrustProvider
  where
  rnf CreateVerifiedAccessTrustProvider' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceOptions
      `Prelude.seq` Prelude.rnf deviceTrustProviderType
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf oidcOptions
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf userTrustProviderType
      `Prelude.seq` Prelude.rnf trustProviderType
      `Prelude.seq` Prelude.rnf policyReferenceName

instance
  Data.ToHeaders
    CreateVerifiedAccessTrustProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateVerifiedAccessTrustProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateVerifiedAccessTrustProvider
  where
  toQuery CreateVerifiedAccessTrustProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateVerifiedAccessTrustProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DeviceOptions" Data.=: deviceOptions,
        "DeviceTrustProviderType"
          Data.=: deviceTrustProviderType,
        "DryRun" Data.=: dryRun,
        "OidcOptions" Data.=: oidcOptions,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "UserTrustProviderType"
          Data.=: userTrustProviderType,
        "TrustProviderType" Data.=: trustProviderType,
        "PolicyReferenceName" Data.=: policyReferenceName
      ]

-- | /See:/ 'newCreateVerifiedAccessTrustProviderResponse' smart constructor.
data CreateVerifiedAccessTrustProviderResponse = CreateVerifiedAccessTrustProviderResponse'
  { -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProvider :: Prelude.Maybe VerifiedAccessTrustProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessTrustProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessTrustProvider', 'createVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider' - The ID of the Amazon Web Services Verified Access trust provider.
--
-- 'httpStatus', 'createVerifiedAccessTrustProviderResponse_httpStatus' - The response's http status code.
newCreateVerifiedAccessTrustProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVerifiedAccessTrustProviderResponse
newCreateVerifiedAccessTrustProviderResponse
  pHttpStatus_ =
    CreateVerifiedAccessTrustProviderResponse'
      { verifiedAccessTrustProvider =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Amazon Web Services Verified Access trust provider.
createVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider :: Lens.Lens' CreateVerifiedAccessTrustProviderResponse (Prelude.Maybe VerifiedAccessTrustProvider)
createVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider = Lens.lens (\CreateVerifiedAccessTrustProviderResponse' {verifiedAccessTrustProvider} -> verifiedAccessTrustProvider) (\s@CreateVerifiedAccessTrustProviderResponse' {} a -> s {verifiedAccessTrustProvider = a} :: CreateVerifiedAccessTrustProviderResponse)

-- | The response's http status code.
createVerifiedAccessTrustProviderResponse_httpStatus :: Lens.Lens' CreateVerifiedAccessTrustProviderResponse Prelude.Int
createVerifiedAccessTrustProviderResponse_httpStatus = Lens.lens (\CreateVerifiedAccessTrustProviderResponse' {httpStatus} -> httpStatus) (\s@CreateVerifiedAccessTrustProviderResponse' {} a -> s {httpStatus = a} :: CreateVerifiedAccessTrustProviderResponse)

instance
  Prelude.NFData
    CreateVerifiedAccessTrustProviderResponse
  where
  rnf CreateVerifiedAccessTrustProviderResponse' {..} =
    Prelude.rnf verifiedAccessTrustProvider
      `Prelude.seq` Prelude.rnf httpStatus
