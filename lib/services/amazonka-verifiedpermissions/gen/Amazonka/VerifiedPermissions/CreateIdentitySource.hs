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
-- Module      : Amazonka.VerifiedPermissions.CreateIdentitySource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference to an Amazon Cognito user pool as an external
-- identity provider (IdP).
--
-- After you create an identity source, you can use the identities provided
-- by the IdP as proxies for the principal in authorization queries that
-- use the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operation. These identities take the form of tokens that contain claims
-- about the user, such as IDs, attributes and group memberships. Amazon
-- Cognito provides both identity tokens and access tokens, and Verified
-- Permissions can use either or both. Any combination of identity and
-- access tokens results in the same Cedar principal. Verified Permissions
-- automatically translates the information about the identities into the
-- standard Cedar attributes that can be evaluated by your policies.
-- Because the Amazon Cognito identity and access tokens can contain
-- different information, the tokens you choose to use determine which
-- principal attributes are available to access when evaluating Cedar
-- policies.
--
-- If you delete a Amazon Cognito user pool or user, tokens from that
-- deleted pool or that deleted user continue to be usable until they
-- expire.
--
-- To reference a user from this identity source in your Cedar policies,
-- use the following syntax.
--
-- /IdentityType::\"\<CognitoUserPoolIdentifier>|\<CognitoClientId>/
--
-- Where @IdentityType@ is the string that you provide to the
-- @PrincipalEntityType@ parameter for this operation. The
-- @CognitoUserPoolId@ and @CognitoClientId@ are defined by the Amazon
-- Cognito user pool.
module Amazonka.VerifiedPermissions.CreateIdentitySource
  ( -- * Creating a Request
    CreateIdentitySource (..),
    newCreateIdentitySource,

    -- * Request Lenses
    createIdentitySource_clientToken,
    createIdentitySource_principalEntityType,
    createIdentitySource_policyStoreId,
    createIdentitySource_configuration,

    -- * Destructuring the Response
    CreateIdentitySourceResponse (..),
    newCreateIdentitySourceResponse,

    -- * Response Lenses
    createIdentitySourceResponse_httpStatus,
    createIdentitySourceResponse_createdDate,
    createIdentitySourceResponse_identitySourceId,
    createIdentitySourceResponse_lastUpdatedDate,
    createIdentitySourceResponse_policyStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newCreateIdentitySource' smart constructor.
data CreateIdentitySource = CreateIdentitySource'
  { -- | Specifies a unique, case-sensitive ID that you provide to ensure the
    -- idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the namespace and data type of the principals generated for
    -- identities authenticated by the new identity source.
    principalEntityType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the policy store in which you want to store this
    -- identity source. Only policies and requests made using this policy store
    -- can reference identities from the identity provider configured in the
    -- new identity source.
    policyStoreId :: Prelude.Text,
    -- | Specifies the details required to communicate with the identity provider
    -- (IdP) associated with this identity source.
    --
    -- At this time, the only valid member of this structure is a Amazon
    -- Cognito user pool configuration.
    --
    -- You must specify a @UserPoolArn@, and optionally, a @ClientId@.
    configuration :: Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIdentitySource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createIdentitySource_clientToken' - Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'principalEntityType', 'createIdentitySource_principalEntityType' - Specifies the namespace and data type of the principals generated for
-- identities authenticated by the new identity source.
--
-- 'policyStoreId', 'createIdentitySource_policyStoreId' - Specifies the ID of the policy store in which you want to store this
-- identity source. Only policies and requests made using this policy store
-- can reference identities from the identity provider configured in the
-- new identity source.
--
-- 'configuration', 'createIdentitySource_configuration' - Specifies the details required to communicate with the identity provider
-- (IdP) associated with this identity source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @UserPoolArn@, and optionally, a @ClientId@.
newCreateIdentitySource ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'configuration'
  Configuration ->
  CreateIdentitySource
newCreateIdentitySource
  pPolicyStoreId_
  pConfiguration_ =
    CreateIdentitySource'
      { clientToken =
          Prelude.Nothing,
        principalEntityType = Prelude.Nothing,
        policyStoreId = pPolicyStoreId_,
        configuration = pConfiguration_
      }

-- | Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
createIdentitySource_clientToken :: Lens.Lens' CreateIdentitySource (Prelude.Maybe Prelude.Text)
createIdentitySource_clientToken = Lens.lens (\CreateIdentitySource' {clientToken} -> clientToken) (\s@CreateIdentitySource' {} a -> s {clientToken = a} :: CreateIdentitySource)

-- | Specifies the namespace and data type of the principals generated for
-- identities authenticated by the new identity source.
createIdentitySource_principalEntityType :: Lens.Lens' CreateIdentitySource (Prelude.Maybe Prelude.Text)
createIdentitySource_principalEntityType = Lens.lens (\CreateIdentitySource' {principalEntityType} -> principalEntityType) (\s@CreateIdentitySource' {} a -> s {principalEntityType = a} :: CreateIdentitySource)

-- | Specifies the ID of the policy store in which you want to store this
-- identity source. Only policies and requests made using this policy store
-- can reference identities from the identity provider configured in the
-- new identity source.
createIdentitySource_policyStoreId :: Lens.Lens' CreateIdentitySource Prelude.Text
createIdentitySource_policyStoreId = Lens.lens (\CreateIdentitySource' {policyStoreId} -> policyStoreId) (\s@CreateIdentitySource' {} a -> s {policyStoreId = a} :: CreateIdentitySource)

-- | Specifies the details required to communicate with the identity provider
-- (IdP) associated with this identity source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @UserPoolArn@, and optionally, a @ClientId@.
createIdentitySource_configuration :: Lens.Lens' CreateIdentitySource Configuration
createIdentitySource_configuration = Lens.lens (\CreateIdentitySource' {configuration} -> configuration) (\s@CreateIdentitySource' {} a -> s {configuration = a} :: CreateIdentitySource)

instance Core.AWSRequest CreateIdentitySource where
  type
    AWSResponse CreateIdentitySource =
      CreateIdentitySourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIdentitySourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "identitySourceId")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
            Prelude.<*> (x Data..:> "policyStoreId")
      )

instance Prelude.Hashable CreateIdentitySource where
  hashWithSalt _salt CreateIdentitySource' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` principalEntityType
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData CreateIdentitySource where
  rnf CreateIdentitySource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf principalEntityType
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToHeaders CreateIdentitySource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.CreateIdentitySource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIdentitySource where
  toJSON CreateIdentitySource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("principalEntityType" Data..=)
              Prelude.<$> principalEntityType,
            Prelude.Just ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath CreateIdentitySource where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIdentitySource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIdentitySourceResponse' smart constructor.
data CreateIdentitySourceResponse = CreateIdentitySourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time the identity source was originally created.
    createdDate :: Data.ISO8601,
    -- | The unique ID of the new identity source.
    identitySourceId :: Prelude.Text,
    -- | The date and time the identity source was most recently updated.
    lastUpdatedDate :: Data.ISO8601,
    -- | The ID of the policy store that contains the identity source.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIdentitySourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createIdentitySourceResponse_httpStatus' - The response's http status code.
--
-- 'createdDate', 'createIdentitySourceResponse_createdDate' - The date and time the identity source was originally created.
--
-- 'identitySourceId', 'createIdentitySourceResponse_identitySourceId' - The unique ID of the new identity source.
--
-- 'lastUpdatedDate', 'createIdentitySourceResponse_lastUpdatedDate' - The date and time the identity source was most recently updated.
--
-- 'policyStoreId', 'createIdentitySourceResponse_policyStoreId' - The ID of the policy store that contains the identity source.
newCreateIdentitySourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'identitySourceId'
  Prelude.Text ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  -- | 'policyStoreId'
  Prelude.Text ->
  CreateIdentitySourceResponse
newCreateIdentitySourceResponse
  pHttpStatus_
  pCreatedDate_
  pIdentitySourceId_
  pLastUpdatedDate_
  pPolicyStoreId_ =
    CreateIdentitySourceResponse'
      { httpStatus =
          pHttpStatus_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        identitySourceId = pIdentitySourceId_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_,
        policyStoreId = pPolicyStoreId_
      }

-- | The response's http status code.
createIdentitySourceResponse_httpStatus :: Lens.Lens' CreateIdentitySourceResponse Prelude.Int
createIdentitySourceResponse_httpStatus = Lens.lens (\CreateIdentitySourceResponse' {httpStatus} -> httpStatus) (\s@CreateIdentitySourceResponse' {} a -> s {httpStatus = a} :: CreateIdentitySourceResponse)

-- | The date and time the identity source was originally created.
createIdentitySourceResponse_createdDate :: Lens.Lens' CreateIdentitySourceResponse Prelude.UTCTime
createIdentitySourceResponse_createdDate = Lens.lens (\CreateIdentitySourceResponse' {createdDate} -> createdDate) (\s@CreateIdentitySourceResponse' {} a -> s {createdDate = a} :: CreateIdentitySourceResponse) Prelude.. Data._Time

-- | The unique ID of the new identity source.
createIdentitySourceResponse_identitySourceId :: Lens.Lens' CreateIdentitySourceResponse Prelude.Text
createIdentitySourceResponse_identitySourceId = Lens.lens (\CreateIdentitySourceResponse' {identitySourceId} -> identitySourceId) (\s@CreateIdentitySourceResponse' {} a -> s {identitySourceId = a} :: CreateIdentitySourceResponse)

-- | The date and time the identity source was most recently updated.
createIdentitySourceResponse_lastUpdatedDate :: Lens.Lens' CreateIdentitySourceResponse Prelude.UTCTime
createIdentitySourceResponse_lastUpdatedDate = Lens.lens (\CreateIdentitySourceResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateIdentitySourceResponse' {} a -> s {lastUpdatedDate = a} :: CreateIdentitySourceResponse) Prelude.. Data._Time

-- | The ID of the policy store that contains the identity source.
createIdentitySourceResponse_policyStoreId :: Lens.Lens' CreateIdentitySourceResponse Prelude.Text
createIdentitySourceResponse_policyStoreId = Lens.lens (\CreateIdentitySourceResponse' {policyStoreId} -> policyStoreId) (\s@CreateIdentitySourceResponse' {} a -> s {policyStoreId = a} :: CreateIdentitySourceResponse)

instance Prelude.NFData CreateIdentitySourceResponse where
  rnf CreateIdentitySourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf identitySourceId
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf policyStoreId
