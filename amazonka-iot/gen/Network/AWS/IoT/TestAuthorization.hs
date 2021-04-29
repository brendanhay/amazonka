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
-- Module      : Network.AWS.IoT.TestAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests if a specified principal is authorized to perform an AWS IoT
-- action on a specified resource. Use this to test and debug the
-- authorization behavior of devices that connect to the AWS IoT device
-- gateway.
module Network.AWS.IoT.TestAuthorization
  ( -- * Creating a Request
    TestAuthorization (..),
    newTestAuthorization,

    -- * Request Lenses
    testAuthorization_clientId,
    testAuthorization_cognitoIdentityPoolId,
    testAuthorization_principal,
    testAuthorization_policyNamesToSkip,
    testAuthorization_policyNamesToAdd,
    testAuthorization_authInfos,

    -- * Destructuring the Response
    TestAuthorizationResponse (..),
    newTestAuthorizationResponse,

    -- * Response Lenses
    testAuthorizationResponse_authResults,
    testAuthorizationResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTestAuthorization' smart constructor.
data TestAuthorization = TestAuthorization'
  { -- | The MQTT client ID.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The Cognito identity pool ID.
    cognitoIdentityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The principal. Valid principals are CertificateArn
    -- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
    -- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
    -- (/region/:/id/).
    principal :: Prelude.Maybe Prelude.Text,
    -- | When testing custom authorization, the policies specified here are
    -- treated as if they are not attached to the principal being authorized.
    policyNamesToSkip :: Prelude.Maybe [Prelude.Text],
    -- | When testing custom authorization, the policies specified here are
    -- treated as if they are attached to the principal being authorized.
    policyNamesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | A list of authorization info objects. Simulating authorization will
    -- create a response for each @authInfo@ object in the list.
    authInfos :: Prelude.NonEmpty AuthInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TestAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'testAuthorization_clientId' - The MQTT client ID.
--
-- 'cognitoIdentityPoolId', 'testAuthorization_cognitoIdentityPoolId' - The Cognito identity pool ID.
--
-- 'principal', 'testAuthorization_principal' - The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
--
-- 'policyNamesToSkip', 'testAuthorization_policyNamesToSkip' - When testing custom authorization, the policies specified here are
-- treated as if they are not attached to the principal being authorized.
--
-- 'policyNamesToAdd', 'testAuthorization_policyNamesToAdd' - When testing custom authorization, the policies specified here are
-- treated as if they are attached to the principal being authorized.
--
-- 'authInfos', 'testAuthorization_authInfos' - A list of authorization info objects. Simulating authorization will
-- create a response for each @authInfo@ object in the list.
newTestAuthorization ::
  -- | 'authInfos'
  Prelude.NonEmpty AuthInfo ->
  TestAuthorization
newTestAuthorization pAuthInfos_ =
  TestAuthorization'
    { clientId = Prelude.Nothing,
      cognitoIdentityPoolId = Prelude.Nothing,
      principal = Prelude.Nothing,
      policyNamesToSkip = Prelude.Nothing,
      policyNamesToAdd = Prelude.Nothing,
      authInfos = Prelude._Coerce Lens.# pAuthInfos_
    }

-- | The MQTT client ID.
testAuthorization_clientId :: Lens.Lens' TestAuthorization (Prelude.Maybe Prelude.Text)
testAuthorization_clientId = Lens.lens (\TestAuthorization' {clientId} -> clientId) (\s@TestAuthorization' {} a -> s {clientId = a} :: TestAuthorization)

-- | The Cognito identity pool ID.
testAuthorization_cognitoIdentityPoolId :: Lens.Lens' TestAuthorization (Prelude.Maybe Prelude.Text)
testAuthorization_cognitoIdentityPoolId = Lens.lens (\TestAuthorization' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@TestAuthorization' {} a -> s {cognitoIdentityPoolId = a} :: TestAuthorization)

-- | The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
testAuthorization_principal :: Lens.Lens' TestAuthorization (Prelude.Maybe Prelude.Text)
testAuthorization_principal = Lens.lens (\TestAuthorization' {principal} -> principal) (\s@TestAuthorization' {} a -> s {principal = a} :: TestAuthorization)

-- | When testing custom authorization, the policies specified here are
-- treated as if they are not attached to the principal being authorized.
testAuthorization_policyNamesToSkip :: Lens.Lens' TestAuthorization (Prelude.Maybe [Prelude.Text])
testAuthorization_policyNamesToSkip = Lens.lens (\TestAuthorization' {policyNamesToSkip} -> policyNamesToSkip) (\s@TestAuthorization' {} a -> s {policyNamesToSkip = a} :: TestAuthorization) Prelude.. Lens.mapping Prelude._Coerce

-- | When testing custom authorization, the policies specified here are
-- treated as if they are attached to the principal being authorized.
testAuthorization_policyNamesToAdd :: Lens.Lens' TestAuthorization (Prelude.Maybe [Prelude.Text])
testAuthorization_policyNamesToAdd = Lens.lens (\TestAuthorization' {policyNamesToAdd} -> policyNamesToAdd) (\s@TestAuthorization' {} a -> s {policyNamesToAdd = a} :: TestAuthorization) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of authorization info objects. Simulating authorization will
-- create a response for each @authInfo@ object in the list.
testAuthorization_authInfos :: Lens.Lens' TestAuthorization (Prelude.NonEmpty AuthInfo)
testAuthorization_authInfos = Lens.lens (\TestAuthorization' {authInfos} -> authInfos) (\s@TestAuthorization' {} a -> s {authInfos = a} :: TestAuthorization) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TestAuthorization where
  type Rs TestAuthorization = TestAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestAuthorizationResponse'
            Prelude.<$> ( x Prelude..?> "authResults"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestAuthorization

instance Prelude.NFData TestAuthorization

instance Prelude.ToHeaders TestAuthorization where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON TestAuthorization where
  toJSON TestAuthorization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("cognitoIdentityPoolId" Prelude..=)
              Prelude.<$> cognitoIdentityPoolId,
            ("principal" Prelude..=) Prelude.<$> principal,
            ("policyNamesToSkip" Prelude..=)
              Prelude.<$> policyNamesToSkip,
            ("policyNamesToAdd" Prelude..=)
              Prelude.<$> policyNamesToAdd,
            Prelude.Just ("authInfos" Prelude..= authInfos)
          ]
      )

instance Prelude.ToPath TestAuthorization where
  toPath = Prelude.const "/test-authorization"

instance Prelude.ToQuery TestAuthorization where
  toQuery TestAuthorization' {..} =
    Prelude.mconcat ["clientId" Prelude.=: clientId]

-- | /See:/ 'newTestAuthorizationResponse' smart constructor.
data TestAuthorizationResponse = TestAuthorizationResponse'
  { -- | The authentication results.
    authResults :: Prelude.Maybe [AuthResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TestAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authResults', 'testAuthorizationResponse_authResults' - The authentication results.
--
-- 'httpStatus', 'testAuthorizationResponse_httpStatus' - The response's http status code.
newTestAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestAuthorizationResponse
newTestAuthorizationResponse pHttpStatus_ =
  TestAuthorizationResponse'
    { authResults =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication results.
testAuthorizationResponse_authResults :: Lens.Lens' TestAuthorizationResponse (Prelude.Maybe [AuthResult])
testAuthorizationResponse_authResults = Lens.lens (\TestAuthorizationResponse' {authResults} -> authResults) (\s@TestAuthorizationResponse' {} a -> s {authResults = a} :: TestAuthorizationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
testAuthorizationResponse_httpStatus :: Lens.Lens' TestAuthorizationResponse Prelude.Int
testAuthorizationResponse_httpStatus = Lens.lens (\TestAuthorizationResponse' {httpStatus} -> httpStatus) (\s@TestAuthorizationResponse' {} a -> s {httpStatus = a} :: TestAuthorizationResponse)

instance Prelude.NFData TestAuthorizationResponse
