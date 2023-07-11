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
-- Module      : Amazonka.CognitoIdentityProvider.VerifyUserAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the specified user attributes in the user pool.
--
-- If your user pool requires verification before Amazon Cognito updates
-- the attribute value, VerifyUserAttribute updates the affected attribute
-- to its pending value. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UserAttributeUpdateSettingsType.html UserAttributeUpdateSettingsType>.
module Amazonka.CognitoIdentityProvider.VerifyUserAttribute
  ( -- * Creating a Request
    VerifyUserAttribute (..),
    newVerifyUserAttribute,

    -- * Request Lenses
    verifyUserAttribute_accessToken,
    verifyUserAttribute_attributeName,
    verifyUserAttribute_code,

    -- * Destructuring the Response
    VerifyUserAttributeResponse (..),
    newVerifyUserAttributeResponse,

    -- * Response Lenses
    verifyUserAttributeResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to verify user attributes.
--
-- /See:/ 'newVerifyUserAttribute' smart constructor.
data VerifyUserAttribute = VerifyUserAttribute'
  { -- | A valid access token that Amazon Cognito issued to the user whose user
    -- attributes you want to verify.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The attribute name in the request to verify user attributes.
    attributeName :: Prelude.Text,
    -- | The verification code in the request to verify user attributes.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyUserAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'verifyUserAttribute_accessToken' - A valid access token that Amazon Cognito issued to the user whose user
-- attributes you want to verify.
--
-- 'attributeName', 'verifyUserAttribute_attributeName' - The attribute name in the request to verify user attributes.
--
-- 'code', 'verifyUserAttribute_code' - The verification code in the request to verify user attributes.
newVerifyUserAttribute ::
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  VerifyUserAttribute
newVerifyUserAttribute
  pAccessToken_
  pAttributeName_
  pCode_ =
    VerifyUserAttribute'
      { accessToken =
          Data._Sensitive Lens.# pAccessToken_,
        attributeName = pAttributeName_,
        code = pCode_
      }

-- | A valid access token that Amazon Cognito issued to the user whose user
-- attributes you want to verify.
verifyUserAttribute_accessToken :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_accessToken = Lens.lens (\VerifyUserAttribute' {accessToken} -> accessToken) (\s@VerifyUserAttribute' {} a -> s {accessToken = a} :: VerifyUserAttribute) Prelude.. Data._Sensitive

-- | The attribute name in the request to verify user attributes.
verifyUserAttribute_attributeName :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_attributeName = Lens.lens (\VerifyUserAttribute' {attributeName} -> attributeName) (\s@VerifyUserAttribute' {} a -> s {attributeName = a} :: VerifyUserAttribute)

-- | The verification code in the request to verify user attributes.
verifyUserAttribute_code :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_code = Lens.lens (\VerifyUserAttribute' {code} -> code) (\s@VerifyUserAttribute' {} a -> s {code = a} :: VerifyUserAttribute)

instance Core.AWSRequest VerifyUserAttribute where
  type
    AWSResponse VerifyUserAttribute =
      VerifyUserAttributeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          VerifyUserAttributeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyUserAttribute where
  hashWithSalt _salt VerifyUserAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` code

instance Prelude.NFData VerifyUserAttribute where
  rnf VerifyUserAttribute' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf code

instance Data.ToHeaders VerifyUserAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.VerifyUserAttribute" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyUserAttribute where
  toJSON VerifyUserAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccessToken" Data..= accessToken),
            Prelude.Just ("AttributeName" Data..= attributeName),
            Prelude.Just ("Code" Data..= code)
          ]
      )

instance Data.ToPath VerifyUserAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyUserAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | A container representing the response from the server from the request
-- to verify user attributes.
--
-- /See:/ 'newVerifyUserAttributeResponse' smart constructor.
data VerifyUserAttributeResponse = VerifyUserAttributeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyUserAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyUserAttributeResponse_httpStatus' - The response's http status code.
newVerifyUserAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifyUserAttributeResponse
newVerifyUserAttributeResponse pHttpStatus_ =
  VerifyUserAttributeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
verifyUserAttributeResponse_httpStatus :: Lens.Lens' VerifyUserAttributeResponse Prelude.Int
verifyUserAttributeResponse_httpStatus = Lens.lens (\VerifyUserAttributeResponse' {httpStatus} -> httpStatus) (\s@VerifyUserAttributeResponse' {} a -> s {httpStatus = a} :: VerifyUserAttributeResponse)

instance Prelude.NFData VerifyUserAttributeResponse where
  rnf VerifyUserAttributeResponse' {..} =
    Prelude.rnf httpStatus
