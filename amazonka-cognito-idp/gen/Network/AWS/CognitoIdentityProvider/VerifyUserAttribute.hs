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
-- Module      : Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the specified user attributes in the user pool.
module Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to verify user attributes.
--
-- /See:/ 'newVerifyUserAttribute' smart constructor.
data VerifyUserAttribute = VerifyUserAttribute'
  { -- | Represents the access token of the request to verify user attributes.
    accessToken :: Prelude.Sensitive Prelude.Text,
    -- | The attribute name in the request to verify user attributes.
    attributeName :: Prelude.Text,
    -- | The verification code in the request to verify user attributes.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VerifyUserAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'verifyUserAttribute_accessToken' - Represents the access token of the request to verify user attributes.
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
          Prelude._Sensitive Lens.# pAccessToken_,
        attributeName = pAttributeName_,
        code = pCode_
      }

-- | Represents the access token of the request to verify user attributes.
verifyUserAttribute_accessToken :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_accessToken = Lens.lens (\VerifyUserAttribute' {accessToken} -> accessToken) (\s@VerifyUserAttribute' {} a -> s {accessToken = a} :: VerifyUserAttribute) Prelude.. Prelude._Sensitive

-- | The attribute name in the request to verify user attributes.
verifyUserAttribute_attributeName :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_attributeName = Lens.lens (\VerifyUserAttribute' {attributeName} -> attributeName) (\s@VerifyUserAttribute' {} a -> s {attributeName = a} :: VerifyUserAttribute)

-- | The verification code in the request to verify user attributes.
verifyUserAttribute_code :: Lens.Lens' VerifyUserAttribute Prelude.Text
verifyUserAttribute_code = Lens.lens (\VerifyUserAttribute' {code} -> code) (\s@VerifyUserAttribute' {} a -> s {code = a} :: VerifyUserAttribute)

instance Prelude.AWSRequest VerifyUserAttribute where
  type
    Rs VerifyUserAttribute =
      VerifyUserAttributeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          VerifyUserAttributeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyUserAttribute

instance Prelude.NFData VerifyUserAttribute

instance Prelude.ToHeaders VerifyUserAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.VerifyUserAttribute" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON VerifyUserAttribute where
  toJSON VerifyUserAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccessToken" Prelude..= accessToken),
            Prelude.Just
              ("AttributeName" Prelude..= attributeName),
            Prelude.Just ("Code" Prelude..= code)
          ]
      )

instance Prelude.ToPath VerifyUserAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery VerifyUserAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | A container representing the response from the server from the request
-- to verify user attributes.
--
-- /See:/ 'newVerifyUserAttributeResponse' smart constructor.
data VerifyUserAttributeResponse = VerifyUserAttributeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData VerifyUserAttributeResponse
