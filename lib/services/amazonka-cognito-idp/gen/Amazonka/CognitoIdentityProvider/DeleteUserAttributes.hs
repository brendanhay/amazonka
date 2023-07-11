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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteUserAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the attributes for a user.
module Amazonka.CognitoIdentityProvider.DeleteUserAttributes
  ( -- * Creating a Request
    DeleteUserAttributes (..),
    newDeleteUserAttributes,

    -- * Request Lenses
    deleteUserAttributes_userAttributeNames,
    deleteUserAttributes_accessToken,

    -- * Destructuring the Response
    DeleteUserAttributesResponse (..),
    newDeleteUserAttributesResponse,

    -- * Response Lenses
    deleteUserAttributesResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete user attributes.
--
-- /See:/ 'newDeleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
  { -- | An array of strings representing the user attribute names you want to
    -- delete.
    --
    -- For custom attributes, you must prependattach the @custom:@ prefix to
    -- the front of the attribute name.
    userAttributeNames :: [Prelude.Text],
    -- | A valid access token that Amazon Cognito issued to the user whose
    -- attributes you want to delete.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAttributeNames', 'deleteUserAttributes_userAttributeNames' - An array of strings representing the user attribute names you want to
-- delete.
--
-- For custom attributes, you must prependattach the @custom:@ prefix to
-- the front of the attribute name.
--
-- 'accessToken', 'deleteUserAttributes_accessToken' - A valid access token that Amazon Cognito issued to the user whose
-- attributes you want to delete.
newDeleteUserAttributes ::
  -- | 'accessToken'
  Prelude.Text ->
  DeleteUserAttributes
newDeleteUserAttributes pAccessToken_ =
  DeleteUserAttributes'
    { userAttributeNames =
        Prelude.mempty,
      accessToken = Data._Sensitive Lens.# pAccessToken_
    }

-- | An array of strings representing the user attribute names you want to
-- delete.
--
-- For custom attributes, you must prependattach the @custom:@ prefix to
-- the front of the attribute name.
deleteUserAttributes_userAttributeNames :: Lens.Lens' DeleteUserAttributes [Prelude.Text]
deleteUserAttributes_userAttributeNames = Lens.lens (\DeleteUserAttributes' {userAttributeNames} -> userAttributeNames) (\s@DeleteUserAttributes' {} a -> s {userAttributeNames = a} :: DeleteUserAttributes) Prelude.. Lens.coerced

-- | A valid access token that Amazon Cognito issued to the user whose
-- attributes you want to delete.
deleteUserAttributes_accessToken :: Lens.Lens' DeleteUserAttributes Prelude.Text
deleteUserAttributes_accessToken = Lens.lens (\DeleteUserAttributes' {accessToken} -> accessToken) (\s@DeleteUserAttributes' {} a -> s {accessToken = a} :: DeleteUserAttributes) Prelude.. Data._Sensitive

instance Core.AWSRequest DeleteUserAttributes where
  type
    AWSResponse DeleteUserAttributes =
      DeleteUserAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUserAttributes where
  hashWithSalt _salt DeleteUserAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` userAttributeNames
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData DeleteUserAttributes where
  rnf DeleteUserAttributes' {..} =
    Prelude.rnf userAttributeNames
      `Prelude.seq` Prelude.rnf accessToken

instance Data.ToHeaders DeleteUserAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteUserAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUserAttributes where
  toJSON DeleteUserAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("UserAttributeNames" Data..= userAttributeNames),
            Prelude.Just ("AccessToken" Data..= accessToken)
          ]
      )

instance Data.ToPath DeleteUserAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to delete user attributes.
--
-- /See:/ 'newDeleteUserAttributesResponse' smart constructor.
data DeleteUserAttributesResponse = DeleteUserAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserAttributesResponse_httpStatus' - The response's http status code.
newDeleteUserAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUserAttributesResponse
newDeleteUserAttributesResponse pHttpStatus_ =
  DeleteUserAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUserAttributesResponse_httpStatus :: Lens.Lens' DeleteUserAttributesResponse Prelude.Int
deleteUserAttributesResponse_httpStatus = Lens.lens (\DeleteUserAttributesResponse' {httpStatus} -> httpStatus) (\s@DeleteUserAttributesResponse' {} a -> s {httpStatus = a} :: DeleteUserAttributesResponse)

instance Prelude.NFData DeleteUserAttributesResponse where
  rnf DeleteUserAttributesResponse' {..} =
    Prelude.rnf httpStatus
