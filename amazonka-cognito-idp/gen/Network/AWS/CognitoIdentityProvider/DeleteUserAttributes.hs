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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the attributes for a user.
module Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete user attributes.
--
-- /See:/ 'newDeleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
  { -- | An array of strings representing the user attribute names you wish to
    -- delete.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributeNames :: [Prelude.Text],
    -- | The access token used in the request to delete user attributes.
    accessToken :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAttributeNames', 'deleteUserAttributes_userAttributeNames' - An array of strings representing the user attribute names you wish to
-- delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- 'accessToken', 'deleteUserAttributes_accessToken' - The access token used in the request to delete user attributes.
newDeleteUserAttributes ::
  -- | 'accessToken'
  Prelude.Text ->
  DeleteUserAttributes
newDeleteUserAttributes pAccessToken_ =
  DeleteUserAttributes'
    { userAttributeNames =
        Prelude.mempty,
      accessToken =
        Prelude._Sensitive Lens.# pAccessToken_
    }

-- | An array of strings representing the user attribute names you wish to
-- delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
deleteUserAttributes_userAttributeNames :: Lens.Lens' DeleteUserAttributes [Prelude.Text]
deleteUserAttributes_userAttributeNames = Lens.lens (\DeleteUserAttributes' {userAttributeNames} -> userAttributeNames) (\s@DeleteUserAttributes' {} a -> s {userAttributeNames = a} :: DeleteUserAttributes) Prelude.. Prelude._Coerce

-- | The access token used in the request to delete user attributes.
deleteUserAttributes_accessToken :: Lens.Lens' DeleteUserAttributes Prelude.Text
deleteUserAttributes_accessToken = Lens.lens (\DeleteUserAttributes' {accessToken} -> accessToken) (\s@DeleteUserAttributes' {} a -> s {accessToken = a} :: DeleteUserAttributes) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest DeleteUserAttributes where
  type
    Rs DeleteUserAttributes =
      DeleteUserAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUserAttributes

instance Prelude.NFData DeleteUserAttributes

instance Prelude.ToHeaders DeleteUserAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.DeleteUserAttributes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteUserAttributes where
  toJSON DeleteUserAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("UserAttributeNames" Prelude..= userAttributeNames),
            Prelude.Just ("AccessToken" Prelude..= accessToken)
          ]
      )

instance Prelude.ToPath DeleteUserAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUserAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to delete user attributes.
--
-- /See:/ 'newDeleteUserAttributesResponse' smart constructor.
data DeleteUserAttributesResponse = DeleteUserAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteUserAttributesResponse
