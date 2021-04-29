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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the user attributes in a user pool as an administrator. Works on
-- any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
  ( -- * Creating a Request
    AdminDeleteUserAttributes (..),
    newAdminDeleteUserAttributes,

    -- * Request Lenses
    adminDeleteUserAttributes_userPoolId,
    adminDeleteUserAttributes_username,
    adminDeleteUserAttributes_userAttributeNames,

    -- * Destructuring the Response
    AdminDeleteUserAttributesResponse (..),
    newAdminDeleteUserAttributesResponse,

    -- * Response Lenses
    adminDeleteUserAttributesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete user attributes as an administrator.
--
-- /See:/ 'newAdminDeleteUserAttributes' smart constructor.
data AdminDeleteUserAttributes = AdminDeleteUserAttributes'
  { -- | The user pool ID for the user pool where you want to delete user
    -- attributes.
    userPoolId :: Prelude.Text,
    -- | The user name of the user from which you would like to delete
    -- attributes.
    username :: Prelude.Sensitive Prelude.Text,
    -- | An array of strings representing the user attribute names you wish to
    -- delete.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDeleteUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminDeleteUserAttributes_userPoolId' - The user pool ID for the user pool where you want to delete user
-- attributes.
--
-- 'username', 'adminDeleteUserAttributes_username' - The user name of the user from which you would like to delete
-- attributes.
--
-- 'userAttributeNames', 'adminDeleteUserAttributes_userAttributeNames' - An array of strings representing the user attribute names you wish to
-- delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
newAdminDeleteUserAttributes ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminDeleteUserAttributes
newAdminDeleteUserAttributes pUserPoolId_ pUsername_ =
  AdminDeleteUserAttributes'
    { userPoolId =
        pUserPoolId_,
      username = Prelude._Sensitive Lens.# pUsername_,
      userAttributeNames = Prelude.mempty
    }

-- | The user pool ID for the user pool where you want to delete user
-- attributes.
adminDeleteUserAttributes_userPoolId :: Lens.Lens' AdminDeleteUserAttributes Prelude.Text
adminDeleteUserAttributes_userPoolId = Lens.lens (\AdminDeleteUserAttributes' {userPoolId} -> userPoolId) (\s@AdminDeleteUserAttributes' {} a -> s {userPoolId = a} :: AdminDeleteUserAttributes)

-- | The user name of the user from which you would like to delete
-- attributes.
adminDeleteUserAttributes_username :: Lens.Lens' AdminDeleteUserAttributes Prelude.Text
adminDeleteUserAttributes_username = Lens.lens (\AdminDeleteUserAttributes' {username} -> username) (\s@AdminDeleteUserAttributes' {} a -> s {username = a} :: AdminDeleteUserAttributes) Prelude.. Prelude._Sensitive

-- | An array of strings representing the user attribute names you wish to
-- delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
adminDeleteUserAttributes_userAttributeNames :: Lens.Lens' AdminDeleteUserAttributes [Prelude.Text]
adminDeleteUserAttributes_userAttributeNames = Lens.lens (\AdminDeleteUserAttributes' {userAttributeNames} -> userAttributeNames) (\s@AdminDeleteUserAttributes' {} a -> s {userAttributeNames = a} :: AdminDeleteUserAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AdminDeleteUserAttributes where
  type
    Rs AdminDeleteUserAttributes =
      AdminDeleteUserAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminDeleteUserAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminDeleteUserAttributes

instance Prelude.NFData AdminDeleteUserAttributes

instance Prelude.ToHeaders AdminDeleteUserAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminDeleteUserAttributes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminDeleteUserAttributes where
  toJSON AdminDeleteUserAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just
              ( "UserAttributeNames"
                  Prelude..= userAttributeNames
              )
          ]
      )

instance Prelude.ToPath AdminDeleteUserAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminDeleteUserAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response received from the server for a request to delete
-- user attributes.
--
-- /See:/ 'newAdminDeleteUserAttributesResponse' smart constructor.
data AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDeleteUserAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminDeleteUserAttributesResponse_httpStatus' - The response's http status code.
newAdminDeleteUserAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminDeleteUserAttributesResponse
newAdminDeleteUserAttributesResponse pHttpStatus_ =
  AdminDeleteUserAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminDeleteUserAttributesResponse_httpStatus :: Lens.Lens' AdminDeleteUserAttributesResponse Prelude.Int
adminDeleteUserAttributesResponse_httpStatus = Lens.lens (\AdminDeleteUserAttributesResponse' {httpStatus} -> httpStatus) (\s@AdminDeleteUserAttributesResponse' {} a -> s {httpStatus = a} :: AdminDeleteUserAttributesResponse)

instance
  Prelude.NFData
    AdminDeleteUserAttributesResponse
