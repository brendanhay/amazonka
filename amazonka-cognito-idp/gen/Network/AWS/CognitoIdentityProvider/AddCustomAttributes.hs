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
-- Module      : Network.AWS.CognitoIdentityProvider.AddCustomAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional user attributes to the user pool schema.
module Network.AWS.CognitoIdentityProvider.AddCustomAttributes
  ( -- * Creating a Request
    AddCustomAttributes (..),
    newAddCustomAttributes,

    -- * Request Lenses
    addCustomAttributes_userPoolId,
    addCustomAttributes_customAttributes,

    -- * Destructuring the Response
    AddCustomAttributesResponse (..),
    newAddCustomAttributesResponse,

    -- * Response Lenses
    addCustomAttributesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to add custom attributes.
--
-- /See:/ 'newAddCustomAttributes' smart constructor.
data AddCustomAttributes = AddCustomAttributes'
  { -- | The user pool ID for the user pool where you want to add custom
    -- attributes.
    userPoolId :: Prelude.Text,
    -- | An array of custom attributes, such as Mutable and Name.
    customAttributes :: Prelude.NonEmpty SchemaAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddCustomAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'addCustomAttributes_userPoolId' - The user pool ID for the user pool where you want to add custom
-- attributes.
--
-- 'customAttributes', 'addCustomAttributes_customAttributes' - An array of custom attributes, such as Mutable and Name.
newAddCustomAttributes ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'customAttributes'
  Prelude.NonEmpty SchemaAttributeType ->
  AddCustomAttributes
newAddCustomAttributes
  pUserPoolId_
  pCustomAttributes_ =
    AddCustomAttributes'
      { userPoolId = pUserPoolId_,
        customAttributes =
          Prelude._Coerce Lens.# pCustomAttributes_
      }

-- | The user pool ID for the user pool where you want to add custom
-- attributes.
addCustomAttributes_userPoolId :: Lens.Lens' AddCustomAttributes Prelude.Text
addCustomAttributes_userPoolId = Lens.lens (\AddCustomAttributes' {userPoolId} -> userPoolId) (\s@AddCustomAttributes' {} a -> s {userPoolId = a} :: AddCustomAttributes)

-- | An array of custom attributes, such as Mutable and Name.
addCustomAttributes_customAttributes :: Lens.Lens' AddCustomAttributes (Prelude.NonEmpty SchemaAttributeType)
addCustomAttributes_customAttributes = Lens.lens (\AddCustomAttributes' {customAttributes} -> customAttributes) (\s@AddCustomAttributes' {} a -> s {customAttributes = a} :: AddCustomAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddCustomAttributes where
  type
    Rs AddCustomAttributes =
      AddCustomAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddCustomAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCustomAttributes

instance Prelude.NFData AddCustomAttributes

instance Prelude.ToHeaders AddCustomAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AddCustomAttributes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddCustomAttributes where
  toJSON AddCustomAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just
              ("CustomAttributes" Prelude..= customAttributes)
          ]
      )

instance Prelude.ToPath AddCustomAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddCustomAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to add custom
-- attributes.
--
-- /See:/ 'newAddCustomAttributesResponse' smart constructor.
data AddCustomAttributesResponse = AddCustomAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddCustomAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addCustomAttributesResponse_httpStatus' - The response's http status code.
newAddCustomAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddCustomAttributesResponse
newAddCustomAttributesResponse pHttpStatus_ =
  AddCustomAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addCustomAttributesResponse_httpStatus :: Lens.Lens' AddCustomAttributesResponse Prelude.Int
addCustomAttributesResponse_httpStatus = Lens.lens (\AddCustomAttributesResponse' {httpStatus} -> httpStatus) (\s@AddCustomAttributesResponse' {} a -> s {httpStatus = a} :: AddCustomAttributesResponse)

instance Prelude.NFData AddCustomAttributesResponse
