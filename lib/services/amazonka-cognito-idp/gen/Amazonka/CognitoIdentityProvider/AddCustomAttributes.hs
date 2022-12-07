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
-- Module      : Amazonka.CognitoIdentityProvider.AddCustomAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional user attributes to the user pool schema.
module Amazonka.CognitoIdentityProvider.AddCustomAttributes
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Lens.coerced Lens.# pCustomAttributes_
      }

-- | The user pool ID for the user pool where you want to add custom
-- attributes.
addCustomAttributes_userPoolId :: Lens.Lens' AddCustomAttributes Prelude.Text
addCustomAttributes_userPoolId = Lens.lens (\AddCustomAttributes' {userPoolId} -> userPoolId) (\s@AddCustomAttributes' {} a -> s {userPoolId = a} :: AddCustomAttributes)

-- | An array of custom attributes, such as Mutable and Name.
addCustomAttributes_customAttributes :: Lens.Lens' AddCustomAttributes (Prelude.NonEmpty SchemaAttributeType)
addCustomAttributes_customAttributes = Lens.lens (\AddCustomAttributes' {customAttributes} -> customAttributes) (\s@AddCustomAttributes' {} a -> s {customAttributes = a} :: AddCustomAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest AddCustomAttributes where
  type
    AWSResponse AddCustomAttributes =
      AddCustomAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddCustomAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCustomAttributes where
  hashWithSalt _salt AddCustomAttributes' {..} =
    _salt `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` customAttributes

instance Prelude.NFData AddCustomAttributes where
  rnf AddCustomAttributes' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf customAttributes

instance Data.ToHeaders AddCustomAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AddCustomAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddCustomAttributes where
  toJSON AddCustomAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just
              ("CustomAttributes" Data..= customAttributes)
          ]
      )

instance Data.ToPath AddCustomAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery AddCustomAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to add custom
-- attributes.
--
-- /See:/ 'newAddCustomAttributesResponse' smart constructor.
data AddCustomAttributesResponse = AddCustomAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AddCustomAttributesResponse where
  rnf AddCustomAttributesResponse' {..} =
    Prelude.rnf httpStatus
