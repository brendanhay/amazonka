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
-- Module      : Network.AWS.Pinpoint.RemoveAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more attributes, of the same attribute type, from all the
-- endpoints that are associated with an application.
module Network.AWS.Pinpoint.RemoveAttributes
  ( -- * Creating a Request
    RemoveAttributes (..),
    newRemoveAttributes,

    -- * Request Lenses
    removeAttributes_attributeType,
    removeAttributes_applicationId,
    removeAttributes_updateAttributesRequest,

    -- * Destructuring the Response
    RemoveAttributesResponse (..),
    newRemoveAttributesResponse,

    -- * Response Lenses
    removeAttributesResponse_httpStatus,
    removeAttributesResponse_attributesResource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveAttributes' smart constructor.
data RemoveAttributes = RemoveAttributes'
  { -- | The type of attribute or attributes to remove. Valid values are:
    --
    -- -   endpoint-custom-attributes - Custom attributes that describe
    --     endpoints, such as the date when an associated user opted in or out
    --     of receiving communications from you through a specific type of
    --     channel.
    --
    -- -   endpoint-metric-attributes - Custom metrics that your app reports to
    --     Amazon Pinpoint for endpoints, such as the number of app sessions or
    --     the number of items left in a cart.
    --
    -- -   endpoint-user-attributes - Custom attributes that describe users,
    --     such as first name, last name, and age.
    attributeType :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    updateAttributesRequest :: UpdateAttributesRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeType', 'removeAttributes_attributeType' - The type of attribute or attributes to remove. Valid values are:
--
-- -   endpoint-custom-attributes - Custom attributes that describe
--     endpoints, such as the date when an associated user opted in or out
--     of receiving communications from you through a specific type of
--     channel.
--
-- -   endpoint-metric-attributes - Custom metrics that your app reports to
--     Amazon Pinpoint for endpoints, such as the number of app sessions or
--     the number of items left in a cart.
--
-- -   endpoint-user-attributes - Custom attributes that describe users,
--     such as first name, last name, and age.
--
-- 'applicationId', 'removeAttributes_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'updateAttributesRequest', 'removeAttributes_updateAttributesRequest' - Undocumented member.
newRemoveAttributes ::
  -- | 'attributeType'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'updateAttributesRequest'
  UpdateAttributesRequest ->
  RemoveAttributes
newRemoveAttributes
  pAttributeType_
  pApplicationId_
  pUpdateAttributesRequest_ =
    RemoveAttributes'
      { attributeType = pAttributeType_,
        applicationId = pApplicationId_,
        updateAttributesRequest = pUpdateAttributesRequest_
      }

-- | The type of attribute or attributes to remove. Valid values are:
--
-- -   endpoint-custom-attributes - Custom attributes that describe
--     endpoints, such as the date when an associated user opted in or out
--     of receiving communications from you through a specific type of
--     channel.
--
-- -   endpoint-metric-attributes - Custom metrics that your app reports to
--     Amazon Pinpoint for endpoints, such as the number of app sessions or
--     the number of items left in a cart.
--
-- -   endpoint-user-attributes - Custom attributes that describe users,
--     such as first name, last name, and age.
removeAttributes_attributeType :: Lens.Lens' RemoveAttributes Prelude.Text
removeAttributes_attributeType = Lens.lens (\RemoveAttributes' {attributeType} -> attributeType) (\s@RemoveAttributes' {} a -> s {attributeType = a} :: RemoveAttributes)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
removeAttributes_applicationId :: Lens.Lens' RemoveAttributes Prelude.Text
removeAttributes_applicationId = Lens.lens (\RemoveAttributes' {applicationId} -> applicationId) (\s@RemoveAttributes' {} a -> s {applicationId = a} :: RemoveAttributes)

-- | Undocumented member.
removeAttributes_updateAttributesRequest :: Lens.Lens' RemoveAttributes UpdateAttributesRequest
removeAttributes_updateAttributesRequest = Lens.lens (\RemoveAttributes' {updateAttributesRequest} -> updateAttributesRequest) (\s@RemoveAttributes' {} a -> s {updateAttributesRequest = a} :: RemoveAttributes)

instance Prelude.AWSRequest RemoveAttributes where
  type Rs RemoveAttributes = RemoveAttributesResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable RemoveAttributes

instance Prelude.NFData RemoveAttributes

instance Prelude.ToHeaders RemoveAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveAttributes where
  toJSON RemoveAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UpdateAttributesRequest"
                  Prelude..= updateAttributesRequest
              )
          ]
      )

instance Prelude.ToPath RemoveAttributes where
  toPath RemoveAttributes' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/attributes/",
        Prelude.toBS attributeType
      ]

instance Prelude.ToQuery RemoveAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAttributesResponse' smart constructor.
data RemoveAttributesResponse = RemoveAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    attributesResource :: AttributesResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeAttributesResponse_httpStatus' - The response's http status code.
--
-- 'attributesResource', 'removeAttributesResponse_attributesResource' - Undocumented member.
newRemoveAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'attributesResource'
  AttributesResource ->
  RemoveAttributesResponse
newRemoveAttributesResponse
  pHttpStatus_
  pAttributesResource_ =
    RemoveAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        attributesResource = pAttributesResource_
      }

-- | The response's http status code.
removeAttributesResponse_httpStatus :: Lens.Lens' RemoveAttributesResponse Prelude.Int
removeAttributesResponse_httpStatus = Lens.lens (\RemoveAttributesResponse' {httpStatus} -> httpStatus) (\s@RemoveAttributesResponse' {} a -> s {httpStatus = a} :: RemoveAttributesResponse)

-- | Undocumented member.
removeAttributesResponse_attributesResource :: Lens.Lens' RemoveAttributesResponse AttributesResource
removeAttributesResponse_attributesResource = Lens.lens (\RemoveAttributesResponse' {attributesResource} -> attributesResource) (\s@RemoveAttributesResponse' {} a -> s {attributesResource = a} :: RemoveAttributesResponse)

instance Prelude.NFData RemoveAttributesResponse
