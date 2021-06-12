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
-- Module      : Network.AWS.Connect.UpdateContactAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the contact attributes associated with the specified
-- contact.
--
-- You can add or update attributes for both ongoing and completed
-- contacts. For example, while the call is active, you can update the
-- customer\'s name or the reason the customer called. You can add notes
-- about steps that the agent took during the call that display to the next
-- agent that takes the call. You can also update attributes for a contact
-- using data from your CRM application and save the data with the contact
-- in Amazon Connect. You could also flag calls for additional analysis,
-- such as legal review or to identify abusive callers.
--
-- Contact attributes are available in Amazon Connect for 24 months, and
-- are then deleted.
--
-- __Important:__ You cannot use the operation to update attributes for
-- contacts that occurred prior to the release of the API, which was
-- September 12, 2018. You can update attributes only for contacts that
-- started after the release of the API. If you attempt to update
-- attributes for a contact that occurred prior to the release of the API,
-- a 400 error is returned. This applies also to queued callbacks that were
-- initiated prior to the release of the API but are still active in your
-- instance.
module Network.AWS.Connect.UpdateContactAttributes
  ( -- * Creating a Request
    UpdateContactAttributes (..),
    newUpdateContactAttributes,

    -- * Request Lenses
    updateContactAttributes_initialContactId,
    updateContactAttributes_instanceId,
    updateContactAttributes_attributes,

    -- * Destructuring the Response
    UpdateContactAttributesResponse (..),
    newUpdateContactAttributesResponse,

    -- * Response Lenses
    updateContactAttributesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContactAttributes' smart constructor.
data UpdateContactAttributes = UpdateContactAttributes'
  { -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The Amazon Connect attributes. These attributes can be accessed in
    -- contact flows just like any other contact attributes.
    --
    -- You can have up to 32,768 UTF-8 bytes across all attributes for a
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContactAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialContactId', 'updateContactAttributes_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
--
-- 'instanceId', 'updateContactAttributes_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'attributes', 'updateContactAttributes_attributes' - The Amazon Connect attributes. These attributes can be accessed in
-- contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
newUpdateContactAttributes ::
  -- | 'initialContactId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  UpdateContactAttributes
newUpdateContactAttributes
  pInitialContactId_
  pInstanceId_ =
    UpdateContactAttributes'
      { initialContactId =
          pInitialContactId_,
        instanceId = pInstanceId_,
        attributes = Core.mempty
      }

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
updateContactAttributes_initialContactId :: Lens.Lens' UpdateContactAttributes Core.Text
updateContactAttributes_initialContactId = Lens.lens (\UpdateContactAttributes' {initialContactId} -> initialContactId) (\s@UpdateContactAttributes' {} a -> s {initialContactId = a} :: UpdateContactAttributes)

-- | The identifier of the Amazon Connect instance.
updateContactAttributes_instanceId :: Lens.Lens' UpdateContactAttributes Core.Text
updateContactAttributes_instanceId = Lens.lens (\UpdateContactAttributes' {instanceId} -> instanceId) (\s@UpdateContactAttributes' {} a -> s {instanceId = a} :: UpdateContactAttributes)

-- | The Amazon Connect attributes. These attributes can be accessed in
-- contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
updateContactAttributes_attributes :: Lens.Lens' UpdateContactAttributes (Core.HashMap Core.Text Core.Text)
updateContactAttributes_attributes = Lens.lens (\UpdateContactAttributes' {attributes} -> attributes) (\s@UpdateContactAttributes' {} a -> s {attributes = a} :: UpdateContactAttributes) Core.. Lens._Coerce

instance Core.AWSRequest UpdateContactAttributes where
  type
    AWSResponse UpdateContactAttributes =
      UpdateContactAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContactAttributes

instance Core.NFData UpdateContactAttributes

instance Core.ToHeaders UpdateContactAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContactAttributes where
  toJSON UpdateContactAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("InitialContactId" Core..= initialContactId),
            Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("Attributes" Core..= attributes)
          ]
      )

instance Core.ToPath UpdateContactAttributes where
  toPath = Core.const "/contact/attributes"

instance Core.ToQuery UpdateContactAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContactAttributesResponse' smart constructor.
data UpdateContactAttributesResponse = UpdateContactAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContactAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactAttributesResponse_httpStatus' - The response's http status code.
newUpdateContactAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateContactAttributesResponse
newUpdateContactAttributesResponse pHttpStatus_ =
  UpdateContactAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateContactAttributesResponse_httpStatus :: Lens.Lens' UpdateContactAttributesResponse Core.Int
updateContactAttributesResponse_httpStatus = Lens.lens (\UpdateContactAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateContactAttributesResponse' {} a -> s {httpStatus = a} :: UpdateContactAttributesResponse)

instance Core.NFData UpdateContactAttributesResponse
