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
-- Module      : Amazonka.Connect.UpdateContactAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates user-defined contact attributes associated with the
-- specified contact.
--
-- You can create or update user-defined attributes for both ongoing and
-- completed contacts. For example, while the call is active, you can
-- update the customer\'s name or the reason the customer called. You can
-- add notes about steps that the agent took during the call that display
-- to the next agent that takes the call. You can also update attributes
-- for a contact using data from your CRM application and save the data
-- with the contact in Amazon Connect. You could also flag calls for
-- additional analysis, such as legal review or to identify abusive
-- callers.
--
-- Contact attributes are available in Amazon Connect for 24 months, and
-- are then deleted. For information about contact record retention and the
-- maximum size of the contact record attributes section, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.UpdateContactAttributes
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactAttributes' smart constructor.
data UpdateContactAttributes = UpdateContactAttributes'
  { -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The Amazon Connect attributes. These attributes can be accessed in flows
    -- just like any other contact attributes.
    --
    -- You can have up to 32,768 UTF-8 bytes across all attributes for a
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'instanceId', 'updateContactAttributes_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'attributes', 'updateContactAttributes_attributes' - The Amazon Connect attributes. These attributes can be accessed in flows
-- just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
newUpdateContactAttributes ::
  -- | 'initialContactId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateContactAttributes
newUpdateContactAttributes
  pInitialContactId_
  pInstanceId_ =
    UpdateContactAttributes'
      { initialContactId =
          pInitialContactId_,
        instanceId = pInstanceId_,
        attributes = Prelude.mempty
      }

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
updateContactAttributes_initialContactId :: Lens.Lens' UpdateContactAttributes Prelude.Text
updateContactAttributes_initialContactId = Lens.lens (\UpdateContactAttributes' {initialContactId} -> initialContactId) (\s@UpdateContactAttributes' {} a -> s {initialContactId = a} :: UpdateContactAttributes)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateContactAttributes_instanceId :: Lens.Lens' UpdateContactAttributes Prelude.Text
updateContactAttributes_instanceId = Lens.lens (\UpdateContactAttributes' {instanceId} -> instanceId) (\s@UpdateContactAttributes' {} a -> s {instanceId = a} :: UpdateContactAttributes)

-- | The Amazon Connect attributes. These attributes can be accessed in flows
-- just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
updateContactAttributes_attributes :: Lens.Lens' UpdateContactAttributes (Prelude.HashMap Prelude.Text Prelude.Text)
updateContactAttributes_attributes = Lens.lens (\UpdateContactAttributes' {attributes} -> attributes) (\s@UpdateContactAttributes' {} a -> s {attributes = a} :: UpdateContactAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateContactAttributes where
  type
    AWSResponse UpdateContactAttributes =
      UpdateContactAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContactAttributes where
  hashWithSalt _salt UpdateContactAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` initialContactId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData UpdateContactAttributes where
  rnf UpdateContactAttributes' {..} =
    Prelude.rnf initialContactId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders UpdateContactAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContactAttributes where
  toJSON UpdateContactAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InitialContactId" Data..= initialContactId),
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("Attributes" Data..= attributes)
          ]
      )

instance Data.ToPath UpdateContactAttributes where
  toPath = Prelude.const "/contact/attributes"

instance Data.ToQuery UpdateContactAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactAttributesResponse' smart constructor.
data UpdateContactAttributesResponse = UpdateContactAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateContactAttributesResponse
newUpdateContactAttributesResponse pHttpStatus_ =
  UpdateContactAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateContactAttributesResponse_httpStatus :: Lens.Lens' UpdateContactAttributesResponse Prelude.Int
updateContactAttributesResponse_httpStatus = Lens.lens (\UpdateContactAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateContactAttributesResponse' {} a -> s {httpStatus = a} :: UpdateContactAttributesResponse)

instance
  Prelude.NFData
    UpdateContactAttributesResponse
  where
  rnf UpdateContactAttributesResponse' {..} =
    Prelude.rnf httpStatus
