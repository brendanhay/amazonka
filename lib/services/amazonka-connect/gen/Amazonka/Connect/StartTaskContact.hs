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
-- Module      : Amazonka.Connect.StartTaskContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a contact flow to start a new task.
module Amazonka.Connect.StartTaskContact
  ( -- * Creating a Request
    StartTaskContact (..),
    newStartTaskContact,

    -- * Request Lenses
    startTaskContact_clientToken,
    startTaskContact_references,
    startTaskContact_previousContactId,
    startTaskContact_attributes,
    startTaskContact_description,
    startTaskContact_instanceId,
    startTaskContact_contactFlowId,
    startTaskContact_name,

    -- * Destructuring the Response
    StartTaskContactResponse (..),
    newStartTaskContactResponse,

    -- * Response Lenses
    startTaskContactResponse_contactId,
    startTaskContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTaskContact' smart constructor.
data StartTaskContact = StartTaskContact'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A formatted URL that is shown to an agent in the Contact Control Panel
    -- (CCP).
    references :: Prelude.Maybe (Prelude.HashMap Prelude.Text Reference),
    -- | The identifier of the previous chat, voice, or task contact.
    previousContactId :: Prelude.Maybe Prelude.Text,
    -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes, and can be accessed in contact flows
    -- just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the task that is shown to an agent in the Contact
    -- Control Panel (CCP).
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact flow for initiating the tasks. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
    -- flow. On the contact flow page, under the name of the contact flow,
    -- choose __Show additional flow information__. The ContactFlowId is the
    -- last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Prelude.Text,
    -- | The name of a task that is shown to an agent in the Contact Control
    -- Panel (CCP).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTaskContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startTaskContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'references', 'startTaskContact_references' - A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
--
-- 'previousContactId', 'startTaskContact_previousContactId' - The identifier of the previous chat, voice, or task contact.
--
-- 'attributes', 'startTaskContact_attributes' - A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in contact flows
-- just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
--
-- 'description', 'startTaskContact_description' - A description of the task that is shown to an agent in the Contact
-- Control Panel (CCP).
--
-- 'instanceId', 'startTaskContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowId', 'startTaskContact_contactFlowId' - The identifier of the contact flow for initiating the tasks. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
-- flow. On the contact flow page, under the name of the contact flow,
-- choose __Show additional flow information__. The ContactFlowId is the
-- last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- 'name', 'startTaskContact_name' - The name of a task that is shown to an agent in the Contact Control
-- Panel (CCP).
newStartTaskContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  StartTaskContact
newStartTaskContact
  pInstanceId_
  pContactFlowId_
  pName_ =
    StartTaskContact'
      { clientToken = Prelude.Nothing,
        references = Prelude.Nothing,
        previousContactId = Prelude.Nothing,
        attributes = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactFlowId = pContactFlowId_,
        name = pName_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
startTaskContact_clientToken :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_clientToken = Lens.lens (\StartTaskContact' {clientToken} -> clientToken) (\s@StartTaskContact' {} a -> s {clientToken = a} :: StartTaskContact)

-- | A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
startTaskContact_references :: Lens.Lens' StartTaskContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Reference))
startTaskContact_references = Lens.lens (\StartTaskContact' {references} -> references) (\s@StartTaskContact' {} a -> s {references = a} :: StartTaskContact) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the previous chat, voice, or task contact.
startTaskContact_previousContactId :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_previousContactId = Lens.lens (\StartTaskContact' {previousContactId} -> previousContactId) (\s@StartTaskContact' {} a -> s {previousContactId = a} :: StartTaskContact)

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in contact flows
-- just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startTaskContact_attributes :: Lens.Lens' StartTaskContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTaskContact_attributes = Lens.lens (\StartTaskContact' {attributes} -> attributes) (\s@StartTaskContact' {} a -> s {attributes = a} :: StartTaskContact) Prelude.. Lens.mapping Lens.coerced

-- | A description of the task that is shown to an agent in the Contact
-- Control Panel (CCP).
startTaskContact_description :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_description = Lens.lens (\StartTaskContact' {description} -> description) (\s@StartTaskContact' {} a -> s {description = a} :: StartTaskContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
startTaskContact_instanceId :: Lens.Lens' StartTaskContact Prelude.Text
startTaskContact_instanceId = Lens.lens (\StartTaskContact' {instanceId} -> instanceId) (\s@StartTaskContact' {} a -> s {instanceId = a} :: StartTaskContact)

-- | The identifier of the contact flow for initiating the tasks. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
-- flow. On the contact flow page, under the name of the contact flow,
-- choose __Show additional flow information__. The ContactFlowId is the
-- last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startTaskContact_contactFlowId :: Lens.Lens' StartTaskContact Prelude.Text
startTaskContact_contactFlowId = Lens.lens (\StartTaskContact' {contactFlowId} -> contactFlowId) (\s@StartTaskContact' {} a -> s {contactFlowId = a} :: StartTaskContact)

-- | The name of a task that is shown to an agent in the Contact Control
-- Panel (CCP).
startTaskContact_name :: Lens.Lens' StartTaskContact Prelude.Text
startTaskContact_name = Lens.lens (\StartTaskContact' {name} -> name) (\s@StartTaskContact' {} a -> s {name = a} :: StartTaskContact)

instance Core.AWSRequest StartTaskContact where
  type
    AWSResponse StartTaskContact =
      StartTaskContactResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTaskContactResponse'
            Prelude.<$> (x Core..?> "ContactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTaskContact where
  hashWithSalt salt' StartTaskContact' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` previousContactId
      `Prelude.hashWithSalt` references
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartTaskContact where
  rnf StartTaskContact' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf previousContactId
      `Prelude.seq` Prelude.rnf references

instance Core.ToHeaders StartTaskContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartTaskContact where
  toJSON StartTaskContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("References" Core..=) Prelude.<$> references,
            ("PreviousContactId" Core..=)
              Prelude.<$> previousContactId,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just ("ContactFlowId" Core..= contactFlowId),
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath StartTaskContact where
  toPath = Prelude.const "/contact/task"

instance Core.ToQuery StartTaskContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTaskContactResponse' smart constructor.
data StartTaskContactResponse = StartTaskContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTaskContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'startTaskContactResponse_contactId' - The identifier of this contact within the Amazon Connect instance.
--
-- 'httpStatus', 'startTaskContactResponse_httpStatus' - The response's http status code.
newStartTaskContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTaskContactResponse
newStartTaskContactResponse pHttpStatus_ =
  StartTaskContactResponse'
    { contactId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
startTaskContactResponse_contactId :: Lens.Lens' StartTaskContactResponse (Prelude.Maybe Prelude.Text)
startTaskContactResponse_contactId = Lens.lens (\StartTaskContactResponse' {contactId} -> contactId) (\s@StartTaskContactResponse' {} a -> s {contactId = a} :: StartTaskContactResponse)

-- | The response's http status code.
startTaskContactResponse_httpStatus :: Lens.Lens' StartTaskContactResponse Prelude.Int
startTaskContactResponse_httpStatus = Lens.lens (\StartTaskContactResponse' {httpStatus} -> httpStatus) (\s@StartTaskContactResponse' {} a -> s {httpStatus = a} :: StartTaskContactResponse)

instance Prelude.NFData StartTaskContactResponse where
  rnf StartTaskContactResponse' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf httpStatus
