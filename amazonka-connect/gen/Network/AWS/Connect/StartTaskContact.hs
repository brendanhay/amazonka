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
-- Module      : Network.AWS.Connect.StartTaskContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a contact flow to start a new task.
module Network.AWS.Connect.StartTaskContact
  ( -- * Creating a Request
    StartTaskContact (..),
    newStartTaskContact,

    -- * Request Lenses
    startTaskContact_previousContactId,
    startTaskContact_references,
    startTaskContact_attributes,
    startTaskContact_description,
    startTaskContact_clientToken,
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartTaskContact' smart constructor.
data StartTaskContact = StartTaskContact'
  { -- | The identifier of the previous chat, voice, or task contact.
    previousContactId :: Core.Maybe Core.Text,
    -- | A formatted URL that is shown to an agent in the Contact Control Panel
    -- (CCP).
    references :: Core.Maybe (Core.HashMap Core.Text Reference),
    -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes, and can be accessed in contact flows
    -- just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A description of the task that is shown to an agent in the Contact
    -- Control Panel (CCP).
    description :: Core.Maybe Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the contact flow for initiating the tasks. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
    -- flow. On the contact flow page, under the name of the contact flow,
    -- choose __Show additional flow information__. The ContactFlowId is the
    -- last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Core.Text,
    -- | The name of a task that is shown to an agent in the Contact Control
    -- Panel (CCP).
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTaskContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'previousContactId', 'startTaskContact_previousContactId' - The identifier of the previous chat, voice, or task contact.
--
-- 'references', 'startTaskContact_references' - A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
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
-- 'clientToken', 'startTaskContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'instanceId', 'startTaskContact_instanceId' - The identifier of the Amazon Connect instance.
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
  Core.Text ->
  -- | 'contactFlowId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  StartTaskContact
newStartTaskContact
  pInstanceId_
  pContactFlowId_
  pName_ =
    StartTaskContact'
      { previousContactId = Core.Nothing,
        references = Core.Nothing,
        attributes = Core.Nothing,
        description = Core.Nothing,
        clientToken = Core.Nothing,
        instanceId = pInstanceId_,
        contactFlowId = pContactFlowId_,
        name = pName_
      }

-- | The identifier of the previous chat, voice, or task contact.
startTaskContact_previousContactId :: Lens.Lens' StartTaskContact (Core.Maybe Core.Text)
startTaskContact_previousContactId = Lens.lens (\StartTaskContact' {previousContactId} -> previousContactId) (\s@StartTaskContact' {} a -> s {previousContactId = a} :: StartTaskContact)

-- | A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
startTaskContact_references :: Lens.Lens' StartTaskContact (Core.Maybe (Core.HashMap Core.Text Reference))
startTaskContact_references = Lens.lens (\StartTaskContact' {references} -> references) (\s@StartTaskContact' {} a -> s {references = a} :: StartTaskContact) Core.. Lens.mapping Lens._Coerce

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in contact flows
-- just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startTaskContact_attributes :: Lens.Lens' StartTaskContact (Core.Maybe (Core.HashMap Core.Text Core.Text))
startTaskContact_attributes = Lens.lens (\StartTaskContact' {attributes} -> attributes) (\s@StartTaskContact' {} a -> s {attributes = a} :: StartTaskContact) Core.. Lens.mapping Lens._Coerce

-- | A description of the task that is shown to an agent in the Contact
-- Control Panel (CCP).
startTaskContact_description :: Lens.Lens' StartTaskContact (Core.Maybe Core.Text)
startTaskContact_description = Lens.lens (\StartTaskContact' {description} -> description) (\s@StartTaskContact' {} a -> s {description = a} :: StartTaskContact)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
startTaskContact_clientToken :: Lens.Lens' StartTaskContact (Core.Maybe Core.Text)
startTaskContact_clientToken = Lens.lens (\StartTaskContact' {clientToken} -> clientToken) (\s@StartTaskContact' {} a -> s {clientToken = a} :: StartTaskContact)

-- | The identifier of the Amazon Connect instance.
startTaskContact_instanceId :: Lens.Lens' StartTaskContact Core.Text
startTaskContact_instanceId = Lens.lens (\StartTaskContact' {instanceId} -> instanceId) (\s@StartTaskContact' {} a -> s {instanceId = a} :: StartTaskContact)

-- | The identifier of the contact flow for initiating the tasks. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
-- flow. On the contact flow page, under the name of the contact flow,
-- choose __Show additional flow information__. The ContactFlowId is the
-- last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startTaskContact_contactFlowId :: Lens.Lens' StartTaskContact Core.Text
startTaskContact_contactFlowId = Lens.lens (\StartTaskContact' {contactFlowId} -> contactFlowId) (\s@StartTaskContact' {} a -> s {contactFlowId = a} :: StartTaskContact)

-- | The name of a task that is shown to an agent in the Contact Control
-- Panel (CCP).
startTaskContact_name :: Lens.Lens' StartTaskContact Core.Text
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
            Core.<$> (x Core..?> "ContactId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartTaskContact

instance Core.NFData StartTaskContact

instance Core.ToHeaders StartTaskContact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartTaskContact where
  toJSON StartTaskContact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PreviousContactId" Core..=)
              Core.<$> previousContactId,
            ("References" Core..=) Core.<$> references,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Description" Core..=) Core.<$> description,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("ContactFlowId" Core..= contactFlowId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath StartTaskContact where
  toPath = Core.const "/contact/task"

instance Core.ToQuery StartTaskContact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartTaskContactResponse' smart constructor.
data StartTaskContactResponse = StartTaskContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartTaskContactResponse
newStartTaskContactResponse pHttpStatus_ =
  StartTaskContactResponse'
    { contactId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
startTaskContactResponse_contactId :: Lens.Lens' StartTaskContactResponse (Core.Maybe Core.Text)
startTaskContactResponse_contactId = Lens.lens (\StartTaskContactResponse' {contactId} -> contactId) (\s@StartTaskContactResponse' {} a -> s {contactId = a} :: StartTaskContactResponse)

-- | The response's http status code.
startTaskContactResponse_httpStatus :: Lens.Lens' StartTaskContactResponse Core.Int
startTaskContactResponse_httpStatus = Lens.lens (\StartTaskContactResponse' {httpStatus} -> httpStatus) (\s@StartTaskContactResponse' {} a -> s {httpStatus = a} :: StartTaskContactResponse)

instance Core.NFData StartTaskContactResponse
