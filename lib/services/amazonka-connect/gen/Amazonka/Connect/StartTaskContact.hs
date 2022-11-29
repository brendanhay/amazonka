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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a flow to start a new task.
module Amazonka.Connect.StartTaskContact
  ( -- * Creating a Request
    StartTaskContact (..),
    newStartTaskContact,

    -- * Request Lenses
    startTaskContact_clientToken,
    startTaskContact_taskTemplateId,
    startTaskContact_description,
    startTaskContact_references,
    startTaskContact_quickConnectId,
    startTaskContact_attributes,
    startTaskContact_previousContactId,
    startTaskContact_contactFlowId,
    startTaskContact_scheduledTime,
    startTaskContact_instanceId,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTaskContact' smart constructor.
data StartTaskContact = StartTaskContact'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the task template.
    taskTemplateId :: Prelude.Maybe Prelude.Text,
    -- | A description of the task that is shown to an agent in the Contact
    -- Control Panel (CCP).
    description :: Prelude.Maybe Prelude.Text,
    -- | A formatted URL that is shown to an agent in the Contact Control Panel
    -- (CCP).
    references :: Prelude.Maybe (Prelude.HashMap Prelude.Text Reference),
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Maybe Prelude.Text,
    -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes, and can be accessed in flows just
    -- like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the previous chat, voice, or task contact.
    previousContactId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow for initiating the tasks. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
    -- On the flow page, under the name of the flow, choose __Show additional
    -- flow information__. The ContactFlowId is the last part of the ARN, shown
    -- here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp, in Unix Epoch seconds format, at which to start running
    -- the inbound flow. The scheduled time cannot be in the past. It must be
    -- within up to 6 days in future.
    scheduledTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
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
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'taskTemplateId', 'startTaskContact_taskTemplateId' - A unique identifier for the task template.
--
-- 'description', 'startTaskContact_description' - A description of the task that is shown to an agent in the Contact
-- Control Panel (CCP).
--
-- 'references', 'startTaskContact_references' - A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
--
-- 'quickConnectId', 'startTaskContact_quickConnectId' - The identifier for the quick connect.
--
-- 'attributes', 'startTaskContact_attributes' - A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
--
-- 'previousContactId', 'startTaskContact_previousContactId' - The identifier of the previous chat, voice, or task contact.
--
-- 'contactFlowId', 'startTaskContact_contactFlowId' - The identifier of the flow for initiating the tasks. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- 'scheduledTime', 'startTaskContact_scheduledTime' - The timestamp, in Unix Epoch seconds format, at which to start running
-- the inbound flow. The scheduled time cannot be in the past. It must be
-- within up to 6 days in future.
--
-- 'instanceId', 'startTaskContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'startTaskContact_name' - The name of a task that is shown to an agent in the Contact Control
-- Panel (CCP).
newStartTaskContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  StartTaskContact
newStartTaskContact pInstanceId_ pName_ =
  StartTaskContact'
    { clientToken = Prelude.Nothing,
      taskTemplateId = Prelude.Nothing,
      description = Prelude.Nothing,
      references = Prelude.Nothing,
      quickConnectId = Prelude.Nothing,
      attributes = Prelude.Nothing,
      previousContactId = Prelude.Nothing,
      contactFlowId = Prelude.Nothing,
      scheduledTime = Prelude.Nothing,
      instanceId = pInstanceId_,
      name = pName_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
startTaskContact_clientToken :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_clientToken = Lens.lens (\StartTaskContact' {clientToken} -> clientToken) (\s@StartTaskContact' {} a -> s {clientToken = a} :: StartTaskContact)

-- | A unique identifier for the task template.
startTaskContact_taskTemplateId :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_taskTemplateId = Lens.lens (\StartTaskContact' {taskTemplateId} -> taskTemplateId) (\s@StartTaskContact' {} a -> s {taskTemplateId = a} :: StartTaskContact)

-- | A description of the task that is shown to an agent in the Contact
-- Control Panel (CCP).
startTaskContact_description :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_description = Lens.lens (\StartTaskContact' {description} -> description) (\s@StartTaskContact' {} a -> s {description = a} :: StartTaskContact)

-- | A formatted URL that is shown to an agent in the Contact Control Panel
-- (CCP).
startTaskContact_references :: Lens.Lens' StartTaskContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Reference))
startTaskContact_references = Lens.lens (\StartTaskContact' {references} -> references) (\s@StartTaskContact' {} a -> s {references = a} :: StartTaskContact) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the quick connect.
startTaskContact_quickConnectId :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_quickConnectId = Lens.lens (\StartTaskContact' {quickConnectId} -> quickConnectId) (\s@StartTaskContact' {} a -> s {quickConnectId = a} :: StartTaskContact)

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startTaskContact_attributes :: Lens.Lens' StartTaskContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTaskContact_attributes = Lens.lens (\StartTaskContact' {attributes} -> attributes) (\s@StartTaskContact' {} a -> s {attributes = a} :: StartTaskContact) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the previous chat, voice, or task contact.
startTaskContact_previousContactId :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_previousContactId = Lens.lens (\StartTaskContact' {previousContactId} -> previousContactId) (\s@StartTaskContact' {} a -> s {previousContactId = a} :: StartTaskContact)

-- | The identifier of the flow for initiating the tasks. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startTaskContact_contactFlowId :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.Text)
startTaskContact_contactFlowId = Lens.lens (\StartTaskContact' {contactFlowId} -> contactFlowId) (\s@StartTaskContact' {} a -> s {contactFlowId = a} :: StartTaskContact)

-- | The timestamp, in Unix Epoch seconds format, at which to start running
-- the inbound flow. The scheduled time cannot be in the past. It must be
-- within up to 6 days in future.
startTaskContact_scheduledTime :: Lens.Lens' StartTaskContact (Prelude.Maybe Prelude.UTCTime)
startTaskContact_scheduledTime = Lens.lens (\StartTaskContact' {scheduledTime} -> scheduledTime) (\s@StartTaskContact' {} a -> s {scheduledTime = a} :: StartTaskContact) Prelude.. Lens.mapping Core._Time

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
startTaskContact_instanceId :: Lens.Lens' StartTaskContact Prelude.Text
startTaskContact_instanceId = Lens.lens (\StartTaskContact' {instanceId} -> instanceId) (\s@StartTaskContact' {} a -> s {instanceId = a} :: StartTaskContact)

-- | The name of a task that is shown to an agent in the Contact Control
-- Panel (CCP).
startTaskContact_name :: Lens.Lens' StartTaskContact Prelude.Text
startTaskContact_name = Lens.lens (\StartTaskContact' {name} -> name) (\s@StartTaskContact' {} a -> s {name = a} :: StartTaskContact)

instance Core.AWSRequest StartTaskContact where
  type
    AWSResponse StartTaskContact =
      StartTaskContactResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTaskContactResponse'
            Prelude.<$> (x Core..?> "ContactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTaskContact where
  hashWithSalt _salt StartTaskContact' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` taskTemplateId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` references
      `Prelude.hashWithSalt` quickConnectId
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` previousContactId
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` scheduledTime
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartTaskContact where
  rnf StartTaskContact' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf taskTemplateId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf references
      `Prelude.seq` Prelude.rnf quickConnectId
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf previousContactId
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf scheduledTime
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name

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
            ("TaskTemplateId" Core..=)
              Prelude.<$> taskTemplateId,
            ("Description" Core..=) Prelude.<$> description,
            ("References" Core..=) Prelude.<$> references,
            ("QuickConnectId" Core..=)
              Prelude.<$> quickConnectId,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("PreviousContactId" Core..=)
              Prelude.<$> previousContactId,
            ("ContactFlowId" Core..=) Prelude.<$> contactFlowId,
            ("ScheduledTime" Core..=) Prelude.<$> scheduledTime,
            Prelude.Just ("InstanceId" Core..= instanceId),
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
