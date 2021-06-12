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
-- Module      : Network.AWS.Connect.CreateContactFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact flow for the specified Amazon Connect instance.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Network.AWS.Connect.CreateContactFlow
  ( -- * Creating a Request
    CreateContactFlow (..),
    newCreateContactFlow,

    -- * Request Lenses
    createContactFlow_tags,
    createContactFlow_description,
    createContactFlow_instanceId,
    createContactFlow_name,
    createContactFlow_type,
    createContactFlow_content,

    -- * Destructuring the Response
    CreateContactFlowResponse (..),
    newCreateContactFlowResponse,

    -- * Response Lenses
    createContactFlowResponse_contactFlowArn,
    createContactFlowResponse_contactFlowId,
    createContactFlowResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContactFlow' smart constructor.
data CreateContactFlow = CreateContactFlow'
  { -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the contact flow.
    description :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The name of the contact flow.
    name :: Core.Text,
    -- | The type of the contact flow. For descriptions of the available types,
    -- see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
    -- in the /Amazon Connect Administrator Guide/.
    type' :: ContactFlowType,
    -- | The content of the contact flow.
    content :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContactFlow_tags' - One or more tags.
--
-- 'description', 'createContactFlow_description' - The description of the contact flow.
--
-- 'instanceId', 'createContactFlow_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'name', 'createContactFlow_name' - The name of the contact flow.
--
-- 'type'', 'createContactFlow_type' - The type of the contact flow. For descriptions of the available types,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
-- in the /Amazon Connect Administrator Guide/.
--
-- 'content', 'createContactFlow_content' - The content of the contact flow.
newCreateContactFlow ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'type''
  ContactFlowType ->
  -- | 'content'
  Core.Text ->
  CreateContactFlow
newCreateContactFlow
  pInstanceId_
  pName_
  pType_
  pContent_ =
    CreateContactFlow'
      { tags = Core.Nothing,
        description = Core.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        type' = pType_,
        content = pContent_
      }

-- | One or more tags.
createContactFlow_tags :: Lens.Lens' CreateContactFlow (Core.Maybe (Core.HashMap Core.Text Core.Text))
createContactFlow_tags = Lens.lens (\CreateContactFlow' {tags} -> tags) (\s@CreateContactFlow' {} a -> s {tags = a} :: CreateContactFlow) Core.. Lens.mapping Lens._Coerce

-- | The description of the contact flow.
createContactFlow_description :: Lens.Lens' CreateContactFlow (Core.Maybe Core.Text)
createContactFlow_description = Lens.lens (\CreateContactFlow' {description} -> description) (\s@CreateContactFlow' {} a -> s {description = a} :: CreateContactFlow)

-- | The identifier of the Amazon Connect instance.
createContactFlow_instanceId :: Lens.Lens' CreateContactFlow Core.Text
createContactFlow_instanceId = Lens.lens (\CreateContactFlow' {instanceId} -> instanceId) (\s@CreateContactFlow' {} a -> s {instanceId = a} :: CreateContactFlow)

-- | The name of the contact flow.
createContactFlow_name :: Lens.Lens' CreateContactFlow Core.Text
createContactFlow_name = Lens.lens (\CreateContactFlow' {name} -> name) (\s@CreateContactFlow' {} a -> s {name = a} :: CreateContactFlow)

-- | The type of the contact flow. For descriptions of the available types,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
-- in the /Amazon Connect Administrator Guide/.
createContactFlow_type :: Lens.Lens' CreateContactFlow ContactFlowType
createContactFlow_type = Lens.lens (\CreateContactFlow' {type'} -> type') (\s@CreateContactFlow' {} a -> s {type' = a} :: CreateContactFlow)

-- | The content of the contact flow.
createContactFlow_content :: Lens.Lens' CreateContactFlow Core.Text
createContactFlow_content = Lens.lens (\CreateContactFlow' {content} -> content) (\s@CreateContactFlow' {} a -> s {content = a} :: CreateContactFlow)

instance Core.AWSRequest CreateContactFlow where
  type
    AWSResponse CreateContactFlow =
      CreateContactFlowResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactFlowResponse'
            Core.<$> (x Core..?> "ContactFlowArn")
            Core.<*> (x Core..?> "ContactFlowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateContactFlow

instance Core.NFData CreateContactFlow

instance Core.ToHeaders CreateContactFlow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateContactFlow where
  toJSON CreateContactFlow' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            Core.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath CreateContactFlow where
  toPath CreateContactFlow' {..} =
    Core.mconcat
      ["/contact-flows/", Core.toBS instanceId]

instance Core.ToQuery CreateContactFlow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateContactFlowResponse' smart constructor.
data CreateContactFlowResponse = CreateContactFlowResponse'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    contactFlowArn :: Core.Maybe Core.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlowArn', 'createContactFlowResponse_contactFlowArn' - The Amazon Resource Name (ARN) of the contact flow.
--
-- 'contactFlowId', 'createContactFlowResponse_contactFlowId' - The identifier of the contact flow.
--
-- 'httpStatus', 'createContactFlowResponse_httpStatus' - The response's http status code.
newCreateContactFlowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateContactFlowResponse
newCreateContactFlowResponse pHttpStatus_ =
  CreateContactFlowResponse'
    { contactFlowArn =
        Core.Nothing,
      contactFlowId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
createContactFlowResponse_contactFlowArn :: Lens.Lens' CreateContactFlowResponse (Core.Maybe Core.Text)
createContactFlowResponse_contactFlowArn = Lens.lens (\CreateContactFlowResponse' {contactFlowArn} -> contactFlowArn) (\s@CreateContactFlowResponse' {} a -> s {contactFlowArn = a} :: CreateContactFlowResponse)

-- | The identifier of the contact flow.
createContactFlowResponse_contactFlowId :: Lens.Lens' CreateContactFlowResponse (Core.Maybe Core.Text)
createContactFlowResponse_contactFlowId = Lens.lens (\CreateContactFlowResponse' {contactFlowId} -> contactFlowId) (\s@CreateContactFlowResponse' {} a -> s {contactFlowId = a} :: CreateContactFlowResponse)

-- | The response's http status code.
createContactFlowResponse_httpStatus :: Lens.Lens' CreateContactFlowResponse Core.Int
createContactFlowResponse_httpStatus = Lens.lens (\CreateContactFlowResponse' {httpStatus} -> httpStatus) (\s@CreateContactFlowResponse' {} a -> s {httpStatus = a} :: CreateContactFlowResponse)

instance Core.NFData CreateContactFlowResponse
