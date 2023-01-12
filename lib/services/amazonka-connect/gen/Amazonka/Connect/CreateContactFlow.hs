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
-- Module      : Amazonka.Connect.CreateContactFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow for the specified Amazon Connect instance.
--
-- You can also create and update flows using the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/flow-language.html Amazon Connect Flow language>.
module Amazonka.Connect.CreateContactFlow
  ( -- * Creating a Request
    CreateContactFlow (..),
    newCreateContactFlow,

    -- * Request Lenses
    createContactFlow_description,
    createContactFlow_tags,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContactFlow' smart constructor.
data CreateContactFlow = CreateContactFlow'
  { -- | The description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The name of the flow.
    name :: Prelude.Text,
    -- | The type of the flow. For descriptions of the available types, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
    -- in the /Amazon Connect Administrator Guide/.
    type' :: ContactFlowType,
    -- | The content of the flow.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createContactFlow_description' - The description of the flow.
--
-- 'tags', 'createContactFlow_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'instanceId', 'createContactFlow_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'name', 'createContactFlow_name' - The name of the flow.
--
-- 'type'', 'createContactFlow_type' - The type of the flow. For descriptions of the available types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
-- in the /Amazon Connect Administrator Guide/.
--
-- 'content', 'createContactFlow_content' - The content of the flow.
newCreateContactFlow ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ContactFlowType ->
  -- | 'content'
  Prelude.Text ->
  CreateContactFlow
newCreateContactFlow
  pInstanceId_
  pName_
  pType_
  pContent_ =
    CreateContactFlow'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        type' = pType_,
        content = pContent_
      }

-- | The description of the flow.
createContactFlow_description :: Lens.Lens' CreateContactFlow (Prelude.Maybe Prelude.Text)
createContactFlow_description = Lens.lens (\CreateContactFlow' {description} -> description) (\s@CreateContactFlow' {} a -> s {description = a} :: CreateContactFlow)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createContactFlow_tags :: Lens.Lens' CreateContactFlow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createContactFlow_tags = Lens.lens (\CreateContactFlow' {tags} -> tags) (\s@CreateContactFlow' {} a -> s {tags = a} :: CreateContactFlow) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance.
createContactFlow_instanceId :: Lens.Lens' CreateContactFlow Prelude.Text
createContactFlow_instanceId = Lens.lens (\CreateContactFlow' {instanceId} -> instanceId) (\s@CreateContactFlow' {} a -> s {instanceId = a} :: CreateContactFlow)

-- | The name of the flow.
createContactFlow_name :: Lens.Lens' CreateContactFlow Prelude.Text
createContactFlow_name = Lens.lens (\CreateContactFlow' {name} -> name) (\s@CreateContactFlow' {} a -> s {name = a} :: CreateContactFlow)

-- | The type of the flow. For descriptions of the available types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
-- in the /Amazon Connect Administrator Guide/.
createContactFlow_type :: Lens.Lens' CreateContactFlow ContactFlowType
createContactFlow_type = Lens.lens (\CreateContactFlow' {type'} -> type') (\s@CreateContactFlow' {} a -> s {type' = a} :: CreateContactFlow)

-- | The content of the flow.
createContactFlow_content :: Lens.Lens' CreateContactFlow Prelude.Text
createContactFlow_content = Lens.lens (\CreateContactFlow' {content} -> content) (\s@CreateContactFlow' {} a -> s {content = a} :: CreateContactFlow)

instance Core.AWSRequest CreateContactFlow where
  type
    AWSResponse CreateContactFlow =
      CreateContactFlowResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactFlowResponse'
            Prelude.<$> (x Data..?> "ContactFlowArn")
            Prelude.<*> (x Data..?> "ContactFlowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContactFlow where
  hashWithSalt _salt CreateContactFlow' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` content

instance Prelude.NFData CreateContactFlow where
  rnf CreateContactFlow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders CreateContactFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContactFlow where
  toJSON CreateContactFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath CreateContactFlow where
  toPath CreateContactFlow' {..} =
    Prelude.mconcat
      ["/contact-flows/", Data.toBS instanceId]

instance Data.ToQuery CreateContactFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactFlowResponse' smart constructor.
data CreateContactFlowResponse = CreateContactFlowResponse'
  { -- | The Amazon Resource Name (ARN) of the flow.
    contactFlowArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlowArn', 'createContactFlowResponse_contactFlowArn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'contactFlowId', 'createContactFlowResponse_contactFlowId' - The identifier of the flow.
--
-- 'httpStatus', 'createContactFlowResponse_httpStatus' - The response's http status code.
newCreateContactFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactFlowResponse
newCreateContactFlowResponse pHttpStatus_ =
  CreateContactFlowResponse'
    { contactFlowArn =
        Prelude.Nothing,
      contactFlowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the flow.
createContactFlowResponse_contactFlowArn :: Lens.Lens' CreateContactFlowResponse (Prelude.Maybe Prelude.Text)
createContactFlowResponse_contactFlowArn = Lens.lens (\CreateContactFlowResponse' {contactFlowArn} -> contactFlowArn) (\s@CreateContactFlowResponse' {} a -> s {contactFlowArn = a} :: CreateContactFlowResponse)

-- | The identifier of the flow.
createContactFlowResponse_contactFlowId :: Lens.Lens' CreateContactFlowResponse (Prelude.Maybe Prelude.Text)
createContactFlowResponse_contactFlowId = Lens.lens (\CreateContactFlowResponse' {contactFlowId} -> contactFlowId) (\s@CreateContactFlowResponse' {} a -> s {contactFlowId = a} :: CreateContactFlowResponse)

-- | The response's http status code.
createContactFlowResponse_httpStatus :: Lens.Lens' CreateContactFlowResponse Prelude.Int
createContactFlowResponse_httpStatus = Lens.lens (\CreateContactFlowResponse' {httpStatus} -> httpStatus) (\s@CreateContactFlowResponse' {} a -> s {httpStatus = a} :: CreateContactFlowResponse)

instance Prelude.NFData CreateContactFlowResponse where
  rnf CreateContactFlowResponse' {..} =
    Prelude.rnf contactFlowArn
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf httpStatus
