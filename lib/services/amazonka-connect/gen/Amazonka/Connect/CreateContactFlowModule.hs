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
-- Module      : Amazonka.Connect.CreateContactFlowModule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow module for the specified Amazon Connect instance.
module Amazonka.Connect.CreateContactFlowModule
  ( -- * Creating a Request
    CreateContactFlowModule (..),
    newCreateContactFlowModule,

    -- * Request Lenses
    createContactFlowModule_tags,
    createContactFlowModule_clientToken,
    createContactFlowModule_description,
    createContactFlowModule_instanceId,
    createContactFlowModule_name,
    createContactFlowModule_content,

    -- * Destructuring the Response
    CreateContactFlowModuleResponse (..),
    newCreateContactFlowModuleResponse,

    -- * Response Lenses
    createContactFlowModuleResponse_arn,
    createContactFlowModuleResponse_id,
    createContactFlowModuleResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContactFlowModule' smart constructor.
data CreateContactFlowModule = CreateContactFlowModule'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the flow module.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the flow module.
    name :: Prelude.Text,
    -- | The content of the flow module.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactFlowModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContactFlowModule_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'clientToken', 'createContactFlowModule_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'description', 'createContactFlowModule_description' - The description of the flow module.
--
-- 'instanceId', 'createContactFlowModule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createContactFlowModule_name' - The name of the flow module.
--
-- 'content', 'createContactFlowModule_content' - The content of the flow module.
newCreateContactFlowModule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  CreateContactFlowModule
newCreateContactFlowModule
  pInstanceId_
  pName_
  pContent_ =
    CreateContactFlowModule'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        content = pContent_
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createContactFlowModule_tags :: Lens.Lens' CreateContactFlowModule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createContactFlowModule_tags = Lens.lens (\CreateContactFlowModule' {tags} -> tags) (\s@CreateContactFlowModule' {} a -> s {tags = a} :: CreateContactFlowModule) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createContactFlowModule_clientToken :: Lens.Lens' CreateContactFlowModule (Prelude.Maybe Prelude.Text)
createContactFlowModule_clientToken = Lens.lens (\CreateContactFlowModule' {clientToken} -> clientToken) (\s@CreateContactFlowModule' {} a -> s {clientToken = a} :: CreateContactFlowModule)

-- | The description of the flow module.
createContactFlowModule_description :: Lens.Lens' CreateContactFlowModule (Prelude.Maybe Prelude.Text)
createContactFlowModule_description = Lens.lens (\CreateContactFlowModule' {description} -> description) (\s@CreateContactFlowModule' {} a -> s {description = a} :: CreateContactFlowModule)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createContactFlowModule_instanceId :: Lens.Lens' CreateContactFlowModule Prelude.Text
createContactFlowModule_instanceId = Lens.lens (\CreateContactFlowModule' {instanceId} -> instanceId) (\s@CreateContactFlowModule' {} a -> s {instanceId = a} :: CreateContactFlowModule)

-- | The name of the flow module.
createContactFlowModule_name :: Lens.Lens' CreateContactFlowModule Prelude.Text
createContactFlowModule_name = Lens.lens (\CreateContactFlowModule' {name} -> name) (\s@CreateContactFlowModule' {} a -> s {name = a} :: CreateContactFlowModule)

-- | The content of the flow module.
createContactFlowModule_content :: Lens.Lens' CreateContactFlowModule Prelude.Text
createContactFlowModule_content = Lens.lens (\CreateContactFlowModule' {content} -> content) (\s@CreateContactFlowModule' {} a -> s {content = a} :: CreateContactFlowModule)

instance Core.AWSRequest CreateContactFlowModule where
  type
    AWSResponse CreateContactFlowModule =
      CreateContactFlowModuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactFlowModuleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContactFlowModule where
  hashWithSalt _salt CreateContactFlowModule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` content

instance Prelude.NFData CreateContactFlowModule where
  rnf CreateContactFlowModule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders CreateContactFlowModule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContactFlowModule where
  toJSON CreateContactFlowModule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath CreateContactFlowModule where
  toPath CreateContactFlowModule' {..} =
    Prelude.mconcat
      ["/contact-flow-modules/", Data.toBS instanceId]

instance Data.ToQuery CreateContactFlowModule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactFlowModuleResponse' smart constructor.
data CreateContactFlowModuleResponse = CreateContactFlowModuleResponse'
  { -- | The Amazon Resource Name (ARN) of the flow module.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow module.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactFlowModuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createContactFlowModuleResponse_arn' - The Amazon Resource Name (ARN) of the flow module.
--
-- 'id', 'createContactFlowModuleResponse_id' - The identifier of the flow module.
--
-- 'httpStatus', 'createContactFlowModuleResponse_httpStatus' - The response's http status code.
newCreateContactFlowModuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactFlowModuleResponse
newCreateContactFlowModuleResponse pHttpStatus_ =
  CreateContactFlowModuleResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the flow module.
createContactFlowModuleResponse_arn :: Lens.Lens' CreateContactFlowModuleResponse (Prelude.Maybe Prelude.Text)
createContactFlowModuleResponse_arn = Lens.lens (\CreateContactFlowModuleResponse' {arn} -> arn) (\s@CreateContactFlowModuleResponse' {} a -> s {arn = a} :: CreateContactFlowModuleResponse)

-- | The identifier of the flow module.
createContactFlowModuleResponse_id :: Lens.Lens' CreateContactFlowModuleResponse (Prelude.Maybe Prelude.Text)
createContactFlowModuleResponse_id = Lens.lens (\CreateContactFlowModuleResponse' {id} -> id) (\s@CreateContactFlowModuleResponse' {} a -> s {id = a} :: CreateContactFlowModuleResponse)

-- | The response's http status code.
createContactFlowModuleResponse_httpStatus :: Lens.Lens' CreateContactFlowModuleResponse Prelude.Int
createContactFlowModuleResponse_httpStatus = Lens.lens (\CreateContactFlowModuleResponse' {httpStatus} -> httpStatus) (\s@CreateContactFlowModuleResponse' {} a -> s {httpStatus = a} :: CreateContactFlowModuleResponse)

instance
  Prelude.NFData
    CreateContactFlowModuleResponse
  where
  rnf CreateContactFlowModuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
