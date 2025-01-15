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
-- Module      : Amazonka.Connect.CreateTaskTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new task template in the specified Amazon Connect instance.
module Amazonka.Connect.CreateTaskTemplate
  ( -- * Creating a Request
    CreateTaskTemplate (..),
    newCreateTaskTemplate,

    -- * Request Lenses
    createTaskTemplate_clientToken,
    createTaskTemplate_constraints,
    createTaskTemplate_contactFlowId,
    createTaskTemplate_defaults,
    createTaskTemplate_description,
    createTaskTemplate_status,
    createTaskTemplate_instanceId,
    createTaskTemplate_name,
    createTaskTemplate_fields,

    -- * Destructuring the Response
    CreateTaskTemplateResponse (..),
    newCreateTaskTemplateResponse,

    -- * Response Lenses
    createTaskTemplateResponse_httpStatus,
    createTaskTemplateResponse_id,
    createTaskTemplateResponse_arn,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTaskTemplate' smart constructor.
data CreateTaskTemplate = CreateTaskTemplate'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Constraints that are applicable to the fields listed.
    constraints :: Prelude.Maybe TaskTemplateConstraints,
    -- | The identifier of the flow that runs by default when a task is created
    -- by referencing this template.
    contactFlowId :: Prelude.Maybe Prelude.Text,
    -- | The default values for fields when a task is created by referencing this
    -- template.
    defaults :: Prelude.Maybe TaskTemplateDefaults,
    -- | The description of the task template.
    description :: Prelude.Maybe Prelude.Text,
    -- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
    -- Tasks can only be created from @ACTIVE@ templates. If a template is
    -- marked as @INACTIVE@, then a task that refers to this template cannot be
    -- created.
    status :: Prelude.Maybe TaskTemplateStatus,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the task template.
    name :: Prelude.Text,
    -- | Fields that are part of the template.
    fields :: [TaskTemplateField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTaskTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTaskTemplate_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'constraints', 'createTaskTemplate_constraints' - Constraints that are applicable to the fields listed.
--
-- 'contactFlowId', 'createTaskTemplate_contactFlowId' - The identifier of the flow that runs by default when a task is created
-- by referencing this template.
--
-- 'defaults', 'createTaskTemplate_defaults' - The default values for fields when a task is created by referencing this
-- template.
--
-- 'description', 'createTaskTemplate_description' - The description of the task template.
--
-- 'status', 'createTaskTemplate_status' - Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
--
-- 'instanceId', 'createTaskTemplate_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createTaskTemplate_name' - The name of the task template.
--
-- 'fields', 'createTaskTemplate_fields' - Fields that are part of the template.
newCreateTaskTemplate ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateTaskTemplate
newCreateTaskTemplate pInstanceId_ pName_ =
  CreateTaskTemplate'
    { clientToken = Prelude.Nothing,
      constraints = Prelude.Nothing,
      contactFlowId = Prelude.Nothing,
      defaults = Prelude.Nothing,
      description = Prelude.Nothing,
      status = Prelude.Nothing,
      instanceId = pInstanceId_,
      name = pName_,
      fields = Prelude.mempty
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createTaskTemplate_clientToken :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe Prelude.Text)
createTaskTemplate_clientToken = Lens.lens (\CreateTaskTemplate' {clientToken} -> clientToken) (\s@CreateTaskTemplate' {} a -> s {clientToken = a} :: CreateTaskTemplate)

-- | Constraints that are applicable to the fields listed.
createTaskTemplate_constraints :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe TaskTemplateConstraints)
createTaskTemplate_constraints = Lens.lens (\CreateTaskTemplate' {constraints} -> constraints) (\s@CreateTaskTemplate' {} a -> s {constraints = a} :: CreateTaskTemplate)

-- | The identifier of the flow that runs by default when a task is created
-- by referencing this template.
createTaskTemplate_contactFlowId :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe Prelude.Text)
createTaskTemplate_contactFlowId = Lens.lens (\CreateTaskTemplate' {contactFlowId} -> contactFlowId) (\s@CreateTaskTemplate' {} a -> s {contactFlowId = a} :: CreateTaskTemplate)

-- | The default values for fields when a task is created by referencing this
-- template.
createTaskTemplate_defaults :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe TaskTemplateDefaults)
createTaskTemplate_defaults = Lens.lens (\CreateTaskTemplate' {defaults} -> defaults) (\s@CreateTaskTemplate' {} a -> s {defaults = a} :: CreateTaskTemplate)

-- | The description of the task template.
createTaskTemplate_description :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe Prelude.Text)
createTaskTemplate_description = Lens.lens (\CreateTaskTemplate' {description} -> description) (\s@CreateTaskTemplate' {} a -> s {description = a} :: CreateTaskTemplate)

-- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
createTaskTemplate_status :: Lens.Lens' CreateTaskTemplate (Prelude.Maybe TaskTemplateStatus)
createTaskTemplate_status = Lens.lens (\CreateTaskTemplate' {status} -> status) (\s@CreateTaskTemplate' {} a -> s {status = a} :: CreateTaskTemplate)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createTaskTemplate_instanceId :: Lens.Lens' CreateTaskTemplate Prelude.Text
createTaskTemplate_instanceId = Lens.lens (\CreateTaskTemplate' {instanceId} -> instanceId) (\s@CreateTaskTemplate' {} a -> s {instanceId = a} :: CreateTaskTemplate)

-- | The name of the task template.
createTaskTemplate_name :: Lens.Lens' CreateTaskTemplate Prelude.Text
createTaskTemplate_name = Lens.lens (\CreateTaskTemplate' {name} -> name) (\s@CreateTaskTemplate' {} a -> s {name = a} :: CreateTaskTemplate)

-- | Fields that are part of the template.
createTaskTemplate_fields :: Lens.Lens' CreateTaskTemplate [TaskTemplateField]
createTaskTemplate_fields = Lens.lens (\CreateTaskTemplate' {fields} -> fields) (\s@CreateTaskTemplate' {} a -> s {fields = a} :: CreateTaskTemplate) Prelude.. Lens.coerced

instance Core.AWSRequest CreateTaskTemplate where
  type
    AWSResponse CreateTaskTemplate =
      CreateTaskTemplateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Id")
            Prelude.<*> (x Data..:> "Arn")
      )

instance Prelude.Hashable CreateTaskTemplate where
  hashWithSalt _salt CreateTaskTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` defaults
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fields

instance Prelude.NFData CreateTaskTemplate where
  rnf CreateTaskTemplate' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf constraints `Prelude.seq`
        Prelude.rnf contactFlowId `Prelude.seq`
          Prelude.rnf defaults `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf instanceId `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf fields

instance Data.ToHeaders CreateTaskTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTaskTemplate where
  toJSON CreateTaskTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Constraints" Data..=) Prelude.<$> constraints,
            ("ContactFlowId" Data..=) Prelude.<$> contactFlowId,
            ("Defaults" Data..=) Prelude.<$> defaults,
            ("Description" Data..=) Prelude.<$> description,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Fields" Data..= fields)
          ]
      )

instance Data.ToPath CreateTaskTemplate where
  toPath CreateTaskTemplate' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/task/template"
      ]

instance Data.ToQuery CreateTaskTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTaskTemplateResponse' smart constructor.
data CreateTaskTemplateResponse = CreateTaskTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the task template resource.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the task template resource.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTaskTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTaskTemplateResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createTaskTemplateResponse_id' - The identifier of the task template resource.
--
-- 'arn', 'createTaskTemplateResponse_arn' - The Amazon Resource Name (ARN) for the task template resource.
newCreateTaskTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateTaskTemplateResponse
newCreateTaskTemplateResponse pHttpStatus_ pId_ pArn_ =
  CreateTaskTemplateResponse'
    { httpStatus =
        pHttpStatus_,
      id = pId_,
      arn = pArn_
    }

-- | The response's http status code.
createTaskTemplateResponse_httpStatus :: Lens.Lens' CreateTaskTemplateResponse Prelude.Int
createTaskTemplateResponse_httpStatus = Lens.lens (\CreateTaskTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateTaskTemplateResponse' {} a -> s {httpStatus = a} :: CreateTaskTemplateResponse)

-- | The identifier of the task template resource.
createTaskTemplateResponse_id :: Lens.Lens' CreateTaskTemplateResponse Prelude.Text
createTaskTemplateResponse_id = Lens.lens (\CreateTaskTemplateResponse' {id} -> id) (\s@CreateTaskTemplateResponse' {} a -> s {id = a} :: CreateTaskTemplateResponse)

-- | The Amazon Resource Name (ARN) for the task template resource.
createTaskTemplateResponse_arn :: Lens.Lens' CreateTaskTemplateResponse Prelude.Text
createTaskTemplateResponse_arn = Lens.lens (\CreateTaskTemplateResponse' {arn} -> arn) (\s@CreateTaskTemplateResponse' {} a -> s {arn = a} :: CreateTaskTemplateResponse)

instance Prelude.NFData CreateTaskTemplateResponse where
  rnf CreateTaskTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf arn
