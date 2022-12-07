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
-- Module      : Amazonka.Connect.UpdateTaskTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates details about a specific task template in the specified Amazon
-- Connect instance. This operation does not support partial updates.
-- Instead it does a full update of template content.
module Amazonka.Connect.UpdateTaskTemplate
  ( -- * Creating a Request
    UpdateTaskTemplate (..),
    newUpdateTaskTemplate,

    -- * Request Lenses
    updateTaskTemplate_name,
    updateTaskTemplate_constraints,
    updateTaskTemplate_status,
    updateTaskTemplate_fields,
    updateTaskTemplate_description,
    updateTaskTemplate_defaults,
    updateTaskTemplate_contactFlowId,
    updateTaskTemplate_taskTemplateId,
    updateTaskTemplate_instanceId,

    -- * Destructuring the Response
    UpdateTaskTemplateResponse (..),
    newUpdateTaskTemplateResponse,

    -- * Response Lenses
    updateTaskTemplateResponse_name,
    updateTaskTemplateResponse_createdTime,
    updateTaskTemplateResponse_constraints,
    updateTaskTemplateResponse_arn,
    updateTaskTemplateResponse_status,
    updateTaskTemplateResponse_fields,
    updateTaskTemplateResponse_id,
    updateTaskTemplateResponse_description,
    updateTaskTemplateResponse_lastModifiedTime,
    updateTaskTemplateResponse_instanceId,
    updateTaskTemplateResponse_defaults,
    updateTaskTemplateResponse_contactFlowId,
    updateTaskTemplateResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTaskTemplate' smart constructor.
data UpdateTaskTemplate = UpdateTaskTemplate'
  { -- | The name of the task template.
    name :: Prelude.Maybe Prelude.Text,
    -- | Constraints that are applicable to the fields listed.
    constraints :: Prelude.Maybe TaskTemplateConstraints,
    -- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
    -- Tasks can only be created from @ACTIVE@ templates. If a template is
    -- marked as @INACTIVE@, then a task that refers to this template cannot be
    -- created.
    status :: Prelude.Maybe TaskTemplateStatus,
    -- | Fields that are part of the template.
    fields :: Prelude.Maybe [TaskTemplateField],
    -- | The description of the task template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The default values for fields when a task is created by referencing this
    -- template.
    defaults :: Prelude.Maybe TaskTemplateDefaults,
    -- | The identifier of the flow that runs by default when a task is created
    -- by referencing this template.
    contactFlowId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the task template.
    taskTemplateId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateTaskTemplate_name' - The name of the task template.
--
-- 'constraints', 'updateTaskTemplate_constraints' - Constraints that are applicable to the fields listed.
--
-- 'status', 'updateTaskTemplate_status' - Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
--
-- 'fields', 'updateTaskTemplate_fields' - Fields that are part of the template.
--
-- 'description', 'updateTaskTemplate_description' - The description of the task template.
--
-- 'defaults', 'updateTaskTemplate_defaults' - The default values for fields when a task is created by referencing this
-- template.
--
-- 'contactFlowId', 'updateTaskTemplate_contactFlowId' - The identifier of the flow that runs by default when a task is created
-- by referencing this template.
--
-- 'taskTemplateId', 'updateTaskTemplate_taskTemplateId' - A unique identifier for the task template.
--
-- 'instanceId', 'updateTaskTemplate_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newUpdateTaskTemplate ::
  -- | 'taskTemplateId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateTaskTemplate
newUpdateTaskTemplate pTaskTemplateId_ pInstanceId_ =
  UpdateTaskTemplate'
    { name = Prelude.Nothing,
      constraints = Prelude.Nothing,
      status = Prelude.Nothing,
      fields = Prelude.Nothing,
      description = Prelude.Nothing,
      defaults = Prelude.Nothing,
      contactFlowId = Prelude.Nothing,
      taskTemplateId = pTaskTemplateId_,
      instanceId = pInstanceId_
    }

-- | The name of the task template.
updateTaskTemplate_name :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe Prelude.Text)
updateTaskTemplate_name = Lens.lens (\UpdateTaskTemplate' {name} -> name) (\s@UpdateTaskTemplate' {} a -> s {name = a} :: UpdateTaskTemplate)

-- | Constraints that are applicable to the fields listed.
updateTaskTemplate_constraints :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe TaskTemplateConstraints)
updateTaskTemplate_constraints = Lens.lens (\UpdateTaskTemplate' {constraints} -> constraints) (\s@UpdateTaskTemplate' {} a -> s {constraints = a} :: UpdateTaskTemplate)

-- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
updateTaskTemplate_status :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe TaskTemplateStatus)
updateTaskTemplate_status = Lens.lens (\UpdateTaskTemplate' {status} -> status) (\s@UpdateTaskTemplate' {} a -> s {status = a} :: UpdateTaskTemplate)

-- | Fields that are part of the template.
updateTaskTemplate_fields :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe [TaskTemplateField])
updateTaskTemplate_fields = Lens.lens (\UpdateTaskTemplate' {fields} -> fields) (\s@UpdateTaskTemplate' {} a -> s {fields = a} :: UpdateTaskTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The description of the task template.
updateTaskTemplate_description :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe Prelude.Text)
updateTaskTemplate_description = Lens.lens (\UpdateTaskTemplate' {description} -> description) (\s@UpdateTaskTemplate' {} a -> s {description = a} :: UpdateTaskTemplate)

-- | The default values for fields when a task is created by referencing this
-- template.
updateTaskTemplate_defaults :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe TaskTemplateDefaults)
updateTaskTemplate_defaults = Lens.lens (\UpdateTaskTemplate' {defaults} -> defaults) (\s@UpdateTaskTemplate' {} a -> s {defaults = a} :: UpdateTaskTemplate)

-- | The identifier of the flow that runs by default when a task is created
-- by referencing this template.
updateTaskTemplate_contactFlowId :: Lens.Lens' UpdateTaskTemplate (Prelude.Maybe Prelude.Text)
updateTaskTemplate_contactFlowId = Lens.lens (\UpdateTaskTemplate' {contactFlowId} -> contactFlowId) (\s@UpdateTaskTemplate' {} a -> s {contactFlowId = a} :: UpdateTaskTemplate)

-- | A unique identifier for the task template.
updateTaskTemplate_taskTemplateId :: Lens.Lens' UpdateTaskTemplate Prelude.Text
updateTaskTemplate_taskTemplateId = Lens.lens (\UpdateTaskTemplate' {taskTemplateId} -> taskTemplateId) (\s@UpdateTaskTemplate' {} a -> s {taskTemplateId = a} :: UpdateTaskTemplate)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateTaskTemplate_instanceId :: Lens.Lens' UpdateTaskTemplate Prelude.Text
updateTaskTemplate_instanceId = Lens.lens (\UpdateTaskTemplate' {instanceId} -> instanceId) (\s@UpdateTaskTemplate' {} a -> s {instanceId = a} :: UpdateTaskTemplate)

instance Core.AWSRequest UpdateTaskTemplate where
  type
    AWSResponse UpdateTaskTemplate =
      UpdateTaskTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTaskTemplateResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "Constraints")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Fields" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "InstanceId")
            Prelude.<*> (x Data..?> "Defaults")
            Prelude.<*> (x Data..?> "ContactFlowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTaskTemplate where
  hashWithSalt _salt UpdateTaskTemplate' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` defaults
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` taskTemplateId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateTaskTemplate where
  rnf UpdateTaskTemplate' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf defaults
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf taskTemplateId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders UpdateTaskTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTaskTemplate where
  toJSON UpdateTaskTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Constraints" Data..=) Prelude.<$> constraints,
            ("Status" Data..=) Prelude.<$> status,
            ("Fields" Data..=) Prelude.<$> fields,
            ("Description" Data..=) Prelude.<$> description,
            ("Defaults" Data..=) Prelude.<$> defaults,
            ("ContactFlowId" Data..=) Prelude.<$> contactFlowId
          ]
      )

instance Data.ToPath UpdateTaskTemplate where
  toPath UpdateTaskTemplate' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/task/template/",
        Data.toBS taskTemplateId
      ]

instance Data.ToQuery UpdateTaskTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTaskTemplateResponse' smart constructor.
data UpdateTaskTemplateResponse = UpdateTaskTemplateResponse'
  { -- | The name of the task template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the task template was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Constraints that are applicable to the fields listed.
    constraints :: Prelude.Maybe TaskTemplateConstraints,
    -- | The Amazon Resource Name (ARN) for the task template resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
    -- Tasks can only be created from @ACTIVE@ templates. If a template is
    -- marked as @INACTIVE@, then a task that refers to this template cannot be
    -- created.
    status :: Prelude.Maybe TaskTemplateStatus,
    -- | Fields that are part of the template.
    fields :: Prelude.Maybe [TaskTemplateField],
    -- | The identifier of the task template resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the task template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the task template was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The default values for fields when a task is created by referencing this
    -- template.
    defaults :: Prelude.Maybe TaskTemplateDefaults,
    -- | The identifier of the flow that runs by default when a task is created
    -- by referencing this template.
    contactFlowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateTaskTemplateResponse_name' - The name of the task template.
--
-- 'createdTime', 'updateTaskTemplateResponse_createdTime' - The timestamp when the task template was created.
--
-- 'constraints', 'updateTaskTemplateResponse_constraints' - Constraints that are applicable to the fields listed.
--
-- 'arn', 'updateTaskTemplateResponse_arn' - The Amazon Resource Name (ARN) for the task template resource.
--
-- 'status', 'updateTaskTemplateResponse_status' - Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
--
-- 'fields', 'updateTaskTemplateResponse_fields' - Fields that are part of the template.
--
-- 'id', 'updateTaskTemplateResponse_id' - The identifier of the task template resource.
--
-- 'description', 'updateTaskTemplateResponse_description' - The description of the task template.
--
-- 'lastModifiedTime', 'updateTaskTemplateResponse_lastModifiedTime' - The timestamp when the task template was last modified.
--
-- 'instanceId', 'updateTaskTemplateResponse_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'defaults', 'updateTaskTemplateResponse_defaults' - The default values for fields when a task is created by referencing this
-- template.
--
-- 'contactFlowId', 'updateTaskTemplateResponse_contactFlowId' - The identifier of the flow that runs by default when a task is created
-- by referencing this template.
--
-- 'httpStatus', 'updateTaskTemplateResponse_httpStatus' - The response's http status code.
newUpdateTaskTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTaskTemplateResponse
newUpdateTaskTemplateResponse pHttpStatus_ =
  UpdateTaskTemplateResponse'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      constraints = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      fields = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      defaults = Prelude.Nothing,
      contactFlowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the task template.
updateTaskTemplateResponse_name :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_name = Lens.lens (\UpdateTaskTemplateResponse' {name} -> name) (\s@UpdateTaskTemplateResponse' {} a -> s {name = a} :: UpdateTaskTemplateResponse)

-- | The timestamp when the task template was created.
updateTaskTemplateResponse_createdTime :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.UTCTime)
updateTaskTemplateResponse_createdTime = Lens.lens (\UpdateTaskTemplateResponse' {createdTime} -> createdTime) (\s@UpdateTaskTemplateResponse' {} a -> s {createdTime = a} :: UpdateTaskTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | Constraints that are applicable to the fields listed.
updateTaskTemplateResponse_constraints :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe TaskTemplateConstraints)
updateTaskTemplateResponse_constraints = Lens.lens (\UpdateTaskTemplateResponse' {constraints} -> constraints) (\s@UpdateTaskTemplateResponse' {} a -> s {constraints = a} :: UpdateTaskTemplateResponse)

-- | The Amazon Resource Name (ARN) for the task template resource.
updateTaskTemplateResponse_arn :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_arn = Lens.lens (\UpdateTaskTemplateResponse' {arn} -> arn) (\s@UpdateTaskTemplateResponse' {} a -> s {arn = a} :: UpdateTaskTemplateResponse)

-- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
updateTaskTemplateResponse_status :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe TaskTemplateStatus)
updateTaskTemplateResponse_status = Lens.lens (\UpdateTaskTemplateResponse' {status} -> status) (\s@UpdateTaskTemplateResponse' {} a -> s {status = a} :: UpdateTaskTemplateResponse)

-- | Fields that are part of the template.
updateTaskTemplateResponse_fields :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe [TaskTemplateField])
updateTaskTemplateResponse_fields = Lens.lens (\UpdateTaskTemplateResponse' {fields} -> fields) (\s@UpdateTaskTemplateResponse' {} a -> s {fields = a} :: UpdateTaskTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the task template resource.
updateTaskTemplateResponse_id :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_id = Lens.lens (\UpdateTaskTemplateResponse' {id} -> id) (\s@UpdateTaskTemplateResponse' {} a -> s {id = a} :: UpdateTaskTemplateResponse)

-- | The description of the task template.
updateTaskTemplateResponse_description :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_description = Lens.lens (\UpdateTaskTemplateResponse' {description} -> description) (\s@UpdateTaskTemplateResponse' {} a -> s {description = a} :: UpdateTaskTemplateResponse)

-- | The timestamp when the task template was last modified.
updateTaskTemplateResponse_lastModifiedTime :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.UTCTime)
updateTaskTemplateResponse_lastModifiedTime = Lens.lens (\UpdateTaskTemplateResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateTaskTemplateResponse' {} a -> s {lastModifiedTime = a} :: UpdateTaskTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateTaskTemplateResponse_instanceId :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_instanceId = Lens.lens (\UpdateTaskTemplateResponse' {instanceId} -> instanceId) (\s@UpdateTaskTemplateResponse' {} a -> s {instanceId = a} :: UpdateTaskTemplateResponse)

-- | The default values for fields when a task is created by referencing this
-- template.
updateTaskTemplateResponse_defaults :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe TaskTemplateDefaults)
updateTaskTemplateResponse_defaults = Lens.lens (\UpdateTaskTemplateResponse' {defaults} -> defaults) (\s@UpdateTaskTemplateResponse' {} a -> s {defaults = a} :: UpdateTaskTemplateResponse)

-- | The identifier of the flow that runs by default when a task is created
-- by referencing this template.
updateTaskTemplateResponse_contactFlowId :: Lens.Lens' UpdateTaskTemplateResponse (Prelude.Maybe Prelude.Text)
updateTaskTemplateResponse_contactFlowId = Lens.lens (\UpdateTaskTemplateResponse' {contactFlowId} -> contactFlowId) (\s@UpdateTaskTemplateResponse' {} a -> s {contactFlowId = a} :: UpdateTaskTemplateResponse)

-- | The response's http status code.
updateTaskTemplateResponse_httpStatus :: Lens.Lens' UpdateTaskTemplateResponse Prelude.Int
updateTaskTemplateResponse_httpStatus = Lens.lens (\UpdateTaskTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateTaskTemplateResponse' {} a -> s {httpStatus = a} :: UpdateTaskTemplateResponse)

instance Prelude.NFData UpdateTaskTemplateResponse where
  rnf UpdateTaskTemplateResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf defaults
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf httpStatus
