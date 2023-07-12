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
-- Module      : Amazonka.MigrationHubOrchestrator.CreateWorkflowStepGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a step group in a migration workflow.
module Amazonka.MigrationHubOrchestrator.CreateWorkflowStepGroup
  ( -- * Creating a Request
    CreateWorkflowStepGroup (..),
    newCreateWorkflowStepGroup,

    -- * Request Lenses
    createWorkflowStepGroup_description,
    createWorkflowStepGroup_next,
    createWorkflowStepGroup_previous,
    createWorkflowStepGroup_workflowId,
    createWorkflowStepGroup_name,

    -- * Destructuring the Response
    CreateWorkflowStepGroupResponse (..),
    newCreateWorkflowStepGroupResponse,

    -- * Response Lenses
    createWorkflowStepGroupResponse_creationTime,
    createWorkflowStepGroupResponse_description,
    createWorkflowStepGroupResponse_id,
    createWorkflowStepGroupResponse_name,
    createWorkflowStepGroupResponse_next,
    createWorkflowStepGroupResponse_previous,
    createWorkflowStepGroupResponse_tools,
    createWorkflowStepGroupResponse_workflowId,
    createWorkflowStepGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkflowStepGroup' smart constructor.
data CreateWorkflowStepGroup = CreateWorkflowStepGroup'
  { -- | The description of the step group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the migration workflow that will contain the step group.
    workflowId :: Prelude.Text,
    -- | The name of the step group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowStepGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createWorkflowStepGroup_description' - The description of the step group.
--
-- 'next', 'createWorkflowStepGroup_next' - The next step group.
--
-- 'previous', 'createWorkflowStepGroup_previous' - The previous step group.
--
-- 'workflowId', 'createWorkflowStepGroup_workflowId' - The ID of the migration workflow that will contain the step group.
--
-- 'name', 'createWorkflowStepGroup_name' - The name of the step group.
newCreateWorkflowStepGroup ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateWorkflowStepGroup
newCreateWorkflowStepGroup pWorkflowId_ pName_ =
  CreateWorkflowStepGroup'
    { description =
        Prelude.Nothing,
      next = Prelude.Nothing,
      previous = Prelude.Nothing,
      workflowId = pWorkflowId_,
      name = pName_
    }

-- | The description of the step group.
createWorkflowStepGroup_description :: Lens.Lens' CreateWorkflowStepGroup (Prelude.Maybe Prelude.Text)
createWorkflowStepGroup_description = Lens.lens (\CreateWorkflowStepGroup' {description} -> description) (\s@CreateWorkflowStepGroup' {} a -> s {description = a} :: CreateWorkflowStepGroup)

-- | The next step group.
createWorkflowStepGroup_next :: Lens.Lens' CreateWorkflowStepGroup (Prelude.Maybe [Prelude.Text])
createWorkflowStepGroup_next = Lens.lens (\CreateWorkflowStepGroup' {next} -> next) (\s@CreateWorkflowStepGroup' {} a -> s {next = a} :: CreateWorkflowStepGroup) Prelude.. Lens.mapping Lens.coerced

-- | The previous step group.
createWorkflowStepGroup_previous :: Lens.Lens' CreateWorkflowStepGroup (Prelude.Maybe [Prelude.Text])
createWorkflowStepGroup_previous = Lens.lens (\CreateWorkflowStepGroup' {previous} -> previous) (\s@CreateWorkflowStepGroup' {} a -> s {previous = a} :: CreateWorkflowStepGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the migration workflow that will contain the step group.
createWorkflowStepGroup_workflowId :: Lens.Lens' CreateWorkflowStepGroup Prelude.Text
createWorkflowStepGroup_workflowId = Lens.lens (\CreateWorkflowStepGroup' {workflowId} -> workflowId) (\s@CreateWorkflowStepGroup' {} a -> s {workflowId = a} :: CreateWorkflowStepGroup)

-- | The name of the step group.
createWorkflowStepGroup_name :: Lens.Lens' CreateWorkflowStepGroup Prelude.Text
createWorkflowStepGroup_name = Lens.lens (\CreateWorkflowStepGroup' {name} -> name) (\s@CreateWorkflowStepGroup' {} a -> s {name = a} :: CreateWorkflowStepGroup)

instance Core.AWSRequest CreateWorkflowStepGroup where
  type
    AWSResponse CreateWorkflowStepGroup =
      CreateWorkflowStepGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowStepGroupResponse'
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkflowStepGroup where
  hashWithSalt _salt CreateWorkflowStepGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWorkflowStepGroup where
  rnf CreateWorkflowStepGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateWorkflowStepGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkflowStepGroup where
  toJSON CreateWorkflowStepGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("next" Data..=) Prelude.<$> next,
            ("previous" Data..=) Prelude.<$> previous,
            Prelude.Just ("workflowId" Data..= workflowId),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateWorkflowStepGroup where
  toPath = Prelude.const "/workflowstepgroups"

instance Data.ToQuery CreateWorkflowStepGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowStepGroupResponse' smart constructor.
data CreateWorkflowStepGroupResponse = CreateWorkflowStepGroupResponse'
  { -- | The time at which the step group is created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the step group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The ID of the migration workflow that contains the step group.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowStepGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createWorkflowStepGroupResponse_creationTime' - The time at which the step group is created.
--
-- 'description', 'createWorkflowStepGroupResponse_description' - The description of the step group.
--
-- 'id', 'createWorkflowStepGroupResponse_id' - The ID of the step group.
--
-- 'name', 'createWorkflowStepGroupResponse_name' - The name of the step group.
--
-- 'next', 'createWorkflowStepGroupResponse_next' - The next step group.
--
-- 'previous', 'createWorkflowStepGroupResponse_previous' - The previous step group.
--
-- 'tools', 'createWorkflowStepGroupResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'workflowId', 'createWorkflowStepGroupResponse_workflowId' - The ID of the migration workflow that contains the step group.
--
-- 'httpStatus', 'createWorkflowStepGroupResponse_httpStatus' - The response's http status code.
newCreateWorkflowStepGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkflowStepGroupResponse
newCreateWorkflowStepGroupResponse pHttpStatus_ =
  CreateWorkflowStepGroupResponse'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      previous = Prelude.Nothing,
      tools = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the step group is created.
createWorkflowStepGroupResponse_creationTime :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
createWorkflowStepGroupResponse_creationTime = Lens.lens (\CreateWorkflowStepGroupResponse' {creationTime} -> creationTime) (\s@CreateWorkflowStepGroupResponse' {} a -> s {creationTime = a} :: CreateWorkflowStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the step group.
createWorkflowStepGroupResponse_description :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepGroupResponse_description = Lens.lens (\CreateWorkflowStepGroupResponse' {description} -> description) (\s@CreateWorkflowStepGroupResponse' {} a -> s {description = a} :: CreateWorkflowStepGroupResponse)

-- | The ID of the step group.
createWorkflowStepGroupResponse_id :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepGroupResponse_id = Lens.lens (\CreateWorkflowStepGroupResponse' {id} -> id) (\s@CreateWorkflowStepGroupResponse' {} a -> s {id = a} :: CreateWorkflowStepGroupResponse)

-- | The name of the step group.
createWorkflowStepGroupResponse_name :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepGroupResponse_name = Lens.lens (\CreateWorkflowStepGroupResponse' {name} -> name) (\s@CreateWorkflowStepGroupResponse' {} a -> s {name = a} :: CreateWorkflowStepGroupResponse)

-- | The next step group.
createWorkflowStepGroupResponse_next :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
createWorkflowStepGroupResponse_next = Lens.lens (\CreateWorkflowStepGroupResponse' {next} -> next) (\s@CreateWorkflowStepGroupResponse' {} a -> s {next = a} :: CreateWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The previous step group.
createWorkflowStepGroupResponse_previous :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
createWorkflowStepGroupResponse_previous = Lens.lens (\CreateWorkflowStepGroupResponse' {previous} -> previous) (\s@CreateWorkflowStepGroupResponse' {} a -> s {previous = a} :: CreateWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | List of AWS services utilized in a migration workflow.
createWorkflowStepGroupResponse_tools :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe [Tool])
createWorkflowStepGroupResponse_tools = Lens.lens (\CreateWorkflowStepGroupResponse' {tools} -> tools) (\s@CreateWorkflowStepGroupResponse' {} a -> s {tools = a} :: CreateWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the migration workflow that contains the step group.
createWorkflowStepGroupResponse_workflowId :: Lens.Lens' CreateWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepGroupResponse_workflowId = Lens.lens (\CreateWorkflowStepGroupResponse' {workflowId} -> workflowId) (\s@CreateWorkflowStepGroupResponse' {} a -> s {workflowId = a} :: CreateWorkflowStepGroupResponse)

-- | The response's http status code.
createWorkflowStepGroupResponse_httpStatus :: Lens.Lens' CreateWorkflowStepGroupResponse Prelude.Int
createWorkflowStepGroupResponse_httpStatus = Lens.lens (\CreateWorkflowStepGroupResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowStepGroupResponse' {} a -> s {httpStatus = a} :: CreateWorkflowStepGroupResponse)

instance
  Prelude.NFData
    CreateWorkflowStepGroupResponse
  where
  rnf CreateWorkflowStepGroupResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf tools
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf httpStatus
