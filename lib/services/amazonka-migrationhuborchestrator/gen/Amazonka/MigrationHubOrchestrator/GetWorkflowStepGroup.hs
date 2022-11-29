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
-- Module      : Amazonka.MigrationHubOrchestrator.GetWorkflowStepGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the step group of a migration workflow.
module Amazonka.MigrationHubOrchestrator.GetWorkflowStepGroup
  ( -- * Creating a Request
    GetWorkflowStepGroup (..),
    newGetWorkflowStepGroup,

    -- * Request Lenses
    getWorkflowStepGroup_id,
    getWorkflowStepGroup_workflowId,

    -- * Destructuring the Response
    GetWorkflowStepGroupResponse (..),
    newGetWorkflowStepGroupResponse,

    -- * Response Lenses
    getWorkflowStepGroupResponse_name,
    getWorkflowStepGroupResponse_workflowId,
    getWorkflowStepGroupResponse_tools,
    getWorkflowStepGroupResponse_next,
    getWorkflowStepGroupResponse_status,
    getWorkflowStepGroupResponse_owner,
    getWorkflowStepGroupResponse_description,
    getWorkflowStepGroupResponse_endTime,
    getWorkflowStepGroupResponse_id,
    getWorkflowStepGroupResponse_lastModifiedTime,
    getWorkflowStepGroupResponse_creationTime,
    getWorkflowStepGroupResponse_previous,
    getWorkflowStepGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowStepGroup' smart constructor.
data GetWorkflowStepGroup = GetWorkflowStepGroup'
  { -- | The ID of the step group.
    id :: Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWorkflowStepGroup_id' - The ID of the step group.
--
-- 'workflowId', 'getWorkflowStepGroup_workflowId' - The ID of the migration workflow.
newGetWorkflowStepGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  GetWorkflowStepGroup
newGetWorkflowStepGroup pId_ pWorkflowId_ =
  GetWorkflowStepGroup'
    { id = pId_,
      workflowId = pWorkflowId_
    }

-- | The ID of the step group.
getWorkflowStepGroup_id :: Lens.Lens' GetWorkflowStepGroup Prelude.Text
getWorkflowStepGroup_id = Lens.lens (\GetWorkflowStepGroup' {id} -> id) (\s@GetWorkflowStepGroup' {} a -> s {id = a} :: GetWorkflowStepGroup)

-- | The ID of the migration workflow.
getWorkflowStepGroup_workflowId :: Lens.Lens' GetWorkflowStepGroup Prelude.Text
getWorkflowStepGroup_workflowId = Lens.lens (\GetWorkflowStepGroup' {workflowId} -> workflowId) (\s@GetWorkflowStepGroup' {} a -> s {workflowId = a} :: GetWorkflowStepGroup)

instance Core.AWSRequest GetWorkflowStepGroup where
  type
    AWSResponse GetWorkflowStepGroup =
      GetWorkflowStepGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowStepGroupResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "workflowId")
            Prelude.<*> (x Core..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "owner")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "endTime")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "lastModifiedTime")
            Prelude.<*> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowStepGroup where
  hashWithSalt _salt GetWorkflowStepGroup' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData GetWorkflowStepGroup where
  rnf GetWorkflowStepGroup' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf workflowId

instance Core.ToHeaders GetWorkflowStepGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWorkflowStepGroup where
  toPath GetWorkflowStepGroup' {..} =
    Prelude.mconcat
      ["/workflowstepgroup/", Core.toBS id]

instance Core.ToQuery GetWorkflowStepGroup where
  toQuery GetWorkflowStepGroup' {..} =
    Prelude.mconcat ["workflowId" Core.=: workflowId]

-- | /See:/ 'newGetWorkflowStepGroupResponse' smart constructor.
data GetWorkflowStepGroupResponse = GetWorkflowStepGroupResponse'
  { -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The status of the step group.
    status :: Prelude.Maybe StepGroupStatus,
    -- | The owner of the step group.
    owner :: Prelude.Maybe Owner,
    -- | The description of the step group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time at which the step group ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the step group was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The time at which the step group was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getWorkflowStepGroupResponse_name' - The name of the step group.
--
-- 'workflowId', 'getWorkflowStepGroupResponse_workflowId' - The ID of the migration workflow.
--
-- 'tools', 'getWorkflowStepGroupResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'next', 'getWorkflowStepGroupResponse_next' - The next step group.
--
-- 'status', 'getWorkflowStepGroupResponse_status' - The status of the step group.
--
-- 'owner', 'getWorkflowStepGroupResponse_owner' - The owner of the step group.
--
-- 'description', 'getWorkflowStepGroupResponse_description' - The description of the step group.
--
-- 'endTime', 'getWorkflowStepGroupResponse_endTime' - The time at which the step group ended.
--
-- 'id', 'getWorkflowStepGroupResponse_id' - The ID of the step group.
--
-- 'lastModifiedTime', 'getWorkflowStepGroupResponse_lastModifiedTime' - The time at which the step group was last modified.
--
-- 'creationTime', 'getWorkflowStepGroupResponse_creationTime' - The time at which the step group was created.
--
-- 'previous', 'getWorkflowStepGroupResponse_previous' - The previous step group.
--
-- 'httpStatus', 'getWorkflowStepGroupResponse_httpStatus' - The response's http status code.
newGetWorkflowStepGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowStepGroupResponse
newGetWorkflowStepGroupResponse pHttpStatus_ =
  GetWorkflowStepGroupResponse'
    { name =
        Prelude.Nothing,
      workflowId = Prelude.Nothing,
      tools = Prelude.Nothing,
      next = Prelude.Nothing,
      status = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      previous = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the step group.
getWorkflowStepGroupResponse_name :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_name = Lens.lens (\GetWorkflowStepGroupResponse' {name} -> name) (\s@GetWorkflowStepGroupResponse' {} a -> s {name = a} :: GetWorkflowStepGroupResponse)

-- | The ID of the migration workflow.
getWorkflowStepGroupResponse_workflowId :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_workflowId = Lens.lens (\GetWorkflowStepGroupResponse' {workflowId} -> workflowId) (\s@GetWorkflowStepGroupResponse' {} a -> s {workflowId = a} :: GetWorkflowStepGroupResponse)

-- | List of AWS services utilized in a migration workflow.
getWorkflowStepGroupResponse_tools :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Tool])
getWorkflowStepGroupResponse_tools = Lens.lens (\GetWorkflowStepGroupResponse' {tools} -> tools) (\s@GetWorkflowStepGroupResponse' {} a -> s {tools = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next step group.
getWorkflowStepGroupResponse_next :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepGroupResponse_next = Lens.lens (\GetWorkflowStepGroupResponse' {next} -> next) (\s@GetWorkflowStepGroupResponse' {} a -> s {next = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the step group.
getWorkflowStepGroupResponse_status :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe StepGroupStatus)
getWorkflowStepGroupResponse_status = Lens.lens (\GetWorkflowStepGroupResponse' {status} -> status) (\s@GetWorkflowStepGroupResponse' {} a -> s {status = a} :: GetWorkflowStepGroupResponse)

-- | The owner of the step group.
getWorkflowStepGroupResponse_owner :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Owner)
getWorkflowStepGroupResponse_owner = Lens.lens (\GetWorkflowStepGroupResponse' {owner} -> owner) (\s@GetWorkflowStepGroupResponse' {} a -> s {owner = a} :: GetWorkflowStepGroupResponse)

-- | The description of the step group.
getWorkflowStepGroupResponse_description :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_description = Lens.lens (\GetWorkflowStepGroupResponse' {description} -> description) (\s@GetWorkflowStepGroupResponse' {} a -> s {description = a} :: GetWorkflowStepGroupResponse)

-- | The time at which the step group ended.
getWorkflowStepGroupResponse_endTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_endTime = Lens.lens (\GetWorkflowStepGroupResponse' {endTime} -> endTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {endTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the step group.
getWorkflowStepGroupResponse_id :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_id = Lens.lens (\GetWorkflowStepGroupResponse' {id} -> id) (\s@GetWorkflowStepGroupResponse' {} a -> s {id = a} :: GetWorkflowStepGroupResponse)

-- | The time at which the step group was last modified.
getWorkflowStepGroupResponse_lastModifiedTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_lastModifiedTime = Lens.lens (\GetWorkflowStepGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {lastModifiedTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Core._Time

-- | The time at which the step group was created.
getWorkflowStepGroupResponse_creationTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_creationTime = Lens.lens (\GetWorkflowStepGroupResponse' {creationTime} -> creationTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {creationTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Core._Time

-- | The previous step group.
getWorkflowStepGroupResponse_previous :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepGroupResponse_previous = Lens.lens (\GetWorkflowStepGroupResponse' {previous} -> previous) (\s@GetWorkflowStepGroupResponse' {} a -> s {previous = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getWorkflowStepGroupResponse_httpStatus :: Lens.Lens' GetWorkflowStepGroupResponse Prelude.Int
getWorkflowStepGroupResponse_httpStatus = Lens.lens (\GetWorkflowStepGroupResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowStepGroupResponse' {} a -> s {httpStatus = a} :: GetWorkflowStepGroupResponse)

instance Prelude.NFData GetWorkflowStepGroupResponse where
  rnf GetWorkflowStepGroupResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf tools
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf httpStatus
