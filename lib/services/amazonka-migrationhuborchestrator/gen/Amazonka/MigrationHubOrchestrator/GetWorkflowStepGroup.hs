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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    getWorkflowStepGroupResponse_creationTime,
    getWorkflowStepGroupResponse_description,
    getWorkflowStepGroupResponse_endTime,
    getWorkflowStepGroupResponse_id,
    getWorkflowStepGroupResponse_lastModifiedTime,
    getWorkflowStepGroupResponse_name,
    getWorkflowStepGroupResponse_next,
    getWorkflowStepGroupResponse_owner,
    getWorkflowStepGroupResponse_previous,
    getWorkflowStepGroupResponse_status,
    getWorkflowStepGroupResponse_tools,
    getWorkflowStepGroupResponse_workflowId,
    getWorkflowStepGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "owner")
            Prelude.<*> (x Data..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowStepGroup where
  hashWithSalt _salt GetWorkflowStepGroup' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData GetWorkflowStepGroup where
  rnf GetWorkflowStepGroup' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders GetWorkflowStepGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflowStepGroup where
  toPath GetWorkflowStepGroup' {..} =
    Prelude.mconcat
      ["/workflowstepgroup/", Data.toBS id]

instance Data.ToQuery GetWorkflowStepGroup where
  toQuery GetWorkflowStepGroup' {..} =
    Prelude.mconcat ["workflowId" Data.=: workflowId]

-- | /See:/ 'newGetWorkflowStepGroupResponse' smart constructor.
data GetWorkflowStepGroupResponse = GetWorkflowStepGroupResponse'
  { -- | The time at which the step group was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the step group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time at which the step group ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the step group was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The owner of the step group.
    owner :: Prelude.Maybe Owner,
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The status of the step group.
    status :: Prelude.Maybe StepGroupStatus,
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'getWorkflowStepGroupResponse_creationTime' - The time at which the step group was created.
--
-- 'description', 'getWorkflowStepGroupResponse_description' - The description of the step group.
--
-- 'endTime', 'getWorkflowStepGroupResponse_endTime' - The time at which the step group ended.
--
-- 'id', 'getWorkflowStepGroupResponse_id' - The ID of the step group.
--
-- 'lastModifiedTime', 'getWorkflowStepGroupResponse_lastModifiedTime' - The time at which the step group was last modified.
--
-- 'name', 'getWorkflowStepGroupResponse_name' - The name of the step group.
--
-- 'next', 'getWorkflowStepGroupResponse_next' - The next step group.
--
-- 'owner', 'getWorkflowStepGroupResponse_owner' - The owner of the step group.
--
-- 'previous', 'getWorkflowStepGroupResponse_previous' - The previous step group.
--
-- 'status', 'getWorkflowStepGroupResponse_status' - The status of the step group.
--
-- 'tools', 'getWorkflowStepGroupResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'workflowId', 'getWorkflowStepGroupResponse_workflowId' - The ID of the migration workflow.
--
-- 'httpStatus', 'getWorkflowStepGroupResponse_httpStatus' - The response's http status code.
newGetWorkflowStepGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowStepGroupResponse
newGetWorkflowStepGroupResponse pHttpStatus_ =
  GetWorkflowStepGroupResponse'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      owner = Prelude.Nothing,
      previous = Prelude.Nothing,
      status = Prelude.Nothing,
      tools = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the step group was created.
getWorkflowStepGroupResponse_creationTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_creationTime = Lens.lens (\GetWorkflowStepGroupResponse' {creationTime} -> creationTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {creationTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the step group.
getWorkflowStepGroupResponse_description :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_description = Lens.lens (\GetWorkflowStepGroupResponse' {description} -> description) (\s@GetWorkflowStepGroupResponse' {} a -> s {description = a} :: GetWorkflowStepGroupResponse)

-- | The time at which the step group ended.
getWorkflowStepGroupResponse_endTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_endTime = Lens.lens (\GetWorkflowStepGroupResponse' {endTime} -> endTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {endTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the step group.
getWorkflowStepGroupResponse_id :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_id = Lens.lens (\GetWorkflowStepGroupResponse' {id} -> id) (\s@GetWorkflowStepGroupResponse' {} a -> s {id = a} :: GetWorkflowStepGroupResponse)

-- | The time at which the step group was last modified.
getWorkflowStepGroupResponse_lastModifiedTime :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepGroupResponse_lastModifiedTime = Lens.lens (\GetWorkflowStepGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetWorkflowStepGroupResponse' {} a -> s {lastModifiedTime = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the step group.
getWorkflowStepGroupResponse_name :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_name = Lens.lens (\GetWorkflowStepGroupResponse' {name} -> name) (\s@GetWorkflowStepGroupResponse' {} a -> s {name = a} :: GetWorkflowStepGroupResponse)

-- | The next step group.
getWorkflowStepGroupResponse_next :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepGroupResponse_next = Lens.lens (\GetWorkflowStepGroupResponse' {next} -> next) (\s@GetWorkflowStepGroupResponse' {} a -> s {next = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The owner of the step group.
getWorkflowStepGroupResponse_owner :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Owner)
getWorkflowStepGroupResponse_owner = Lens.lens (\GetWorkflowStepGroupResponse' {owner} -> owner) (\s@GetWorkflowStepGroupResponse' {} a -> s {owner = a} :: GetWorkflowStepGroupResponse)

-- | The previous step group.
getWorkflowStepGroupResponse_previous :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepGroupResponse_previous = Lens.lens (\GetWorkflowStepGroupResponse' {previous} -> previous) (\s@GetWorkflowStepGroupResponse' {} a -> s {previous = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the step group.
getWorkflowStepGroupResponse_status :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe StepGroupStatus)
getWorkflowStepGroupResponse_status = Lens.lens (\GetWorkflowStepGroupResponse' {status} -> status) (\s@GetWorkflowStepGroupResponse' {} a -> s {status = a} :: GetWorkflowStepGroupResponse)

-- | List of AWS services utilized in a migration workflow.
getWorkflowStepGroupResponse_tools :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe [Tool])
getWorkflowStepGroupResponse_tools = Lens.lens (\GetWorkflowStepGroupResponse' {tools} -> tools) (\s@GetWorkflowStepGroupResponse' {} a -> s {tools = a} :: GetWorkflowStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the migration workflow.
getWorkflowStepGroupResponse_workflowId :: Lens.Lens' GetWorkflowStepGroupResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepGroupResponse_workflowId = Lens.lens (\GetWorkflowStepGroupResponse' {workflowId} -> workflowId) (\s@GetWorkflowStepGroupResponse' {} a -> s {workflowId = a} :: GetWorkflowStepGroupResponse)

-- | The response's http status code.
getWorkflowStepGroupResponse_httpStatus :: Lens.Lens' GetWorkflowStepGroupResponse Prelude.Int
getWorkflowStepGroupResponse_httpStatus = Lens.lens (\GetWorkflowStepGroupResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowStepGroupResponse' {} a -> s {httpStatus = a} :: GetWorkflowStepGroupResponse)

instance Prelude.NFData GetWorkflowStepGroupResponse where
  rnf GetWorkflowStepGroupResponse' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf next `Prelude.seq`
                  Prelude.rnf owner `Prelude.seq`
                    Prelude.rnf previous `Prelude.seq`
                      Prelude.rnf status `Prelude.seq`
                        Prelude.rnf tools `Prelude.seq`
                          Prelude.rnf workflowId `Prelude.seq`
                            Prelude.rnf httpStatus
