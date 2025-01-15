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
-- Module      : Amazonka.MigrationHubOrchestrator.GetTemplateStepGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a step group in a template.
module Amazonka.MigrationHubOrchestrator.GetTemplateStepGroup
  ( -- * Creating a Request
    GetTemplateStepGroup (..),
    newGetTemplateStepGroup,

    -- * Request Lenses
    getTemplateStepGroup_templateId,
    getTemplateStepGroup_id,

    -- * Destructuring the Response
    GetTemplateStepGroupResponse (..),
    newGetTemplateStepGroupResponse,

    -- * Response Lenses
    getTemplateStepGroupResponse_creationTime,
    getTemplateStepGroupResponse_description,
    getTemplateStepGroupResponse_id,
    getTemplateStepGroupResponse_lastModifiedTime,
    getTemplateStepGroupResponse_name,
    getTemplateStepGroupResponse_next,
    getTemplateStepGroupResponse_previous,
    getTemplateStepGroupResponse_status,
    getTemplateStepGroupResponse_templateId,
    getTemplateStepGroupResponse_tools,
    getTemplateStepGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemplateStepGroup' smart constructor.
data GetTemplateStepGroup = GetTemplateStepGroup'
  { -- | The ID of the template.
    templateId :: Prelude.Text,
    -- | The ID of the step group.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateStepGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateId', 'getTemplateStepGroup_templateId' - The ID of the template.
--
-- 'id', 'getTemplateStepGroup_id' - The ID of the step group.
newGetTemplateStepGroup ::
  -- | 'templateId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetTemplateStepGroup
newGetTemplateStepGroup pTemplateId_ pId_ =
  GetTemplateStepGroup'
    { templateId = pTemplateId_,
      id = pId_
    }

-- | The ID of the template.
getTemplateStepGroup_templateId :: Lens.Lens' GetTemplateStepGroup Prelude.Text
getTemplateStepGroup_templateId = Lens.lens (\GetTemplateStepGroup' {templateId} -> templateId) (\s@GetTemplateStepGroup' {} a -> s {templateId = a} :: GetTemplateStepGroup)

-- | The ID of the step group.
getTemplateStepGroup_id :: Lens.Lens' GetTemplateStepGroup Prelude.Text
getTemplateStepGroup_id = Lens.lens (\GetTemplateStepGroup' {id} -> id) (\s@GetTemplateStepGroup' {} a -> s {id = a} :: GetTemplateStepGroup)

instance Core.AWSRequest GetTemplateStepGroup where
  type
    AWSResponse GetTemplateStepGroup =
      GetTemplateStepGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemplateStepGroupResponse'
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "templateId")
            Prelude.<*> (x Data..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateStepGroup where
  hashWithSalt _salt GetTemplateStepGroup' {..} =
    _salt
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetTemplateStepGroup where
  rnf GetTemplateStepGroup' {..} =
    Prelude.rnf templateId `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetTemplateStepGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTemplateStepGroup where
  toPath GetTemplateStepGroup' {..} =
    Prelude.mconcat
      [ "/templates/",
        Data.toBS templateId,
        "/stepgroups/",
        Data.toBS id
      ]

instance Data.ToQuery GetTemplateStepGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemplateStepGroupResponse' smart constructor.
data GetTemplateStepGroupResponse = GetTemplateStepGroupResponse'
  { -- | The time at which the step group was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the step group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the step group was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The status of the step group.
    status :: Prelude.Maybe StepGroupStatus,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateStepGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'getTemplateStepGroupResponse_creationTime' - The time at which the step group was created.
--
-- 'description', 'getTemplateStepGroupResponse_description' - The description of the step group.
--
-- 'id', 'getTemplateStepGroupResponse_id' - The ID of the step group.
--
-- 'lastModifiedTime', 'getTemplateStepGroupResponse_lastModifiedTime' - The time at which the step group was last modified.
--
-- 'name', 'getTemplateStepGroupResponse_name' - The name of the step group.
--
-- 'next', 'getTemplateStepGroupResponse_next' - The next step group.
--
-- 'previous', 'getTemplateStepGroupResponse_previous' - The previous step group.
--
-- 'status', 'getTemplateStepGroupResponse_status' - The status of the step group.
--
-- 'templateId', 'getTemplateStepGroupResponse_templateId' - The ID of the template.
--
-- 'tools', 'getTemplateStepGroupResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'httpStatus', 'getTemplateStepGroupResponse_httpStatus' - The response's http status code.
newGetTemplateStepGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateStepGroupResponse
newGetTemplateStepGroupResponse pHttpStatus_ =
  GetTemplateStepGroupResponse'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      previous = Prelude.Nothing,
      status = Prelude.Nothing,
      templateId = Prelude.Nothing,
      tools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the step group was created.
getTemplateStepGroupResponse_creationTime :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getTemplateStepGroupResponse_creationTime = Lens.lens (\GetTemplateStepGroupResponse' {creationTime} -> creationTime) (\s@GetTemplateStepGroupResponse' {} a -> s {creationTime = a} :: GetTemplateStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the step group.
getTemplateStepGroupResponse_description :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.Text)
getTemplateStepGroupResponse_description = Lens.lens (\GetTemplateStepGroupResponse' {description} -> description) (\s@GetTemplateStepGroupResponse' {} a -> s {description = a} :: GetTemplateStepGroupResponse)

-- | The ID of the step group.
getTemplateStepGroupResponse_id :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.Text)
getTemplateStepGroupResponse_id = Lens.lens (\GetTemplateStepGroupResponse' {id} -> id) (\s@GetTemplateStepGroupResponse' {} a -> s {id = a} :: GetTemplateStepGroupResponse)

-- | The time at which the step group was last modified.
getTemplateStepGroupResponse_lastModifiedTime :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.UTCTime)
getTemplateStepGroupResponse_lastModifiedTime = Lens.lens (\GetTemplateStepGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetTemplateStepGroupResponse' {} a -> s {lastModifiedTime = a} :: GetTemplateStepGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the step group.
getTemplateStepGroupResponse_name :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.Text)
getTemplateStepGroupResponse_name = Lens.lens (\GetTemplateStepGroupResponse' {name} -> name) (\s@GetTemplateStepGroupResponse' {} a -> s {name = a} :: GetTemplateStepGroupResponse)

-- | The next step group.
getTemplateStepGroupResponse_next :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe [Prelude.Text])
getTemplateStepGroupResponse_next = Lens.lens (\GetTemplateStepGroupResponse' {next} -> next) (\s@GetTemplateStepGroupResponse' {} a -> s {next = a} :: GetTemplateStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The previous step group.
getTemplateStepGroupResponse_previous :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe [Prelude.Text])
getTemplateStepGroupResponse_previous = Lens.lens (\GetTemplateStepGroupResponse' {previous} -> previous) (\s@GetTemplateStepGroupResponse' {} a -> s {previous = a} :: GetTemplateStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the step group.
getTemplateStepGroupResponse_status :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe StepGroupStatus)
getTemplateStepGroupResponse_status = Lens.lens (\GetTemplateStepGroupResponse' {status} -> status) (\s@GetTemplateStepGroupResponse' {} a -> s {status = a} :: GetTemplateStepGroupResponse)

-- | The ID of the template.
getTemplateStepGroupResponse_templateId :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe Prelude.Text)
getTemplateStepGroupResponse_templateId = Lens.lens (\GetTemplateStepGroupResponse' {templateId} -> templateId) (\s@GetTemplateStepGroupResponse' {} a -> s {templateId = a} :: GetTemplateStepGroupResponse)

-- | List of AWS services utilized in a migration workflow.
getTemplateStepGroupResponse_tools :: Lens.Lens' GetTemplateStepGroupResponse (Prelude.Maybe [Tool])
getTemplateStepGroupResponse_tools = Lens.lens (\GetTemplateStepGroupResponse' {tools} -> tools) (\s@GetTemplateStepGroupResponse' {} a -> s {tools = a} :: GetTemplateStepGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTemplateStepGroupResponse_httpStatus :: Lens.Lens' GetTemplateStepGroupResponse Prelude.Int
getTemplateStepGroupResponse_httpStatus = Lens.lens (\GetTemplateStepGroupResponse' {httpStatus} -> httpStatus) (\s@GetTemplateStepGroupResponse' {} a -> s {httpStatus = a} :: GetTemplateStepGroupResponse)

instance Prelude.NFData GetTemplateStepGroupResponse where
  rnf GetTemplateStepGroupResponse' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastModifiedTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf next `Prelude.seq`
                Prelude.rnf previous `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf templateId `Prelude.seq`
                      Prelude.rnf tools `Prelude.seq`
                        Prelude.rnf httpStatus
