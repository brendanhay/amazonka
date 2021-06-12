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
-- Module      : Network.AWS.Glue.GetTriggers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the triggers associated with a job.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTriggers
  ( -- * Creating a Request
    GetTriggers (..),
    newGetTriggers,

    -- * Request Lenses
    getTriggers_nextToken,
    getTriggers_maxResults,
    getTriggers_dependentJobName,

    -- * Destructuring the Response
    GetTriggersResponse (..),
    newGetTriggersResponse,

    -- * Response Lenses
    getTriggersResponse_nextToken,
    getTriggersResponse_triggers,
    getTriggersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the job to retrieve triggers for. The trigger that can start
    -- this job is returned, and if there is no such trigger, all triggers are
    -- returned.
    dependentJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTriggers_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getTriggers_maxResults' - The maximum size of the response.
--
-- 'dependentJobName', 'getTriggers_dependentJobName' - The name of the job to retrieve triggers for. The trigger that can start
-- this job is returned, and if there is no such trigger, all triggers are
-- returned.
newGetTriggers ::
  GetTriggers
newGetTriggers =
  GetTriggers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      dependentJobName = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getTriggers_nextToken :: Lens.Lens' GetTriggers (Core.Maybe Core.Text)
getTriggers_nextToken = Lens.lens (\GetTriggers' {nextToken} -> nextToken) (\s@GetTriggers' {} a -> s {nextToken = a} :: GetTriggers)

-- | The maximum size of the response.
getTriggers_maxResults :: Lens.Lens' GetTriggers (Core.Maybe Core.Natural)
getTriggers_maxResults = Lens.lens (\GetTriggers' {maxResults} -> maxResults) (\s@GetTriggers' {} a -> s {maxResults = a} :: GetTriggers)

-- | The name of the job to retrieve triggers for. The trigger that can start
-- this job is returned, and if there is no such trigger, all triggers are
-- returned.
getTriggers_dependentJobName :: Lens.Lens' GetTriggers (Core.Maybe Core.Text)
getTriggers_dependentJobName = Lens.lens (\GetTriggers' {dependentJobName} -> dependentJobName) (\s@GetTriggers' {} a -> s {dependentJobName = a} :: GetTriggers)

instance Core.AWSPager GetTriggers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTriggersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTriggersResponse_triggers Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTriggers_nextToken
          Lens..~ rs
          Lens.^? getTriggersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetTriggers where
  type AWSResponse GetTriggers = GetTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTriggersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Triggers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTriggers

instance Core.NFData GetTriggers

instance Core.ToHeaders GetTriggers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTriggers" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTriggers where
  toJSON GetTriggers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DependentJobName" Core..=)
              Core.<$> dependentJobName
          ]
      )

instance Core.ToPath GetTriggers where
  toPath = Core.const "/"

instance Core.ToQuery GetTriggers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { -- | A continuation token, if not all the requested triggers have yet been
    -- returned.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of triggers for the specified job.
    triggers :: Core.Maybe [Trigger],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTriggersResponse_nextToken' - A continuation token, if not all the requested triggers have yet been
-- returned.
--
-- 'triggers', 'getTriggersResponse_triggers' - A list of triggers for the specified job.
--
-- 'httpStatus', 'getTriggersResponse_httpStatus' - The response's http status code.
newGetTriggersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTriggersResponse
newGetTriggersResponse pHttpStatus_ =
  GetTriggersResponse'
    { nextToken = Core.Nothing,
      triggers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all the requested triggers have yet been
-- returned.
getTriggersResponse_nextToken :: Lens.Lens' GetTriggersResponse (Core.Maybe Core.Text)
getTriggersResponse_nextToken = Lens.lens (\GetTriggersResponse' {nextToken} -> nextToken) (\s@GetTriggersResponse' {} a -> s {nextToken = a} :: GetTriggersResponse)

-- | A list of triggers for the specified job.
getTriggersResponse_triggers :: Lens.Lens' GetTriggersResponse (Core.Maybe [Trigger])
getTriggersResponse_triggers = Lens.lens (\GetTriggersResponse' {triggers} -> triggers) (\s@GetTriggersResponse' {} a -> s {triggers = a} :: GetTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTriggersResponse_httpStatus :: Lens.Lens' GetTriggersResponse Core.Int
getTriggersResponse_httpStatus = Lens.lens (\GetTriggersResponse' {httpStatus} -> httpStatus) (\s@GetTriggersResponse' {} a -> s {httpStatus = a} :: GetTriggersResponse)

instance Core.NFData GetTriggersResponse
