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
-- Module      : Amazonka.Glue.GetTriggers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the triggers associated with a job.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetTriggers
  ( -- * Creating a Request
    GetTriggers (..),
    newGetTriggers,

    -- * Request Lenses
    getTriggers_dependentJobName,
    getTriggers_maxResults,
    getTriggers_nextToken,

    -- * Destructuring the Response
    GetTriggersResponse (..),
    newGetTriggersResponse,

    -- * Response Lenses
    getTriggersResponse_nextToken,
    getTriggersResponse_triggers,
    getTriggersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { -- | The name of the job to retrieve triggers for. The trigger that can start
    -- this job is returned, and if there is no such trigger, all triggers are
    -- returned.
    dependentJobName :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependentJobName', 'getTriggers_dependentJobName' - The name of the job to retrieve triggers for. The trigger that can start
-- this job is returned, and if there is no such trigger, all triggers are
-- returned.
--
-- 'maxResults', 'getTriggers_maxResults' - The maximum size of the response.
--
-- 'nextToken', 'getTriggers_nextToken' - A continuation token, if this is a continuation call.
newGetTriggers ::
  GetTriggers
newGetTriggers =
  GetTriggers'
    { dependentJobName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the job to retrieve triggers for. The trigger that can start
-- this job is returned, and if there is no such trigger, all triggers are
-- returned.
getTriggers_dependentJobName :: Lens.Lens' GetTriggers (Prelude.Maybe Prelude.Text)
getTriggers_dependentJobName = Lens.lens (\GetTriggers' {dependentJobName} -> dependentJobName) (\s@GetTriggers' {} a -> s {dependentJobName = a} :: GetTriggers)

-- | The maximum size of the response.
getTriggers_maxResults :: Lens.Lens' GetTriggers (Prelude.Maybe Prelude.Natural)
getTriggers_maxResults = Lens.lens (\GetTriggers' {maxResults} -> maxResults) (\s@GetTriggers' {} a -> s {maxResults = a} :: GetTriggers)

-- | A continuation token, if this is a continuation call.
getTriggers_nextToken :: Lens.Lens' GetTriggers (Prelude.Maybe Prelude.Text)
getTriggers_nextToken = Lens.lens (\GetTriggers' {nextToken} -> nextToken) (\s@GetTriggers' {} a -> s {nextToken = a} :: GetTriggers)

instance Core.AWSPager GetTriggers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTriggersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTriggersResponse_triggers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTriggers_nextToken
          Lens..~ rs
          Lens.^? getTriggersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetTriggers where
  type AWSResponse GetTriggers = GetTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTriggersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Triggers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTriggers where
  hashWithSalt _salt GetTriggers' {..} =
    _salt `Prelude.hashWithSalt` dependentJobName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetTriggers where
  rnf GetTriggers' {..} =
    Prelude.rnf dependentJobName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetTriggers" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTriggers where
  toJSON GetTriggers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DependentJobName" Data..=)
              Prelude.<$> dependentJobName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetTriggers where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { -- | A continuation token, if not all the requested triggers have yet been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of triggers for the specified job.
    triggers :: Prelude.Maybe [Trigger],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTriggersResponse
newGetTriggersResponse pHttpStatus_ =
  GetTriggersResponse'
    { nextToken = Prelude.Nothing,
      triggers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all the requested triggers have yet been
-- returned.
getTriggersResponse_nextToken :: Lens.Lens' GetTriggersResponse (Prelude.Maybe Prelude.Text)
getTriggersResponse_nextToken = Lens.lens (\GetTriggersResponse' {nextToken} -> nextToken) (\s@GetTriggersResponse' {} a -> s {nextToken = a} :: GetTriggersResponse)

-- | A list of triggers for the specified job.
getTriggersResponse_triggers :: Lens.Lens' GetTriggersResponse (Prelude.Maybe [Trigger])
getTriggersResponse_triggers = Lens.lens (\GetTriggersResponse' {triggers} -> triggers) (\s@GetTriggersResponse' {} a -> s {triggers = a} :: GetTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTriggersResponse_httpStatus :: Lens.Lens' GetTriggersResponse Prelude.Int
getTriggersResponse_httpStatus = Lens.lens (\GetTriggersResponse' {httpStatus} -> httpStatus) (\s@GetTriggersResponse' {} a -> s {httpStatus = a} :: GetTriggersResponse)

instance Prelude.NFData GetTriggersResponse where
  rnf GetTriggersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf httpStatus
