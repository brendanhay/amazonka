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
-- Module      : Amazonka.EMRContainers.ListJobTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists job templates based on a set of parameters. Job template stores
-- values of StartJobRun API request in a template and can be used to start
-- a job run. Job template allows two use cases: avoid repeating recurring
-- StartJobRun API request values, enforcing certain values in StartJobRun
-- API request.
--
-- This operation returns paginated results.
module Amazonka.EMRContainers.ListJobTemplates
  ( -- * Creating a Request
    ListJobTemplates (..),
    newListJobTemplates,

    -- * Request Lenses
    listJobTemplates_nextToken,
    listJobTemplates_createdBefore,
    listJobTemplates_maxResults,
    listJobTemplates_createdAfter,

    -- * Destructuring the Response
    ListJobTemplatesResponse (..),
    newListJobTemplatesResponse,

    -- * Response Lenses
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_templates,
    listJobTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobTemplates' smart constructor.
data ListJobTemplates = ListJobTemplates'
  { -- | The token for the next set of job templates to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time before which the job templates were created.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of job templates that can be listed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The date and time after which the job templates were created.
    createdAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobTemplates_nextToken' - The token for the next set of job templates to return.
--
-- 'createdBefore', 'listJobTemplates_createdBefore' - The date and time before which the job templates were created.
--
-- 'maxResults', 'listJobTemplates_maxResults' - The maximum number of job templates that can be listed.
--
-- 'createdAfter', 'listJobTemplates_createdAfter' - The date and time after which the job templates were created.
newListJobTemplates ::
  ListJobTemplates
newListJobTemplates =
  ListJobTemplates'
    { nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdAfter = Prelude.Nothing
    }

-- | The token for the next set of job templates to return.
listJobTemplates_nextToken :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.Text)
listJobTemplates_nextToken = Lens.lens (\ListJobTemplates' {nextToken} -> nextToken) (\s@ListJobTemplates' {} a -> s {nextToken = a} :: ListJobTemplates)

-- | The date and time before which the job templates were created.
listJobTemplates_createdBefore :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.UTCTime)
listJobTemplates_createdBefore = Lens.lens (\ListJobTemplates' {createdBefore} -> createdBefore) (\s@ListJobTemplates' {} a -> s {createdBefore = a} :: ListJobTemplates) Prelude.. Lens.mapping Core._Time

-- | The maximum number of job templates that can be listed.
listJobTemplates_maxResults :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.Int)
listJobTemplates_maxResults = Lens.lens (\ListJobTemplates' {maxResults} -> maxResults) (\s@ListJobTemplates' {} a -> s {maxResults = a} :: ListJobTemplates)

-- | The date and time after which the job templates were created.
listJobTemplates_createdAfter :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.UTCTime)
listJobTemplates_createdAfter = Lens.lens (\ListJobTemplates' {createdAfter} -> createdAfter) (\s@ListJobTemplates' {} a -> s {createdAfter = a} :: ListJobTemplates) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListJobTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobTemplatesResponse_templates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobTemplates_nextToken
          Lens..~ rs
          Lens.^? listJobTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListJobTemplates where
  type
    AWSResponse ListJobTemplates =
      ListJobTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "templates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobTemplates where
  hashWithSalt _salt ListJobTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdAfter

instance Prelude.NFData ListJobTemplates where
  rnf ListJobTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdAfter

instance Core.ToHeaders ListJobTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListJobTemplates where
  toPath = Prelude.const "/jobtemplates"

instance Core.ToQuery ListJobTemplates where
  toQuery ListJobTemplates' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "createdBefore" Core.=: createdBefore,
        "maxResults" Core.=: maxResults,
        "createdAfter" Core.=: createdAfter
      ]

-- | /See:/ 'newListJobTemplatesResponse' smart constructor.
data ListJobTemplatesResponse = ListJobTemplatesResponse'
  { -- | This output displays the token for the next set of job templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This output lists information about the specified job templates.
    templates :: Prelude.Maybe [JobTemplate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobTemplatesResponse_nextToken' - This output displays the token for the next set of job templates.
--
-- 'templates', 'listJobTemplatesResponse_templates' - This output lists information about the specified job templates.
--
-- 'httpStatus', 'listJobTemplatesResponse_httpStatus' - The response's http status code.
newListJobTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobTemplatesResponse
newListJobTemplatesResponse pHttpStatus_ =
  ListJobTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      templates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays the token for the next set of job templates.
listJobTemplatesResponse_nextToken :: Lens.Lens' ListJobTemplatesResponse (Prelude.Maybe Prelude.Text)
listJobTemplatesResponse_nextToken = Lens.lens (\ListJobTemplatesResponse' {nextToken} -> nextToken) (\s@ListJobTemplatesResponse' {} a -> s {nextToken = a} :: ListJobTemplatesResponse)

-- | This output lists information about the specified job templates.
listJobTemplatesResponse_templates :: Lens.Lens' ListJobTemplatesResponse (Prelude.Maybe [JobTemplate])
listJobTemplatesResponse_templates = Lens.lens (\ListJobTemplatesResponse' {templates} -> templates) (\s@ListJobTemplatesResponse' {} a -> s {templates = a} :: ListJobTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobTemplatesResponse_httpStatus :: Lens.Lens' ListJobTemplatesResponse Prelude.Int
listJobTemplatesResponse_httpStatus = Lens.lens (\ListJobTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListJobTemplatesResponse' {} a -> s {httpStatus = a} :: ListJobTemplatesResponse)

instance Prelude.NFData ListJobTemplatesResponse where
  rnf ListJobTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templates
      `Prelude.seq` Prelude.rnf httpStatus
