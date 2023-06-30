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
-- Module      : Amazonka.Proton.ListEnvironmentTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List environment templates.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironmentTemplates
  ( -- * Creating a Request
    ListEnvironmentTemplates (..),
    newListEnvironmentTemplates,

    -- * Request Lenses
    listEnvironmentTemplates_maxResults,
    listEnvironmentTemplates_nextToken,

    -- * Destructuring the Response
    ListEnvironmentTemplatesResponse (..),
    newListEnvironmentTemplatesResponse,

    -- * Response Lenses
    listEnvironmentTemplatesResponse_nextToken,
    listEnvironmentTemplatesResponse_httpStatus,
    listEnvironmentTemplatesResponse_templates,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentTemplates' smart constructor.
data ListEnvironmentTemplates = ListEnvironmentTemplates'
  { -- | The maximum number of environment templates to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the location of the next environment template in
    -- the array of environment templates, after the list of environment
    -- templates that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEnvironmentTemplates_maxResults' - The maximum number of environment templates to list.
--
-- 'nextToken', 'listEnvironmentTemplates_nextToken' - A token that indicates the location of the next environment template in
-- the array of environment templates, after the list of environment
-- templates that was previously requested.
newListEnvironmentTemplates ::
  ListEnvironmentTemplates
newListEnvironmentTemplates =
  ListEnvironmentTemplates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of environment templates to list.
listEnvironmentTemplates_maxResults :: Lens.Lens' ListEnvironmentTemplates (Prelude.Maybe Prelude.Natural)
listEnvironmentTemplates_maxResults = Lens.lens (\ListEnvironmentTemplates' {maxResults} -> maxResults) (\s@ListEnvironmentTemplates' {} a -> s {maxResults = a} :: ListEnvironmentTemplates)

-- | A token that indicates the location of the next environment template in
-- the array of environment templates, after the list of environment
-- templates that was previously requested.
listEnvironmentTemplates_nextToken :: Lens.Lens' ListEnvironmentTemplates (Prelude.Maybe Prelude.Text)
listEnvironmentTemplates_nextToken = Lens.lens (\ListEnvironmentTemplates' {nextToken} -> nextToken) (\s@ListEnvironmentTemplates' {} a -> s {nextToken = a} :: ListEnvironmentTemplates)

instance Core.AWSPager ListEnvironmentTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEnvironmentTemplatesResponse_templates
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEnvironmentTemplates_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEnvironmentTemplates where
  type
    AWSResponse ListEnvironmentTemplates =
      ListEnvironmentTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "templates" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEnvironmentTemplates where
  hashWithSalt _salt ListEnvironmentTemplates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEnvironmentTemplates where
  rnf ListEnvironmentTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEnvironmentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironmentTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEnvironmentTemplates where
  toJSON ListEnvironmentTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEnvironmentTemplates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEnvironmentTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentTemplatesResponse' smart constructor.
data ListEnvironmentTemplatesResponse = ListEnvironmentTemplatesResponse'
  { -- | A token that indicates the location of the next environment template in
    -- the array of environment templates, after the current requested list of
    -- environment templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of environment templates with detail data.
    templates :: [EnvironmentTemplateSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentTemplatesResponse_nextToken' - A token that indicates the location of the next environment template in
-- the array of environment templates, after the current requested list of
-- environment templates.
--
-- 'httpStatus', 'listEnvironmentTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'templates', 'listEnvironmentTemplatesResponse_templates' - An array of environment templates with detail data.
newListEnvironmentTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentTemplatesResponse
newListEnvironmentTemplatesResponse pHttpStatus_ =
  ListEnvironmentTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      templates = Prelude.mempty
    }

-- | A token that indicates the location of the next environment template in
-- the array of environment templates, after the current requested list of
-- environment templates.
listEnvironmentTemplatesResponse_nextToken :: Lens.Lens' ListEnvironmentTemplatesResponse (Prelude.Maybe Prelude.Text)
listEnvironmentTemplatesResponse_nextToken = Lens.lens (\ListEnvironmentTemplatesResponse' {nextToken} -> nextToken) (\s@ListEnvironmentTemplatesResponse' {} a -> s {nextToken = a} :: ListEnvironmentTemplatesResponse)

-- | The response's http status code.
listEnvironmentTemplatesResponse_httpStatus :: Lens.Lens' ListEnvironmentTemplatesResponse Prelude.Int
listEnvironmentTemplatesResponse_httpStatus = Lens.lens (\ListEnvironmentTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentTemplatesResponse' {} a -> s {httpStatus = a} :: ListEnvironmentTemplatesResponse)

-- | An array of environment templates with detail data.
listEnvironmentTemplatesResponse_templates :: Lens.Lens' ListEnvironmentTemplatesResponse [EnvironmentTemplateSummary]
listEnvironmentTemplatesResponse_templates = Lens.lens (\ListEnvironmentTemplatesResponse' {templates} -> templates) (\s@ListEnvironmentTemplatesResponse' {} a -> s {templates = a} :: ListEnvironmentTemplatesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEnvironmentTemplatesResponse
  where
  rnf ListEnvironmentTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templates
