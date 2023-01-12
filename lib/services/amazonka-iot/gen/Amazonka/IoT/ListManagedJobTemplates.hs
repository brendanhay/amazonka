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
-- Module      : Amazonka.IoT.ListManagedJobTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of managed job templates.
module Amazonka.IoT.ListManagedJobTemplates
  ( -- * Creating a Request
    ListManagedJobTemplates (..),
    newListManagedJobTemplates,

    -- * Request Lenses
    listManagedJobTemplates_maxResults,
    listManagedJobTemplates_nextToken,
    listManagedJobTemplates_templateName,

    -- * Destructuring the Response
    ListManagedJobTemplatesResponse (..),
    newListManagedJobTemplatesResponse,

    -- * Response Lenses
    listManagedJobTemplatesResponse_managedJobTemplates,
    listManagedJobTemplatesResponse_nextToken,
    listManagedJobTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedJobTemplates' smart constructor.
data ListManagedJobTemplates = ListManagedJobTemplates'
  { -- | Maximum number of entries that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter for template name. If specified, only the versions
    -- of the managed job templates that have the specified template name will
    -- be returned.
    templateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedJobTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listManagedJobTemplates_maxResults' - Maximum number of entries that can be returned.
--
-- 'nextToken', 'listManagedJobTemplates_nextToken' - The token to retrieve the next set of results.
--
-- 'templateName', 'listManagedJobTemplates_templateName' - An optional parameter for template name. If specified, only the versions
-- of the managed job templates that have the specified template name will
-- be returned.
newListManagedJobTemplates ::
  ListManagedJobTemplates
newListManagedJobTemplates =
  ListManagedJobTemplates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      templateName = Prelude.Nothing
    }

-- | Maximum number of entries that can be returned.
listManagedJobTemplates_maxResults :: Lens.Lens' ListManagedJobTemplates (Prelude.Maybe Prelude.Natural)
listManagedJobTemplates_maxResults = Lens.lens (\ListManagedJobTemplates' {maxResults} -> maxResults) (\s@ListManagedJobTemplates' {} a -> s {maxResults = a} :: ListManagedJobTemplates)

-- | The token to retrieve the next set of results.
listManagedJobTemplates_nextToken :: Lens.Lens' ListManagedJobTemplates (Prelude.Maybe Prelude.Text)
listManagedJobTemplates_nextToken = Lens.lens (\ListManagedJobTemplates' {nextToken} -> nextToken) (\s@ListManagedJobTemplates' {} a -> s {nextToken = a} :: ListManagedJobTemplates)

-- | An optional parameter for template name. If specified, only the versions
-- of the managed job templates that have the specified template name will
-- be returned.
listManagedJobTemplates_templateName :: Lens.Lens' ListManagedJobTemplates (Prelude.Maybe Prelude.Text)
listManagedJobTemplates_templateName = Lens.lens (\ListManagedJobTemplates' {templateName} -> templateName) (\s@ListManagedJobTemplates' {} a -> s {templateName = a} :: ListManagedJobTemplates)

instance Core.AWSRequest ListManagedJobTemplates where
  type
    AWSResponse ListManagedJobTemplates =
      ListManagedJobTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedJobTemplatesResponse'
            Prelude.<$> ( x Data..?> "managedJobTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedJobTemplates where
  hashWithSalt _salt ListManagedJobTemplates' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ListManagedJobTemplates where
  rnf ListManagedJobTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders ListManagedJobTemplates where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListManagedJobTemplates where
  toPath = Prelude.const "/managed-job-templates"

instance Data.ToQuery ListManagedJobTemplates where
  toQuery ListManagedJobTemplates' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "templateName" Data.=: templateName
      ]

-- | /See:/ 'newListManagedJobTemplatesResponse' smart constructor.
data ListManagedJobTemplatesResponse = ListManagedJobTemplatesResponse'
  { -- | A list of managed job templates that are returned.
    managedJobTemplates :: Prelude.Maybe [ManagedJobTemplateSummary],
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedJobTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedJobTemplates', 'listManagedJobTemplatesResponse_managedJobTemplates' - A list of managed job templates that are returned.
--
-- 'nextToken', 'listManagedJobTemplatesResponse_nextToken' - The token to retrieve the next set of results.
--
-- 'httpStatus', 'listManagedJobTemplatesResponse_httpStatus' - The response's http status code.
newListManagedJobTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedJobTemplatesResponse
newListManagedJobTemplatesResponse pHttpStatus_ =
  ListManagedJobTemplatesResponse'
    { managedJobTemplates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of managed job templates that are returned.
listManagedJobTemplatesResponse_managedJobTemplates :: Lens.Lens' ListManagedJobTemplatesResponse (Prelude.Maybe [ManagedJobTemplateSummary])
listManagedJobTemplatesResponse_managedJobTemplates = Lens.lens (\ListManagedJobTemplatesResponse' {managedJobTemplates} -> managedJobTemplates) (\s@ListManagedJobTemplatesResponse' {} a -> s {managedJobTemplates = a} :: ListManagedJobTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results.
listManagedJobTemplatesResponse_nextToken :: Lens.Lens' ListManagedJobTemplatesResponse (Prelude.Maybe Prelude.Text)
listManagedJobTemplatesResponse_nextToken = Lens.lens (\ListManagedJobTemplatesResponse' {nextToken} -> nextToken) (\s@ListManagedJobTemplatesResponse' {} a -> s {nextToken = a} :: ListManagedJobTemplatesResponse)

-- | The response's http status code.
listManagedJobTemplatesResponse_httpStatus :: Lens.Lens' ListManagedJobTemplatesResponse Prelude.Int
listManagedJobTemplatesResponse_httpStatus = Lens.lens (\ListManagedJobTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListManagedJobTemplatesResponse' {} a -> s {httpStatus = a} :: ListManagedJobTemplatesResponse)

instance
  Prelude.NFData
    ListManagedJobTemplatesResponse
  where
  rnf ListManagedJobTemplatesResponse' {..} =
    Prelude.rnf managedJobTemplates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
