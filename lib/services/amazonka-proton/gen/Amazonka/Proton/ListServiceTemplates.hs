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
-- Module      : Amazonka.Proton.ListServiceTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List service templates with detail data.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServiceTemplates
  ( -- * Creating a Request
    ListServiceTemplates (..),
    newListServiceTemplates,

    -- * Request Lenses
    listServiceTemplates_nextToken,
    listServiceTemplates_maxResults,

    -- * Destructuring the Response
    ListServiceTemplatesResponse (..),
    newListServiceTemplatesResponse,

    -- * Response Lenses
    listServiceTemplatesResponse_nextToken,
    listServiceTemplatesResponse_httpStatus,
    listServiceTemplatesResponse_templates,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServiceTemplates' smart constructor.
data ListServiceTemplates = ListServiceTemplates'
  { -- | A token that indicates the location of the next service template in the
    -- array of service templates, after the list of service templates
    -- previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of service templates to list.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceTemplates_nextToken' - A token that indicates the location of the next service template in the
-- array of service templates, after the list of service templates
-- previously requested.
--
-- 'maxResults', 'listServiceTemplates_maxResults' - The maximum number of service templates to list.
newListServiceTemplates ::
  ListServiceTemplates
newListServiceTemplates =
  ListServiceTemplates'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token that indicates the location of the next service template in the
-- array of service templates, after the list of service templates
-- previously requested.
listServiceTemplates_nextToken :: Lens.Lens' ListServiceTemplates (Prelude.Maybe Prelude.Text)
listServiceTemplates_nextToken = Lens.lens (\ListServiceTemplates' {nextToken} -> nextToken) (\s@ListServiceTemplates' {} a -> s {nextToken = a} :: ListServiceTemplates)

-- | The maximum number of service templates to list.
listServiceTemplates_maxResults :: Lens.Lens' ListServiceTemplates (Prelude.Maybe Prelude.Natural)
listServiceTemplates_maxResults = Lens.lens (\ListServiceTemplates' {maxResults} -> maxResults) (\s@ListServiceTemplates' {} a -> s {maxResults = a} :: ListServiceTemplates)

instance Core.AWSPager ListServiceTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listServiceTemplatesResponse_templates) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServiceTemplates_nextToken
          Lens..~ rs
          Lens.^? listServiceTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListServiceTemplates where
  type
    AWSResponse ListServiceTemplates =
      ListServiceTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "templates" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServiceTemplates where
  hashWithSalt _salt ListServiceTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListServiceTemplates where
  rnf ListServiceTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListServiceTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.ListServiceTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListServiceTemplates where
  toJSON ListServiceTemplates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListServiceTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServiceTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceTemplatesResponse' smart constructor.
data ListServiceTemplatesResponse = ListServiceTemplatesResponse'
  { -- | A token that indicates the location of the next service template in the
    -- array of service templates, after the current requested list of service
    -- templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of service templates with detail data.
    templates :: [ServiceTemplateSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceTemplatesResponse_nextToken' - A token that indicates the location of the next service template in the
-- array of service templates, after the current requested list of service
-- templates.
--
-- 'httpStatus', 'listServiceTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'templates', 'listServiceTemplatesResponse_templates' - An array of service templates with detail data.
newListServiceTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceTemplatesResponse
newListServiceTemplatesResponse pHttpStatus_ =
  ListServiceTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      templates = Prelude.mempty
    }

-- | A token that indicates the location of the next service template in the
-- array of service templates, after the current requested list of service
-- templates.
listServiceTemplatesResponse_nextToken :: Lens.Lens' ListServiceTemplatesResponse (Prelude.Maybe Prelude.Text)
listServiceTemplatesResponse_nextToken = Lens.lens (\ListServiceTemplatesResponse' {nextToken} -> nextToken) (\s@ListServiceTemplatesResponse' {} a -> s {nextToken = a} :: ListServiceTemplatesResponse)

-- | The response's http status code.
listServiceTemplatesResponse_httpStatus :: Lens.Lens' ListServiceTemplatesResponse Prelude.Int
listServiceTemplatesResponse_httpStatus = Lens.lens (\ListServiceTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListServiceTemplatesResponse' {} a -> s {httpStatus = a} :: ListServiceTemplatesResponse)

-- | An array of service templates with detail data.
listServiceTemplatesResponse_templates :: Lens.Lens' ListServiceTemplatesResponse [ServiceTemplateSummary]
listServiceTemplatesResponse_templates = Lens.lens (\ListServiceTemplatesResponse' {templates} -> templates) (\s@ListServiceTemplatesResponse' {} a -> s {templates = a} :: ListServiceTemplatesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListServiceTemplatesResponse where
  rnf ListServiceTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templates
