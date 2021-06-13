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
-- Module      : Network.AWS.IoT.ListProvisioningTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the fleet provisioning templates in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplates
  ( -- * Creating a Request
    ListProvisioningTemplates (..),
    newListProvisioningTemplates,

    -- * Request Lenses
    listProvisioningTemplates_nextToken,
    listProvisioningTemplates_maxResults,

    -- * Destructuring the Response
    ListProvisioningTemplatesResponse (..),
    newListProvisioningTemplatesResponse,

    -- * Response Lenses
    listProvisioningTemplatesResponse_nextToken,
    listProvisioningTemplatesResponse_templates,
    listProvisioningTemplatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProvisioningTemplates' smart constructor.
data ListProvisioningTemplates = ListProvisioningTemplates'
  { -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisioningTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProvisioningTemplates_nextToken' - A token to retrieve the next set of results.
--
-- 'maxResults', 'listProvisioningTemplates_maxResults' - The maximum number of results to return at one time.
newListProvisioningTemplates ::
  ListProvisioningTemplates
newListProvisioningTemplates =
  ListProvisioningTemplates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplates_nextToken :: Lens.Lens' ListProvisioningTemplates (Prelude.Maybe Prelude.Text)
listProvisioningTemplates_nextToken = Lens.lens (\ListProvisioningTemplates' {nextToken} -> nextToken) (\s@ListProvisioningTemplates' {} a -> s {nextToken = a} :: ListProvisioningTemplates)

-- | The maximum number of results to return at one time.
listProvisioningTemplates_maxResults :: Lens.Lens' ListProvisioningTemplates (Prelude.Maybe Prelude.Natural)
listProvisioningTemplates_maxResults = Lens.lens (\ListProvisioningTemplates' {maxResults} -> maxResults) (\s@ListProvisioningTemplates' {} a -> s {maxResults = a} :: ListProvisioningTemplates)

instance Core.AWSPager ListProvisioningTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplatesResponse_templates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProvisioningTemplates_nextToken
          Lens..~ rs
          Lens.^? listProvisioningTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProvisioningTemplates where
  type
    AWSResponse ListProvisioningTemplates =
      ListProvisioningTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "templates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProvisioningTemplates

instance Prelude.NFData ListProvisioningTemplates

instance Core.ToHeaders ListProvisioningTemplates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListProvisioningTemplates where
  toPath = Prelude.const "/provisioning-templates"

instance Core.ToQuery ListProvisioningTemplates where
  toQuery ListProvisioningTemplates' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProvisioningTemplatesResponse' smart constructor.
data ListProvisioningTemplatesResponse = ListProvisioningTemplatesResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of fleet provisioning templates
    templates :: Prelude.Maybe [ProvisioningTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisioningTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProvisioningTemplatesResponse_nextToken' - A token to retrieve the next set of results.
--
-- 'templates', 'listProvisioningTemplatesResponse_templates' - A list of fleet provisioning templates
--
-- 'httpStatus', 'listProvisioningTemplatesResponse_httpStatus' - The response's http status code.
newListProvisioningTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisioningTemplatesResponse
newListProvisioningTemplatesResponse pHttpStatus_ =
  ListProvisioningTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      templates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplatesResponse_nextToken :: Lens.Lens' ListProvisioningTemplatesResponse (Prelude.Maybe Prelude.Text)
listProvisioningTemplatesResponse_nextToken = Lens.lens (\ListProvisioningTemplatesResponse' {nextToken} -> nextToken) (\s@ListProvisioningTemplatesResponse' {} a -> s {nextToken = a} :: ListProvisioningTemplatesResponse)

-- | A list of fleet provisioning templates
listProvisioningTemplatesResponse_templates :: Lens.Lens' ListProvisioningTemplatesResponse (Prelude.Maybe [ProvisioningTemplateSummary])
listProvisioningTemplatesResponse_templates = Lens.lens (\ListProvisioningTemplatesResponse' {templates} -> templates) (\s@ListProvisioningTemplatesResponse' {} a -> s {templates = a} :: ListProvisioningTemplatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProvisioningTemplatesResponse_httpStatus :: Lens.Lens' ListProvisioningTemplatesResponse Prelude.Int
listProvisioningTemplatesResponse_httpStatus = Lens.lens (\ListProvisioningTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningTemplatesResponse' {} a -> s {httpStatus = a} :: ListProvisioningTemplatesResponse)

instance
  Prelude.NFData
    ListProvisioningTemplatesResponse
