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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProvisioningTemplates' smart constructor.
data ListProvisioningTemplates = ListProvisioningTemplates'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplates_nextToken :: Lens.Lens' ListProvisioningTemplates (Core.Maybe Core.Text)
listProvisioningTemplates_nextToken = Lens.lens (\ListProvisioningTemplates' {nextToken} -> nextToken) (\s@ListProvisioningTemplates' {} a -> s {nextToken = a} :: ListProvisioningTemplates)

-- | The maximum number of results to return at one time.
listProvisioningTemplates_maxResults :: Lens.Lens' ListProvisioningTemplates (Core.Maybe Core.Natural)
listProvisioningTemplates_maxResults = Lens.lens (\ListProvisioningTemplates' {maxResults} -> maxResults) (\s@ListProvisioningTemplates' {} a -> s {maxResults = a} :: ListProvisioningTemplates)

instance Core.AWSPager ListProvisioningTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplatesResponse_templates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProvisioningTemplates_nextToken
          Lens..~ rs
          Lens.^? listProvisioningTemplatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListProvisioningTemplates where
  type
    AWSResponse ListProvisioningTemplates =
      ListProvisioningTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplatesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "templates" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProvisioningTemplates

instance Core.NFData ListProvisioningTemplates

instance Core.ToHeaders ListProvisioningTemplates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListProvisioningTemplates where
  toPath = Core.const "/provisioning-templates"

instance Core.ToQuery ListProvisioningTemplates where
  toQuery ListProvisioningTemplates' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProvisioningTemplatesResponse' smart constructor.
data ListProvisioningTemplatesResponse = ListProvisioningTemplatesResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of fleet provisioning templates
    templates :: Core.Maybe [ProvisioningTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListProvisioningTemplatesResponse
newListProvisioningTemplatesResponse pHttpStatus_ =
  ListProvisioningTemplatesResponse'
    { nextToken =
        Core.Nothing,
      templates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplatesResponse_nextToken :: Lens.Lens' ListProvisioningTemplatesResponse (Core.Maybe Core.Text)
listProvisioningTemplatesResponse_nextToken = Lens.lens (\ListProvisioningTemplatesResponse' {nextToken} -> nextToken) (\s@ListProvisioningTemplatesResponse' {} a -> s {nextToken = a} :: ListProvisioningTemplatesResponse)

-- | A list of fleet provisioning templates
listProvisioningTemplatesResponse_templates :: Lens.Lens' ListProvisioningTemplatesResponse (Core.Maybe [ProvisioningTemplateSummary])
listProvisioningTemplatesResponse_templates = Lens.lens (\ListProvisioningTemplatesResponse' {templates} -> templates) (\s@ListProvisioningTemplatesResponse' {} a -> s {templates = a} :: ListProvisioningTemplatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProvisioningTemplatesResponse_httpStatus :: Lens.Lens' ListProvisioningTemplatesResponse Core.Int
listProvisioningTemplatesResponse_httpStatus = Lens.lens (\ListProvisioningTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningTemplatesResponse' {} a -> s {httpStatus = a} :: ListProvisioningTemplatesResponse)

instance
  Core.NFData
    ListProvisioningTemplatesResponse
