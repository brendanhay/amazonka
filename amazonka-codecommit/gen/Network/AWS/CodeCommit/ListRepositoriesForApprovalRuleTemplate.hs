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
-- Module      : Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all repositories associated with the specified approval rule
-- template.
module Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
  ( -- * Creating a Request
    ListRepositoriesForApprovalRuleTemplate (..),
    newListRepositoriesForApprovalRuleTemplate,

    -- * Request Lenses
    listRepositoriesForApprovalRuleTemplate_nextToken,
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName,

    -- * Destructuring the Response
    ListRepositoriesForApprovalRuleTemplateResponse (..),
    newListRepositoriesForApprovalRuleTemplateResponse,

    -- * Response Lenses
    listRepositoriesForApprovalRuleTemplateResponse_nextToken,
    listRepositoriesForApprovalRuleTemplateResponse_repositoryNames,
    listRepositoriesForApprovalRuleTemplateResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Core.Maybe Core.Int,
    -- | The name of the approval rule template for which you want to list
    -- repositories that are associated with that template.
    approvalRuleTemplateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRepositoriesForApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositoriesForApprovalRuleTemplate_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'listRepositoriesForApprovalRuleTemplate_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'approvalRuleTemplateName', 'listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template for which you want to list
-- repositories that are associated with that template.
newListRepositoriesForApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Core.Text ->
  ListRepositoriesForApprovalRuleTemplate
newListRepositoriesForApprovalRuleTemplate
  pApprovalRuleTemplateName_ =
    ListRepositoriesForApprovalRuleTemplate'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listRepositoriesForApprovalRuleTemplate_nextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Core.Text)
listRepositoriesForApprovalRuleTemplate_nextToken = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {nextToken} -> nextToken) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listRepositoriesForApprovalRuleTemplate_maxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Core.Int)
listRepositoriesForApprovalRuleTemplate_maxResults = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {maxResults} -> maxResults) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {maxResults = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | The name of the approval rule template for which you want to list
-- repositories that are associated with that template.
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Core.Text
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: ListRepositoriesForApprovalRuleTemplate)

instance
  Core.AWSRequest
    ListRepositoriesForApprovalRuleTemplate
  where
  type
    AWSResponse
      ListRepositoriesForApprovalRuleTemplate =
      ListRepositoriesForApprovalRuleTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesForApprovalRuleTemplateResponse'
            Core.<$> (x Core..?> "nextToken")
              Core.<*> (x Core..?> "repositoryNames" Core..!@ Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListRepositoriesForApprovalRuleTemplate

instance
  Core.NFData
    ListRepositoriesForApprovalRuleTemplate

instance
  Core.ToHeaders
    ListRepositoriesForApprovalRuleTemplate
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListRepositoriesForApprovalRuleTemplate
  where
  toJSON ListRepositoriesForApprovalRuleTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ( "approvalRuleTemplateName"
                  Core..= approvalRuleTemplateName
              )
          ]
      )

instance
  Core.ToPath
    ListRepositoriesForApprovalRuleTemplate
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListRepositoriesForApprovalRuleTemplate
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRepositoriesForApprovalRuleTemplateResponse' smart constructor.
data ListRepositoriesForApprovalRuleTemplateResponse = ListRepositoriesForApprovalRuleTemplateResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of repository names that are associated with the specified
    -- approval rule template.
    repositoryNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRepositoriesForApprovalRuleTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositoriesForApprovalRuleTemplateResponse_nextToken' - An enumeration token that allows the operation to batch the next results
-- of the operation.
--
-- 'repositoryNames', 'listRepositoriesForApprovalRuleTemplateResponse_repositoryNames' - A list of repository names that are associated with the specified
-- approval rule template.
--
-- 'httpStatus', 'listRepositoriesForApprovalRuleTemplateResponse_httpStatus' - The response's http status code.
newListRepositoriesForApprovalRuleTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRepositoriesForApprovalRuleTemplateResponse
newListRepositoriesForApprovalRuleTemplateResponse
  pHttpStatus_ =
    ListRepositoriesForApprovalRuleTemplateResponse'
      { nextToken =
          Core.Nothing,
        repositoryNames =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listRepositoriesForApprovalRuleTemplateResponse_nextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe Core.Text)
listRepositoriesForApprovalRuleTemplateResponse_nextToken = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {nextToken} -> nextToken) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplateResponse)

-- | A list of repository names that are associated with the specified
-- approval rule template.
listRepositoriesForApprovalRuleTemplateResponse_repositoryNames :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe [Core.Text])
listRepositoriesForApprovalRuleTemplateResponse_repositoryNames = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {repositoryNames} -> repositoryNames) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {repositoryNames = a} :: ListRepositoriesForApprovalRuleTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRepositoriesForApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Core.Int
listRepositoriesForApprovalRuleTemplateResponse_httpStatus = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: ListRepositoriesForApprovalRuleTemplateResponse)

instance
  Core.NFData
    ListRepositoriesForApprovalRuleTemplateResponse
