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
-- Module      : Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates that are associated with a specified
-- repository.
module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
  ( -- * Creating a Request
    ListAssociatedApprovalRuleTemplatesForRepository (..),
    newListAssociatedApprovalRuleTemplatesForRepository,

    -- * Request Lenses
    listAssociatedApprovalRuleTemplatesForRepository_nextToken,
    listAssociatedApprovalRuleTemplatesForRepository_maxResults,
    listAssociatedApprovalRuleTemplatesForRepository_repositoryName,

    -- * Destructuring the Response
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse (..),
    newListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- * Response Lenses
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Core.Maybe Core.Int,
    -- | The name of the repository for which you want to list all associated
    -- approval rule templates.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedApprovalRuleTemplatesForRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedApprovalRuleTemplatesForRepository_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'listAssociatedApprovalRuleTemplatesForRepository_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'repositoryName', 'listAssociatedApprovalRuleTemplatesForRepository_repositoryName' - The name of the repository for which you want to list all associated
-- approval rule templates.
newListAssociatedApprovalRuleTemplatesForRepository ::
  -- | 'repositoryName'
  Core.Text ->
  ListAssociatedApprovalRuleTemplatesForRepository
newListAssociatedApprovalRuleTemplatesForRepository
  pRepositoryName_ =
    ListAssociatedApprovalRuleTemplatesForRepository'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        repositoryName =
          pRepositoryName_
      }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listAssociatedApprovalRuleTemplatesForRepository_nextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Core.Text)
listAssociatedApprovalRuleTemplatesForRepository_nextToken = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {nextToken} -> nextToken) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listAssociatedApprovalRuleTemplatesForRepository_maxResults :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Core.Int)
listAssociatedApprovalRuleTemplatesForRepository_maxResults = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {maxResults} -> maxResults) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {maxResults = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

-- | The name of the repository for which you want to list all associated
-- approval rule templates.
listAssociatedApprovalRuleTemplatesForRepository_repositoryName :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository Core.Text
listAssociatedApprovalRuleTemplatesForRepository_repositoryName = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {repositoryName} -> repositoryName) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {repositoryName = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

instance
  Core.AWSRequest
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  type
    AWSResponse
      ListAssociatedApprovalRuleTemplatesForRepository =
      ListAssociatedApprovalRuleTemplatesForRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
            Core.<$> (x Core..?> "nextToken")
              Core.<*> ( x Core..?> "approvalRuleTemplateNames"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListAssociatedApprovalRuleTemplatesForRepository

instance
  Core.NFData
    ListAssociatedApprovalRuleTemplatesForRepository

instance
  Core.ToHeaders
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toJSON
    ListAssociatedApprovalRuleTemplatesForRepository' {..} =
      Core.object
        ( Core.catMaybes
            [ ("nextToken" Core..=) Core.<$> nextToken,
              ("maxResults" Core..=) Core.<$> maxResults,
              Core.Just ("repositoryName" Core..= repositoryName)
            ]
        )

instance
  Core.ToPath
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of all approval rule templates associated with the repository.
    approvalRuleTemplateNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedApprovalRuleTemplatesForRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken' - An enumeration token that allows the operation to batch the next results
-- of the operation.
--
-- 'approvalRuleTemplateNames', 'listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames' - The names of all approval rule templates associated with the repository.
--
-- 'httpStatus', 'listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus' - The response's http status code.
newListAssociatedApprovalRuleTemplatesForRepositoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociatedApprovalRuleTemplatesForRepositoryResponse
newListAssociatedApprovalRuleTemplatesForRepositoryResponse
  pHttpStatus_ =
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
      { nextToken =
          Core.Nothing,
        approvalRuleTemplateNames =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe Core.Text)
listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {nextToken} -> nextToken) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)

-- | The names of all approval rule templates associated with the repository.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe [Core.Text])
listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {approvalRuleTemplateNames} -> approvalRuleTemplateNames) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {approvalRuleTemplateNames = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Core.Int
listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {httpStatus = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)

instance
  Core.NFData
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse
