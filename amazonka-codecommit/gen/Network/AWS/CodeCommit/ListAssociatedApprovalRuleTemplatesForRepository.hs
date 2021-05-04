{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the repository for which you want to list all associated
    -- approval rule templates.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListAssociatedApprovalRuleTemplatesForRepository
newListAssociatedApprovalRuleTemplatesForRepository
  pRepositoryName_ =
    ListAssociatedApprovalRuleTemplatesForRepository'
      { nextToken =
          Prelude.Nothing,
        maxResults =
          Prelude.Nothing,
        repositoryName =
          pRepositoryName_
      }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listAssociatedApprovalRuleTemplatesForRepository_nextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Prelude.Maybe Prelude.Text)
listAssociatedApprovalRuleTemplatesForRepository_nextToken = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {nextToken} -> nextToken) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listAssociatedApprovalRuleTemplatesForRepository_maxResults :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Prelude.Maybe Prelude.Int)
listAssociatedApprovalRuleTemplatesForRepository_maxResults = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {maxResults} -> maxResults) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {maxResults = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

-- | The name of the repository for which you want to list all associated
-- approval rule templates.
listAssociatedApprovalRuleTemplatesForRepository_repositoryName :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository Prelude.Text
listAssociatedApprovalRuleTemplatesForRepository_repositoryName = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepository' {repositoryName} -> repositoryName) (\s@ListAssociatedApprovalRuleTemplatesForRepository' {} a -> s {repositoryName = a} :: ListAssociatedApprovalRuleTemplatesForRepository)

instance
  Prelude.AWSRequest
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  type
    Rs
      ListAssociatedApprovalRuleTemplatesForRepository =
      ListAssociatedApprovalRuleTemplatesForRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
              Prelude.<*> ( x Prelude..?> "approvalRuleTemplateNames"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAssociatedApprovalRuleTemplatesForRepository

instance
  Prelude.NFData
    ListAssociatedApprovalRuleTemplatesForRepository

instance
  Prelude.ToHeaders
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toJSON
    ListAssociatedApprovalRuleTemplatesForRepository' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
              ("maxResults" Prelude..=) Prelude.<$> maxResults,
              Prelude.Just
                ("repositoryName" Prelude..= repositoryName)
            ]
        )

instance
  Prelude.ToPath
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of all approval rule templates associated with the repository.
    approvalRuleTemplateNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListAssociatedApprovalRuleTemplatesForRepositoryResponse
newListAssociatedApprovalRuleTemplatesForRepositoryResponse
  pHttpStatus_ =
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
      { nextToken =
          Prelude.Nothing,
        approvalRuleTemplateNames =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Prelude.Maybe Prelude.Text)
listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {nextToken} -> nextToken) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)

-- | The names of all approval rule templates associated with the repository.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Prelude.Maybe [Prelude.Text])
listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {approvalRuleTemplateNames} -> approvalRuleTemplateNames) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {approvalRuleTemplateNames = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Prelude.Int
listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus = Lens.lens (\ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedApprovalRuleTemplatesForRepositoryResponse' {} a -> s {httpStatus = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)

instance
  Prelude.NFData
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse
