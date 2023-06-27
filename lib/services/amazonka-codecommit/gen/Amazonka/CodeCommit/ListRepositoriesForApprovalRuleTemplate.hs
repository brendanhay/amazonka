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
-- Module      : Amazonka.CodeCommit.ListRepositoriesForApprovalRuleTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all repositories associated with the specified approval rule
-- template.
module Amazonka.CodeCommit.ListRepositoriesForApprovalRuleTemplate
  ( -- * Creating a Request
    ListRepositoriesForApprovalRuleTemplate (..),
    newListRepositoriesForApprovalRuleTemplate,

    -- * Request Lenses
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_nextToken,
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the approval rule template for which you want to list
    -- repositories that are associated with that template.
    approvalRuleTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoriesForApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRepositoriesForApprovalRuleTemplate_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'nextToken', 'listRepositoriesForApprovalRuleTemplate_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'approvalRuleTemplateName', 'listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template for which you want to list
-- repositories that are associated with that template.
newListRepositoriesForApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  ListRepositoriesForApprovalRuleTemplate
newListRepositoriesForApprovalRuleTemplate
  pApprovalRuleTemplateName_ =
    ListRepositoriesForApprovalRuleTemplate'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listRepositoriesForApprovalRuleTemplate_maxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Prelude.Maybe Prelude.Int)
listRepositoriesForApprovalRuleTemplate_maxResults = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {maxResults} -> maxResults) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {maxResults = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listRepositoriesForApprovalRuleTemplate_nextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
listRepositoriesForApprovalRuleTemplate_nextToken = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {nextToken} -> nextToken) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | The name of the approval rule template for which you want to list
-- repositories that are associated with that template.
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Prelude.Text
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: ListRepositoriesForApprovalRuleTemplate)

instance
  Core.AWSRequest
    ListRepositoriesForApprovalRuleTemplate
  where
  type
    AWSResponse
      ListRepositoriesForApprovalRuleTemplate =
      ListRepositoriesForApprovalRuleTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesForApprovalRuleTemplateResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "repositoryNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRepositoriesForApprovalRuleTemplate
  where
  hashWithSalt
    _salt
    ListRepositoriesForApprovalRuleTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` approvalRuleTemplateName

instance
  Prelude.NFData
    ListRepositoriesForApprovalRuleTemplate
  where
  rnf ListRepositoriesForApprovalRuleTemplate' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf approvalRuleTemplateName

instance
  Data.ToHeaders
    ListRepositoriesForApprovalRuleTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListRepositoriesForApprovalRuleTemplate
  where
  toJSON ListRepositoriesForApprovalRuleTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "approvalRuleTemplateName"
                  Data..= approvalRuleTemplateName
              )
          ]
      )

instance
  Data.ToPath
    ListRepositoriesForApprovalRuleTemplate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListRepositoriesForApprovalRuleTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRepositoriesForApprovalRuleTemplateResponse' smart constructor.
data ListRepositoriesForApprovalRuleTemplateResponse = ListRepositoriesForApprovalRuleTemplateResponse'
  { -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of repository names that are associated with the specified
    -- approval rule template.
    repositoryNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListRepositoriesForApprovalRuleTemplateResponse
newListRepositoriesForApprovalRuleTemplateResponse
  pHttpStatus_ =
    ListRepositoriesForApprovalRuleTemplateResponse'
      { nextToken =
          Prelude.Nothing,
        repositoryNames =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listRepositoriesForApprovalRuleTemplateResponse_nextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Prelude.Maybe Prelude.Text)
listRepositoriesForApprovalRuleTemplateResponse_nextToken = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {nextToken} -> nextToken) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplateResponse)

-- | A list of repository names that are associated with the specified
-- approval rule template.
listRepositoriesForApprovalRuleTemplateResponse_repositoryNames :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Prelude.Maybe [Prelude.Text])
listRepositoriesForApprovalRuleTemplateResponse_repositoryNames = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {repositoryNames} -> repositoryNames) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {repositoryNames = a} :: ListRepositoriesForApprovalRuleTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRepositoriesForApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Prelude.Int
listRepositoriesForApprovalRuleTemplateResponse_httpStatus = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: ListRepositoriesForApprovalRuleTemplateResponse)

instance
  Prelude.NFData
    ListRepositoriesForApprovalRuleTemplateResponse
  where
  rnf
    ListRepositoriesForApprovalRuleTemplateResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf repositoryNames
        `Prelude.seq` Prelude.rnf httpStatus
