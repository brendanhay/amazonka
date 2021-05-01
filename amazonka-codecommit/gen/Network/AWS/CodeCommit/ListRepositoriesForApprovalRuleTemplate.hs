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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the approval rule template for which you want to list
    -- repositories that are associated with that template.
    approvalRuleTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListRepositoriesForApprovalRuleTemplate
newListRepositoriesForApprovalRuleTemplate
  pApprovalRuleTemplateName_ =
    ListRepositoriesForApprovalRuleTemplate'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listRepositoriesForApprovalRuleTemplate_nextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
listRepositoriesForApprovalRuleTemplate_nextToken = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {nextToken} -> nextToken) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listRepositoriesForApprovalRuleTemplate_maxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Prelude.Maybe Prelude.Int)
listRepositoriesForApprovalRuleTemplate_maxResults = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {maxResults} -> maxResults) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {maxResults = a} :: ListRepositoriesForApprovalRuleTemplate)

-- | The name of the approval rule template for which you want to list
-- repositories that are associated with that template.
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Prelude.Text
listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\ListRepositoriesForApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@ListRepositoriesForApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: ListRepositoriesForApprovalRuleTemplate)

instance
  Prelude.AWSRequest
    ListRepositoriesForApprovalRuleTemplate
  where
  type
    Rs ListRepositoriesForApprovalRuleTemplate =
      ListRepositoriesForApprovalRuleTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesForApprovalRuleTemplateResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
              Prelude.<*> ( x Prelude..?> "repositoryNames"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRepositoriesForApprovalRuleTemplate

instance
  Prelude.NFData
    ListRepositoriesForApprovalRuleTemplate

instance
  Prelude.ToHeaders
    ListRepositoriesForApprovalRuleTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate" ::
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
    ListRepositoriesForApprovalRuleTemplate
  where
  toJSON ListRepositoriesForApprovalRuleTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "approvalRuleTemplateName"
                  Prelude..= approvalRuleTemplateName
              )
          ]
      )

instance
  Prelude.ToPath
    ListRepositoriesForApprovalRuleTemplate
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listRepositoriesForApprovalRuleTemplateResponse_repositoryNames = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {repositoryNames} -> repositoryNames) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {repositoryNames = a} :: ListRepositoriesForApprovalRuleTemplateResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRepositoriesForApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Prelude.Int
listRepositoriesForApprovalRuleTemplateResponse_httpStatus = Lens.lens (\ListRepositoriesForApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesForApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: ListRepositoriesForApprovalRuleTemplateResponse)

instance
  Prelude.NFData
    ListRepositoriesForApprovalRuleTemplateResponse
