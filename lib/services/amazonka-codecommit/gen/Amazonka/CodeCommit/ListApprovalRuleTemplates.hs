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
-- Module      : Amazonka.CodeCommit.ListApprovalRuleTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates in the specified AWS Region in your
-- AWS account. If an AWS Region is not specified, the AWS Region where you
-- are signed in is used.
module Amazonka.CodeCommit.ListApprovalRuleTemplates
  ( -- * Creating a Request
    ListApprovalRuleTemplates (..),
    newListApprovalRuleTemplates,

    -- * Request Lenses
    listApprovalRuleTemplates_maxResults,
    listApprovalRuleTemplates_nextToken,

    -- * Destructuring the Response
    ListApprovalRuleTemplatesResponse (..),
    newListApprovalRuleTemplatesResponse,

    -- * Response Lenses
    listApprovalRuleTemplatesResponse_approvalRuleTemplateNames,
    listApprovalRuleTemplatesResponse_nextToken,
    listApprovalRuleTemplatesResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApprovalRuleTemplates' smart constructor.
data ListApprovalRuleTemplates = ListApprovalRuleTemplates'
  { -- | A non-zero, non-negative integer used to limit the number of returned
    -- results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApprovalRuleTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listApprovalRuleTemplates_maxResults' - A non-zero, non-negative integer used to limit the number of returned
-- results.
--
-- 'nextToken', 'listApprovalRuleTemplates_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
newListApprovalRuleTemplates ::
  ListApprovalRuleTemplates
newListApprovalRuleTemplates =
  ListApprovalRuleTemplates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A non-zero, non-negative integer used to limit the number of returned
-- results.
listApprovalRuleTemplates_maxResults :: Lens.Lens' ListApprovalRuleTemplates (Prelude.Maybe Prelude.Int)
listApprovalRuleTemplates_maxResults = Lens.lens (\ListApprovalRuleTemplates' {maxResults} -> maxResults) (\s@ListApprovalRuleTemplates' {} a -> s {maxResults = a} :: ListApprovalRuleTemplates)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listApprovalRuleTemplates_nextToken :: Lens.Lens' ListApprovalRuleTemplates (Prelude.Maybe Prelude.Text)
listApprovalRuleTemplates_nextToken = Lens.lens (\ListApprovalRuleTemplates' {nextToken} -> nextToken) (\s@ListApprovalRuleTemplates' {} a -> s {nextToken = a} :: ListApprovalRuleTemplates)

instance Core.AWSRequest ListApprovalRuleTemplates where
  type
    AWSResponse ListApprovalRuleTemplates =
      ListApprovalRuleTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApprovalRuleTemplatesResponse'
            Prelude.<$> ( x
                            Data..?> "approvalRuleTemplateNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApprovalRuleTemplates where
  hashWithSalt _salt ListApprovalRuleTemplates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApprovalRuleTemplates where
  rnf ListApprovalRuleTemplates' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListApprovalRuleTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.ListApprovalRuleTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApprovalRuleTemplates where
  toJSON ListApprovalRuleTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApprovalRuleTemplates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApprovalRuleTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApprovalRuleTemplatesResponse' smart constructor.
data ListApprovalRuleTemplatesResponse = ListApprovalRuleTemplatesResponse'
  { -- | The names of all the approval rule templates found in the AWS Region for
    -- your AWS account.
    approvalRuleTemplateNames :: Prelude.Maybe [Prelude.Text],
    -- | An enumeration token that allows the operation to batch the next results
    -- of the operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApprovalRuleTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateNames', 'listApprovalRuleTemplatesResponse_approvalRuleTemplateNames' - The names of all the approval rule templates found in the AWS Region for
-- your AWS account.
--
-- 'nextToken', 'listApprovalRuleTemplatesResponse_nextToken' - An enumeration token that allows the operation to batch the next results
-- of the operation.
--
-- 'httpStatus', 'listApprovalRuleTemplatesResponse_httpStatus' - The response's http status code.
newListApprovalRuleTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApprovalRuleTemplatesResponse
newListApprovalRuleTemplatesResponse pHttpStatus_ =
  ListApprovalRuleTemplatesResponse'
    { approvalRuleTemplateNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of all the approval rule templates found in the AWS Region for
-- your AWS account.
listApprovalRuleTemplatesResponse_approvalRuleTemplateNames :: Lens.Lens' ListApprovalRuleTemplatesResponse (Prelude.Maybe [Prelude.Text])
listApprovalRuleTemplatesResponse_approvalRuleTemplateNames = Lens.lens (\ListApprovalRuleTemplatesResponse' {approvalRuleTemplateNames} -> approvalRuleTemplateNames) (\s@ListApprovalRuleTemplatesResponse' {} a -> s {approvalRuleTemplateNames = a} :: ListApprovalRuleTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An enumeration token that allows the operation to batch the next results
-- of the operation.
listApprovalRuleTemplatesResponse_nextToken :: Lens.Lens' ListApprovalRuleTemplatesResponse (Prelude.Maybe Prelude.Text)
listApprovalRuleTemplatesResponse_nextToken = Lens.lens (\ListApprovalRuleTemplatesResponse' {nextToken} -> nextToken) (\s@ListApprovalRuleTemplatesResponse' {} a -> s {nextToken = a} :: ListApprovalRuleTemplatesResponse)

-- | The response's http status code.
listApprovalRuleTemplatesResponse_httpStatus :: Lens.Lens' ListApprovalRuleTemplatesResponse Prelude.Int
listApprovalRuleTemplatesResponse_httpStatus = Lens.lens (\ListApprovalRuleTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListApprovalRuleTemplatesResponse' {} a -> s {httpStatus = a} :: ListApprovalRuleTemplatesResponse)

instance
  Prelude.NFData
    ListApprovalRuleTemplatesResponse
  where
  rnf ListApprovalRuleTemplatesResponse' {..} =
    Prelude.rnf approvalRuleTemplateNames `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
