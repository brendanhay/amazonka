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
-- Module      : Amazonka.Macie.ListMemberAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Discontinued) Lists all Amazon Macie Classic member accounts for the
-- current Macie Classic administrator account.
--
-- This operation returns paginated results.
module Amazonka.Macie.ListMemberAccounts
  ( -- * Creating a Request
    ListMemberAccounts (..),
    newListMemberAccounts,

    -- * Request Lenses
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,

    -- * Destructuring the Response
    ListMemberAccountsResponse (..),
    newListMemberAccountsResponse,

    -- * Response Lenses
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Macie.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMemberAccounts' smart constructor.
data ListMemberAccounts = ListMemberAccounts'
  { -- | (Discontinued) Use this parameter when paginating results. Set the value
    -- of this parameter to null on your first call to the @ListMemberAccounts@
    -- action. Subsequent calls to the action fill @nextToken@ in the request
    -- with the value of @nextToken@ from the previous response to continue
    -- listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) Use this parameter to indicate the maximum number of
    -- items that you want in the response. The default value is 250.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMemberAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMemberAccounts_nextToken' - (Discontinued) Use this parameter when paginating results. Set the value
-- of this parameter to null on your first call to the @ListMemberAccounts@
-- action. Subsequent calls to the action fill @nextToken@ in the request
-- with the value of @nextToken@ from the previous response to continue
-- listing data.
--
-- 'maxResults', 'listMemberAccounts_maxResults' - (Discontinued) Use this parameter to indicate the maximum number of
-- items that you want in the response. The default value is 250.
newListMemberAccounts ::
  ListMemberAccounts
newListMemberAccounts =
  ListMemberAccounts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | (Discontinued) Use this parameter when paginating results. Set the value
-- of this parameter to null on your first call to the @ListMemberAccounts@
-- action. Subsequent calls to the action fill @nextToken@ in the request
-- with the value of @nextToken@ from the previous response to continue
-- listing data.
listMemberAccounts_nextToken :: Lens.Lens' ListMemberAccounts (Prelude.Maybe Prelude.Text)
listMemberAccounts_nextToken = Lens.lens (\ListMemberAccounts' {nextToken} -> nextToken) (\s@ListMemberAccounts' {} a -> s {nextToken = a} :: ListMemberAccounts)

-- | (Discontinued) Use this parameter to indicate the maximum number of
-- items that you want in the response. The default value is 250.
listMemberAccounts_maxResults :: Lens.Lens' ListMemberAccounts (Prelude.Maybe Prelude.Int)
listMemberAccounts_maxResults = Lens.lens (\ListMemberAccounts' {maxResults} -> maxResults) (\s@ListMemberAccounts' {} a -> s {maxResults = a} :: ListMemberAccounts)

instance Core.AWSPager ListMemberAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_memberAccounts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMemberAccounts_nextToken
          Lens..~ rs
          Lens.^? listMemberAccountsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMemberAccounts where
  type
    AWSResponse ListMemberAccounts =
      ListMemberAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMemberAccountsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "memberAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMemberAccounts where
  hashWithSalt _salt ListMemberAccounts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMemberAccounts where
  rnf ListMemberAccounts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListMemberAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MacieService.ListMemberAccounts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMemberAccounts where
  toJSON ListMemberAccounts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListMemberAccounts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMemberAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMemberAccountsResponse' smart constructor.
data ListMemberAccountsResponse = ListMemberAccountsResponse'
  { -- | (Discontinued) When a response is generated, if there is more data to be
    -- listed, this parameter is present in the response and contains the value
    -- to use for the @nextToken@ parameter in a subsequent pagination request.
    -- If there is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) A list of the Amazon Macie Classic member accounts
    -- returned by the action. The current Macie Classic administrator account
    -- is also included in this list.
    memberAccounts :: Prelude.Maybe [MemberAccount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMemberAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMemberAccountsResponse_nextToken' - (Discontinued) When a response is generated, if there is more data to be
-- listed, this parameter is present in the response and contains the value
-- to use for the @nextToken@ parameter in a subsequent pagination request.
-- If there is no more data to be listed, this parameter is set to null.
--
-- 'memberAccounts', 'listMemberAccountsResponse_memberAccounts' - (Discontinued) A list of the Amazon Macie Classic member accounts
-- returned by the action. The current Macie Classic administrator account
-- is also included in this list.
--
-- 'httpStatus', 'listMemberAccountsResponse_httpStatus' - The response's http status code.
newListMemberAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMemberAccountsResponse
newListMemberAccountsResponse pHttpStatus_ =
  ListMemberAccountsResponse'
    { nextToken =
        Prelude.Nothing,
      memberAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Discontinued) When a response is generated, if there is more data to be
-- listed, this parameter is present in the response and contains the value
-- to use for the @nextToken@ parameter in a subsequent pagination request.
-- If there is no more data to be listed, this parameter is set to null.
listMemberAccountsResponse_nextToken :: Lens.Lens' ListMemberAccountsResponse (Prelude.Maybe Prelude.Text)
listMemberAccountsResponse_nextToken = Lens.lens (\ListMemberAccountsResponse' {nextToken} -> nextToken) (\s@ListMemberAccountsResponse' {} a -> s {nextToken = a} :: ListMemberAccountsResponse)

-- | (Discontinued) A list of the Amazon Macie Classic member accounts
-- returned by the action. The current Macie Classic administrator account
-- is also included in this list.
listMemberAccountsResponse_memberAccounts :: Lens.Lens' ListMemberAccountsResponse (Prelude.Maybe [MemberAccount])
listMemberAccountsResponse_memberAccounts = Lens.lens (\ListMemberAccountsResponse' {memberAccounts} -> memberAccounts) (\s@ListMemberAccountsResponse' {} a -> s {memberAccounts = a} :: ListMemberAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMemberAccountsResponse_httpStatus :: Lens.Lens' ListMemberAccountsResponse Prelude.Int
listMemberAccountsResponse_httpStatus = Lens.lens (\ListMemberAccountsResponse' {httpStatus} -> httpStatus) (\s@ListMemberAccountsResponse' {} a -> s {httpStatus = a} :: ListMemberAccountsResponse)

instance Prelude.NFData ListMemberAccountsResponse where
  rnf ListMemberAccountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf memberAccounts
      `Prelude.seq` Prelude.rnf httpStatus
