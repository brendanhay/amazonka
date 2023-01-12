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
-- Module      : Amazonka.WorkSpaces.DescribeAccountModifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes modifications to the configuration of
-- Bring Your Own License (BYOL) for the specified account.
--
-- This operation returns paginated results.
module Amazonka.WorkSpaces.DescribeAccountModifications
  ( -- * Creating a Request
    DescribeAccountModifications (..),
    newDescribeAccountModifications,

    -- * Request Lenses
    describeAccountModifications_nextToken,

    -- * Destructuring the Response
    DescribeAccountModificationsResponse (..),
    newDescribeAccountModificationsResponse,

    -- * Response Lenses
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeAccountModifications' smart constructor.
data DescribeAccountModifications = DescribeAccountModifications'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccountModifications_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
newDescribeAccountModifications ::
  DescribeAccountModifications
newDescribeAccountModifications =
  DescribeAccountModifications'
    { nextToken =
        Prelude.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeAccountModifications_nextToken :: Lens.Lens' DescribeAccountModifications (Prelude.Maybe Prelude.Text)
describeAccountModifications_nextToken = Lens.lens (\DescribeAccountModifications' {nextToken} -> nextToken) (\s@DescribeAccountModifications' {} a -> s {nextToken = a} :: DescribeAccountModifications)

instance Core.AWSPager DescribeAccountModifications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_accountModifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAccountModifications_nextToken
          Lens..~ rs
          Lens.^? describeAccountModificationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAccountModifications where
  type
    AWSResponse DescribeAccountModifications =
      DescribeAccountModificationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountModificationsResponse'
            Prelude.<$> ( x Data..?> "AccountModifications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAccountModifications
  where
  hashWithSalt _salt DescribeAccountModifications' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAccountModifications where
  rnf DescribeAccountModifications' {..} =
    Prelude.rnf nextToken

instance Data.ToHeaders DescribeAccountModifications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeAccountModifications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccountModifications where
  toJSON DescribeAccountModifications' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath DescribeAccountModifications where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountModifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountModificationsResponse' smart constructor.
data DescribeAccountModificationsResponse = DescribeAccountModificationsResponse'
  { -- | The list of modifications to the configuration of BYOL.
    accountModifications :: Prelude.Maybe [AccountModification],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountModifications', 'describeAccountModificationsResponse_accountModifications' - The list of modifications to the configuration of BYOL.
--
-- 'nextToken', 'describeAccountModificationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeAccountModificationsResponse_httpStatus' - The response's http status code.
newDescribeAccountModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountModificationsResponse
newDescribeAccountModificationsResponse pHttpStatus_ =
  DescribeAccountModificationsResponse'
    { accountModifications =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of modifications to the configuration of BYOL.
describeAccountModificationsResponse_accountModifications :: Lens.Lens' DescribeAccountModificationsResponse (Prelude.Maybe [AccountModification])
describeAccountModificationsResponse_accountModifications = Lens.lens (\DescribeAccountModificationsResponse' {accountModifications} -> accountModifications) (\s@DescribeAccountModificationsResponse' {} a -> s {accountModifications = a} :: DescribeAccountModificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeAccountModificationsResponse_nextToken :: Lens.Lens' DescribeAccountModificationsResponse (Prelude.Maybe Prelude.Text)
describeAccountModificationsResponse_nextToken = Lens.lens (\DescribeAccountModificationsResponse' {nextToken} -> nextToken) (\s@DescribeAccountModificationsResponse' {} a -> s {nextToken = a} :: DescribeAccountModificationsResponse)

-- | The response's http status code.
describeAccountModificationsResponse_httpStatus :: Lens.Lens' DescribeAccountModificationsResponse Prelude.Int
describeAccountModificationsResponse_httpStatus = Lens.lens (\DescribeAccountModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountModificationsResponse' {} a -> s {httpStatus = a} :: DescribeAccountModificationsResponse)

instance
  Prelude.NFData
    DescribeAccountModificationsResponse
  where
  rnf DescribeAccountModificationsResponse' {..} =
    Prelude.rnf accountModifications
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
