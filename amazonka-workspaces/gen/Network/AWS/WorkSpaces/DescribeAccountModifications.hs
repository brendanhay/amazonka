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
-- Module      : Network.AWS.WorkSpaces.DescribeAccountModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes modifications to the configuration of
-- Bring Your Own License (BYOL) for the specified account.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeAccountModifications
  ( -- * Creating a Request
    DescribeAccountModifications (..),
    newDescribeAccountModifications,

    -- * Request Lenses
    describeAccountModifications_nextToken,

    -- * Destructuring the Response
    DescribeAccountModificationsResponse (..),
    newDescribeAccountModificationsResponse,

    -- * Response Lenses
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeAccountModifications' smart constructor.
data DescribeAccountModifications = DescribeAccountModifications'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager DescribeAccountModifications where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_accountModifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeAccountModifications_nextToken
          Lens..~ rs
          Lens.^? describeAccountModificationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeAccountModifications
  where
  type
    Rs DescribeAccountModifications =
      DescribeAccountModificationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountModificationsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AccountModifications"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAccountModifications

instance Prelude.NFData DescribeAccountModifications

instance
  Prelude.ToHeaders
    DescribeAccountModifications
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DescribeAccountModifications" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAccountModifications where
  toJSON DescribeAccountModifications' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("NextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath DescribeAccountModifications where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAccountModifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountModificationsResponse' smart constructor.
data DescribeAccountModificationsResponse = DescribeAccountModificationsResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of modifications to the configuration of BYOL.
    accountModifications :: Prelude.Maybe [AccountModification],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccountModificationsResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'accountModifications', 'describeAccountModificationsResponse_accountModifications' - The list of modifications to the configuration of BYOL.
--
-- 'httpStatus', 'describeAccountModificationsResponse_httpStatus' - The response's http status code.
newDescribeAccountModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountModificationsResponse
newDescribeAccountModificationsResponse pHttpStatus_ =
  DescribeAccountModificationsResponse'
    { nextToken =
        Prelude.Nothing,
      accountModifications =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeAccountModificationsResponse_nextToken :: Lens.Lens' DescribeAccountModificationsResponse (Prelude.Maybe Prelude.Text)
describeAccountModificationsResponse_nextToken = Lens.lens (\DescribeAccountModificationsResponse' {nextToken} -> nextToken) (\s@DescribeAccountModificationsResponse' {} a -> s {nextToken = a} :: DescribeAccountModificationsResponse)

-- | The list of modifications to the configuration of BYOL.
describeAccountModificationsResponse_accountModifications :: Lens.Lens' DescribeAccountModificationsResponse (Prelude.Maybe [AccountModification])
describeAccountModificationsResponse_accountModifications = Lens.lens (\DescribeAccountModificationsResponse' {accountModifications} -> accountModifications) (\s@DescribeAccountModificationsResponse' {} a -> s {accountModifications = a} :: DescribeAccountModificationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAccountModificationsResponse_httpStatus :: Lens.Lens' DescribeAccountModificationsResponse Prelude.Int
describeAccountModificationsResponse_httpStatus = Lens.lens (\DescribeAccountModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountModificationsResponse' {} a -> s {httpStatus = a} :: DescribeAccountModificationsResponse)

instance
  Prelude.NFData
    DescribeAccountModificationsResponse
