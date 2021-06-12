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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeAccountModifications' smart constructor.
data DescribeAccountModifications = DescribeAccountModifications'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeAccountModifications_nextToken :: Lens.Lens' DescribeAccountModifications (Core.Maybe Core.Text)
describeAccountModifications_nextToken = Lens.lens (\DescribeAccountModifications' {nextToken} -> nextToken) (\s@DescribeAccountModifications' {} a -> s {nextToken = a} :: DescribeAccountModifications)

instance Core.AWSPager DescribeAccountModifications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAccountModificationsResponse_accountModifications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAccountModifications_nextToken
          Lens..~ rs
          Lens.^? describeAccountModificationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAccountModifications where
  type
    AWSResponse DescribeAccountModifications =
      DescribeAccountModificationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountModificationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "AccountModifications"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAccountModifications

instance Core.NFData DescribeAccountModifications

instance Core.ToHeaders DescribeAccountModifications where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeAccountModifications" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAccountModifications where
  toJSON DescribeAccountModifications' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath DescribeAccountModifications where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAccountModifications where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAccountModificationsResponse' smart constructor.
data DescribeAccountModificationsResponse = DescribeAccountModificationsResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of modifications to the configuration of BYOL.
    accountModifications :: Core.Maybe [AccountModification],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAccountModificationsResponse
newDescribeAccountModificationsResponse pHttpStatus_ =
  DescribeAccountModificationsResponse'
    { nextToken =
        Core.Nothing,
      accountModifications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeAccountModificationsResponse_nextToken :: Lens.Lens' DescribeAccountModificationsResponse (Core.Maybe Core.Text)
describeAccountModificationsResponse_nextToken = Lens.lens (\DescribeAccountModificationsResponse' {nextToken} -> nextToken) (\s@DescribeAccountModificationsResponse' {} a -> s {nextToken = a} :: DescribeAccountModificationsResponse)

-- | The list of modifications to the configuration of BYOL.
describeAccountModificationsResponse_accountModifications :: Lens.Lens' DescribeAccountModificationsResponse (Core.Maybe [AccountModification])
describeAccountModificationsResponse_accountModifications = Lens.lens (\DescribeAccountModificationsResponse' {accountModifications} -> accountModifications) (\s@DescribeAccountModificationsResponse' {} a -> s {accountModifications = a} :: DescribeAccountModificationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAccountModificationsResponse_httpStatus :: Lens.Lens' DescribeAccountModificationsResponse Core.Int
describeAccountModificationsResponse_httpStatus = Lens.lens (\DescribeAccountModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountModificationsResponse' {} a -> s {httpStatus = a} :: DescribeAccountModificationsResponse)

instance
  Core.NFData
    DescribeAccountModificationsResponse
