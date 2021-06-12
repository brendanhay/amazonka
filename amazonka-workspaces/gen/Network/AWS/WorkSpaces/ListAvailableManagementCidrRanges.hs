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
-- Module      : Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of IP address ranges, specified as IPv4 CIDR blocks,
-- that you can use for the network management interface when you enable
-- Bring Your Own License (BYOL).
--
-- This operation can be run only by AWS accounts that are enabled for
-- BYOL. If your account isn\'t enabled for BYOL, you\'ll receive an
-- @AccessDeniedException@ error.
--
-- The management network interface is connected to a secure Amazon
-- WorkSpaces management network. It is used for interactive streaming of
-- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
-- WorkSpaces to manage the WorkSpace.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
  ( -- * Creating a Request
    ListAvailableManagementCidrRanges (..),
    newListAvailableManagementCidrRanges,

    -- * Request Lenses
    listAvailableManagementCidrRanges_nextToken,
    listAvailableManagementCidrRanges_maxResults,
    listAvailableManagementCidrRanges_managementCidrRangeConstraint,

    -- * Destructuring the Response
    ListAvailableManagementCidrRangesResponse (..),
    newListAvailableManagementCidrRangesResponse,

    -- * Response Lenses
    listAvailableManagementCidrRangesResponse_nextToken,
    listAvailableManagementCidrRangesResponse_managementCidrRanges,
    listAvailableManagementCidrRangesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newListAvailableManagementCidrRanges' smart constructor.
data ListAvailableManagementCidrRanges = ListAvailableManagementCidrRanges'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The IP address range to search. Specify an IP address range that is
    -- compatible with your network and in CIDR notation (that is, specify the
    -- range as an IPv4 CIDR block).
    managementCidrRangeConstraint :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAvailableManagementCidrRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAvailableManagementCidrRanges_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'maxResults', 'listAvailableManagementCidrRanges_maxResults' - The maximum number of items to return.
--
-- 'managementCidrRangeConstraint', 'listAvailableManagementCidrRanges_managementCidrRangeConstraint' - The IP address range to search. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block).
newListAvailableManagementCidrRanges ::
  -- | 'managementCidrRangeConstraint'
  Core.Text ->
  ListAvailableManagementCidrRanges
newListAvailableManagementCidrRanges
  pManagementCidrRangeConstraint_ =
    ListAvailableManagementCidrRanges'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        managementCidrRangeConstraint =
          pManagementCidrRangeConstraint_
      }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
listAvailableManagementCidrRanges_nextToken :: Lens.Lens' ListAvailableManagementCidrRanges (Core.Maybe Core.Text)
listAvailableManagementCidrRanges_nextToken = Lens.lens (\ListAvailableManagementCidrRanges' {nextToken} -> nextToken) (\s@ListAvailableManagementCidrRanges' {} a -> s {nextToken = a} :: ListAvailableManagementCidrRanges)

-- | The maximum number of items to return.
listAvailableManagementCidrRanges_maxResults :: Lens.Lens' ListAvailableManagementCidrRanges (Core.Maybe Core.Natural)
listAvailableManagementCidrRanges_maxResults = Lens.lens (\ListAvailableManagementCidrRanges' {maxResults} -> maxResults) (\s@ListAvailableManagementCidrRanges' {} a -> s {maxResults = a} :: ListAvailableManagementCidrRanges)

-- | The IP address range to search. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block).
listAvailableManagementCidrRanges_managementCidrRangeConstraint :: Lens.Lens' ListAvailableManagementCidrRanges Core.Text
listAvailableManagementCidrRanges_managementCidrRangeConstraint = Lens.lens (\ListAvailableManagementCidrRanges' {managementCidrRangeConstraint} -> managementCidrRangeConstraint) (\s@ListAvailableManagementCidrRanges' {} a -> s {managementCidrRangeConstraint = a} :: ListAvailableManagementCidrRanges)

instance
  Core.AWSPager
    ListAvailableManagementCidrRanges
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAvailableManagementCidrRangesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAvailableManagementCidrRangesResponse_managementCidrRanges
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAvailableManagementCidrRanges_nextToken
          Lens..~ rs
          Lens.^? listAvailableManagementCidrRangesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListAvailableManagementCidrRanges
  where
  type
    AWSResponse ListAvailableManagementCidrRanges =
      ListAvailableManagementCidrRangesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableManagementCidrRangesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ManagementCidrRanges"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListAvailableManagementCidrRanges

instance
  Core.NFData
    ListAvailableManagementCidrRanges

instance
  Core.ToHeaders
    ListAvailableManagementCidrRanges
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ListAvailableManagementCidrRanges" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListAvailableManagementCidrRanges
  where
  toJSON ListAvailableManagementCidrRanges' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ( "ManagementCidrRangeConstraint"
                  Core..= managementCidrRangeConstraint
              )
          ]
      )

instance
  Core.ToPath
    ListAvailableManagementCidrRanges
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListAvailableManagementCidrRanges
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAvailableManagementCidrRangesResponse' smart constructor.
data ListAvailableManagementCidrRangesResponse = ListAvailableManagementCidrRangesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
    managementCidrRanges :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAvailableManagementCidrRangesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAvailableManagementCidrRangesResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'managementCidrRanges', 'listAvailableManagementCidrRangesResponse_managementCidrRanges' - The list of available IP address ranges, specified as IPv4 CIDR blocks.
--
-- 'httpStatus', 'listAvailableManagementCidrRangesResponse_httpStatus' - The response's http status code.
newListAvailableManagementCidrRangesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAvailableManagementCidrRangesResponse
newListAvailableManagementCidrRangesResponse
  pHttpStatus_ =
    ListAvailableManagementCidrRangesResponse'
      { nextToken =
          Core.Nothing,
        managementCidrRanges =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
listAvailableManagementCidrRangesResponse_nextToken :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Core.Maybe Core.Text)
listAvailableManagementCidrRangesResponse_nextToken = Lens.lens (\ListAvailableManagementCidrRangesResponse' {nextToken} -> nextToken) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {nextToken = a} :: ListAvailableManagementCidrRangesResponse)

-- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
listAvailableManagementCidrRangesResponse_managementCidrRanges :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Core.Maybe [Core.Text])
listAvailableManagementCidrRangesResponse_managementCidrRanges = Lens.lens (\ListAvailableManagementCidrRangesResponse' {managementCidrRanges} -> managementCidrRanges) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {managementCidrRanges = a} :: ListAvailableManagementCidrRangesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAvailableManagementCidrRangesResponse_httpStatus :: Lens.Lens' ListAvailableManagementCidrRangesResponse Core.Int
listAvailableManagementCidrRangesResponse_httpStatus = Lens.lens (\ListAvailableManagementCidrRangesResponse' {httpStatus} -> httpStatus) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {httpStatus = a} :: ListAvailableManagementCidrRangesResponse)

instance
  Core.NFData
    ListAvailableManagementCidrRangesResponse
