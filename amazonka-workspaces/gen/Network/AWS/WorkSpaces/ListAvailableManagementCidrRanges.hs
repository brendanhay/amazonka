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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newListAvailableManagementCidrRanges' smart constructor.
data ListAvailableManagementCidrRanges = ListAvailableManagementCidrRanges'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IP address range to search. Specify an IP address range that is
    -- compatible with your network and in CIDR notation (that is, specify the
    -- range as an IPv4 CIDR block).
    managementCidrRangeConstraint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListAvailableManagementCidrRanges
newListAvailableManagementCidrRanges
  pManagementCidrRangeConstraint_ =
    ListAvailableManagementCidrRanges'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        managementCidrRangeConstraint =
          pManagementCidrRangeConstraint_
      }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
listAvailableManagementCidrRanges_nextToken :: Lens.Lens' ListAvailableManagementCidrRanges (Prelude.Maybe Prelude.Text)
listAvailableManagementCidrRanges_nextToken = Lens.lens (\ListAvailableManagementCidrRanges' {nextToken} -> nextToken) (\s@ListAvailableManagementCidrRanges' {} a -> s {nextToken = a} :: ListAvailableManagementCidrRanges)

-- | The maximum number of items to return.
listAvailableManagementCidrRanges_maxResults :: Lens.Lens' ListAvailableManagementCidrRanges (Prelude.Maybe Prelude.Natural)
listAvailableManagementCidrRanges_maxResults = Lens.lens (\ListAvailableManagementCidrRanges' {maxResults} -> maxResults) (\s@ListAvailableManagementCidrRanges' {} a -> s {maxResults = a} :: ListAvailableManagementCidrRanges)

-- | The IP address range to search. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block).
listAvailableManagementCidrRanges_managementCidrRangeConstraint :: Lens.Lens' ListAvailableManagementCidrRanges Prelude.Text
listAvailableManagementCidrRanges_managementCidrRangeConstraint = Lens.lens (\ListAvailableManagementCidrRanges' {managementCidrRangeConstraint} -> managementCidrRangeConstraint) (\s@ListAvailableManagementCidrRanges' {} a -> s {managementCidrRangeConstraint = a} :: ListAvailableManagementCidrRanges)

instance
  Core.AWSPager
    ListAvailableManagementCidrRanges
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAvailableManagementCidrRangesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAvailableManagementCidrRangesResponse_managementCidrRanges
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAvailableManagementCidrRanges_nextToken
          Lens..~ rs
          Lens.^? listAvailableManagementCidrRangesResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "ManagementCidrRanges"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailableManagementCidrRanges

instance
  Prelude.NFData
    ListAvailableManagementCidrRanges

instance
  Core.ToHeaders
    ListAvailableManagementCidrRanges
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ListAvailableManagementCidrRanges" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListAvailableManagementCidrRanges
  where
  toJSON ListAvailableManagementCidrRanges' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "ManagementCidrRangeConstraint"
                  Core..= managementCidrRangeConstraint
              )
          ]
      )

instance
  Core.ToPath
    ListAvailableManagementCidrRanges
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListAvailableManagementCidrRanges
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableManagementCidrRangesResponse' smart constructor.
data ListAvailableManagementCidrRangesResponse = ListAvailableManagementCidrRangesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
    managementCidrRanges :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAvailableManagementCidrRangesResponse
newListAvailableManagementCidrRangesResponse
  pHttpStatus_ =
    ListAvailableManagementCidrRangesResponse'
      { nextToken =
          Prelude.Nothing,
        managementCidrRanges =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
listAvailableManagementCidrRangesResponse_nextToken :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Prelude.Maybe Prelude.Text)
listAvailableManagementCidrRangesResponse_nextToken = Lens.lens (\ListAvailableManagementCidrRangesResponse' {nextToken} -> nextToken) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {nextToken = a} :: ListAvailableManagementCidrRangesResponse)

-- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
listAvailableManagementCidrRangesResponse_managementCidrRanges :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Prelude.Maybe [Prelude.Text])
listAvailableManagementCidrRangesResponse_managementCidrRanges = Lens.lens (\ListAvailableManagementCidrRangesResponse' {managementCidrRanges} -> managementCidrRanges) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {managementCidrRanges = a} :: ListAvailableManagementCidrRangesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAvailableManagementCidrRangesResponse_httpStatus :: Lens.Lens' ListAvailableManagementCidrRangesResponse Prelude.Int
listAvailableManagementCidrRangesResponse_httpStatus = Lens.lens (\ListAvailableManagementCidrRangesResponse' {httpStatus} -> httpStatus) (\s@ListAvailableManagementCidrRangesResponse' {} a -> s {httpStatus = a} :: ListAvailableManagementCidrRangesResponse)

instance
  Prelude.NFData
    ListAvailableManagementCidrRangesResponse
