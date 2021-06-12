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
-- Module      : Network.AWS.EC2.GetManagedPrefixListAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resources that are associated with the
-- specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListAssociations
  ( -- * Creating a Request
    GetManagedPrefixListAssociations (..),
    newGetManagedPrefixListAssociations,

    -- * Request Lenses
    getManagedPrefixListAssociations_nextToken,
    getManagedPrefixListAssociations_dryRun,
    getManagedPrefixListAssociations_maxResults,
    getManagedPrefixListAssociations_prefixListId,

    -- * Destructuring the Response
    GetManagedPrefixListAssociationsResponse (..),
    newGetManagedPrefixListAssociationsResponse,

    -- * Response Lenses
    getManagedPrefixListAssociationsResponse_nextToken,
    getManagedPrefixListAssociationsResponse_prefixListAssociations,
    getManagedPrefixListAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetManagedPrefixListAssociations' smart constructor.
data GetManagedPrefixListAssociations = GetManagedPrefixListAssociations'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the prefix list.
    prefixListId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetManagedPrefixListAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getManagedPrefixListAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getManagedPrefixListAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getManagedPrefixListAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'prefixListId', 'getManagedPrefixListAssociations_prefixListId' - The ID of the prefix list.
newGetManagedPrefixListAssociations ::
  -- | 'prefixListId'
  Core.Text ->
  GetManagedPrefixListAssociations
newGetManagedPrefixListAssociations pPrefixListId_ =
  GetManagedPrefixListAssociations'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      prefixListId = pPrefixListId_
    }

-- | The token for the next page of results.
getManagedPrefixListAssociations_nextToken :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Core.Text)
getManagedPrefixListAssociations_nextToken = Lens.lens (\GetManagedPrefixListAssociations' {nextToken} -> nextToken) (\s@GetManagedPrefixListAssociations' {} a -> s {nextToken = a} :: GetManagedPrefixListAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getManagedPrefixListAssociations_dryRun :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Core.Bool)
getManagedPrefixListAssociations_dryRun = Lens.lens (\GetManagedPrefixListAssociations' {dryRun} -> dryRun) (\s@GetManagedPrefixListAssociations' {} a -> s {dryRun = a} :: GetManagedPrefixListAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getManagedPrefixListAssociations_maxResults :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Core.Natural)
getManagedPrefixListAssociations_maxResults = Lens.lens (\GetManagedPrefixListAssociations' {maxResults} -> maxResults) (\s@GetManagedPrefixListAssociations' {} a -> s {maxResults = a} :: GetManagedPrefixListAssociations)

-- | The ID of the prefix list.
getManagedPrefixListAssociations_prefixListId :: Lens.Lens' GetManagedPrefixListAssociations Core.Text
getManagedPrefixListAssociations_prefixListId = Lens.lens (\GetManagedPrefixListAssociations' {prefixListId} -> prefixListId) (\s@GetManagedPrefixListAssociations' {} a -> s {prefixListId = a} :: GetManagedPrefixListAssociations)

instance
  Core.AWSPager
    GetManagedPrefixListAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListAssociationsResponse_prefixListAssociations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getManagedPrefixListAssociations_nextToken
          Lens..~ rs
          Lens.^? getManagedPrefixListAssociationsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetManagedPrefixListAssociations
  where
  type
    AWSResponse GetManagedPrefixListAssociations =
      GetManagedPrefixListAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetManagedPrefixListAssociationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "prefixListAssociationSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetManagedPrefixListAssociations

instance Core.NFData GetManagedPrefixListAssociations

instance
  Core.ToHeaders
    GetManagedPrefixListAssociations
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetManagedPrefixListAssociations where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetManagedPrefixListAssociations
  where
  toQuery GetManagedPrefixListAssociations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetManagedPrefixListAssociations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newGetManagedPrefixListAssociationsResponse' smart constructor.
data GetManagedPrefixListAssociationsResponse = GetManagedPrefixListAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the associations.
    prefixListAssociations :: Core.Maybe [PrefixListAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetManagedPrefixListAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getManagedPrefixListAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'prefixListAssociations', 'getManagedPrefixListAssociationsResponse_prefixListAssociations' - Information about the associations.
--
-- 'httpStatus', 'getManagedPrefixListAssociationsResponse_httpStatus' - The response's http status code.
newGetManagedPrefixListAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetManagedPrefixListAssociationsResponse
newGetManagedPrefixListAssociationsResponse
  pHttpStatus_ =
    GetManagedPrefixListAssociationsResponse'
      { nextToken =
          Core.Nothing,
        prefixListAssociations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getManagedPrefixListAssociationsResponse_nextToken :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Core.Maybe Core.Text)
getManagedPrefixListAssociationsResponse_nextToken = Lens.lens (\GetManagedPrefixListAssociationsResponse' {nextToken} -> nextToken) (\s@GetManagedPrefixListAssociationsResponse' {} a -> s {nextToken = a} :: GetManagedPrefixListAssociationsResponse)

-- | Information about the associations.
getManagedPrefixListAssociationsResponse_prefixListAssociations :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Core.Maybe [PrefixListAssociation])
getManagedPrefixListAssociationsResponse_prefixListAssociations = Lens.lens (\GetManagedPrefixListAssociationsResponse' {prefixListAssociations} -> prefixListAssociations) (\s@GetManagedPrefixListAssociationsResponse' {} a -> s {prefixListAssociations = a} :: GetManagedPrefixListAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getManagedPrefixListAssociationsResponse_httpStatus :: Lens.Lens' GetManagedPrefixListAssociationsResponse Core.Int
getManagedPrefixListAssociationsResponse_httpStatus = Lens.lens (\GetManagedPrefixListAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetManagedPrefixListAssociationsResponse' {} a -> s {httpStatus = a} :: GetManagedPrefixListAssociationsResponse)

instance
  Core.NFData
    GetManagedPrefixListAssociationsResponse
