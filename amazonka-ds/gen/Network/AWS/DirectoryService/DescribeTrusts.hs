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
-- Module      : Network.AWS.DirectoryService.DescribeTrusts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the trust relationships for this account.
--
-- If no input parameters are provided, such as DirectoryId or TrustIds,
-- this request describes all the trust relationships belonging to the
-- account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeTrusts
  ( -- * Creating a Request
    DescribeTrusts (..),
    newDescribeTrusts,

    -- * Request Lenses
    describeTrusts_nextToken,
    describeTrusts_directoryId,
    describeTrusts_limit,
    describeTrusts_trustIds,

    -- * Destructuring the Response
    DescribeTrustsResponse (..),
    newDescribeTrustsResponse,

    -- * Response Lenses
    describeTrustsResponse_nextToken,
    describeTrustsResponse_trusts,
    describeTrustsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes the trust relationships for a particular AWS Managed Microsoft
-- AD directory. If no input parameters are are provided, such as directory
-- ID or trust ID, this request describes all the trust relationships.
--
-- /See:/ 'newDescribeTrusts' smart constructor.
data DescribeTrusts = DescribeTrusts'
  { -- | The /DescribeTrustsResult.NextToken/ value from a previous call to
    -- DescribeTrusts. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The Directory ID of the AWS directory that is a part of the requested
    -- trust relationship.
    directoryId :: Core.Maybe Core.Text,
    -- | The maximum number of objects to return.
    limit :: Core.Maybe Core.Natural,
    -- | A list of identifiers of the trust relationships for which to obtain the
    -- information. If this member is null, all trust relationships that belong
    -- to the current account are returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    trustIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrusts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrusts_nextToken' - The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
--
-- 'directoryId', 'describeTrusts_directoryId' - The Directory ID of the AWS directory that is a part of the requested
-- trust relationship.
--
-- 'limit', 'describeTrusts_limit' - The maximum number of objects to return.
--
-- 'trustIds', 'describeTrusts_trustIds' - A list of identifiers of the trust relationships for which to obtain the
-- information. If this member is null, all trust relationships that belong
-- to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
newDescribeTrusts ::
  DescribeTrusts
newDescribeTrusts =
  DescribeTrusts'
    { nextToken = Core.Nothing,
      directoryId = Core.Nothing,
      limit = Core.Nothing,
      trustIds = Core.Nothing
    }

-- | The /DescribeTrustsResult.NextToken/ value from a previous call to
-- DescribeTrusts. Pass null if this is the first call.
describeTrusts_nextToken :: Lens.Lens' DescribeTrusts (Core.Maybe Core.Text)
describeTrusts_nextToken = Lens.lens (\DescribeTrusts' {nextToken} -> nextToken) (\s@DescribeTrusts' {} a -> s {nextToken = a} :: DescribeTrusts)

-- | The Directory ID of the AWS directory that is a part of the requested
-- trust relationship.
describeTrusts_directoryId :: Lens.Lens' DescribeTrusts (Core.Maybe Core.Text)
describeTrusts_directoryId = Lens.lens (\DescribeTrusts' {directoryId} -> directoryId) (\s@DescribeTrusts' {} a -> s {directoryId = a} :: DescribeTrusts)

-- | The maximum number of objects to return.
describeTrusts_limit :: Lens.Lens' DescribeTrusts (Core.Maybe Core.Natural)
describeTrusts_limit = Lens.lens (\DescribeTrusts' {limit} -> limit) (\s@DescribeTrusts' {} a -> s {limit = a} :: DescribeTrusts)

-- | A list of identifiers of the trust relationships for which to obtain the
-- information. If this member is null, all trust relationships that belong
-- to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeTrusts_trustIds :: Lens.Lens' DescribeTrusts (Core.Maybe [Core.Text])
describeTrusts_trustIds = Lens.lens (\DescribeTrusts' {trustIds} -> trustIds) (\s@DescribeTrusts' {} a -> s {trustIds = a} :: DescribeTrusts) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeTrusts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrustsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrustsResponse_trusts Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTrusts_nextToken
          Lens..~ rs
          Lens.^? describeTrustsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeTrusts where
  type
    AWSResponse DescribeTrusts =
      DescribeTrustsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Trusts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTrusts

instance Core.NFData DescribeTrusts

instance Core.ToHeaders DescribeTrusts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeTrusts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTrusts where
  toJSON DescribeTrusts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("DirectoryId" Core..=) Core.<$> directoryId,
            ("Limit" Core..=) Core.<$> limit,
            ("TrustIds" Core..=) Core.<$> trustIds
          ]
      )

instance Core.ToPath DescribeTrusts where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrusts where
  toQuery = Core.const Core.mempty

-- | The result of a DescribeTrust request.
--
-- /See:/ 'newDescribeTrustsResponse' smart constructor.
data DescribeTrustsResponse = DescribeTrustsResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
    -- the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of Trust objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the /Limit/ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
    trusts :: Core.Maybe [Trust],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrustsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrustsResponse_nextToken' - If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
-- the next set of items.
--
-- 'trusts', 'describeTrustsResponse_trusts' - The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
--
-- 'httpStatus', 'describeTrustsResponse_httpStatus' - The response's http status code.
newDescribeTrustsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrustsResponse
newDescribeTrustsResponse pHttpStatus_ =
  DescribeTrustsResponse'
    { nextToken = Core.Nothing,
      trusts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeTrusts to retrieve
-- the next set of items.
describeTrustsResponse_nextToken :: Lens.Lens' DescribeTrustsResponse (Core.Maybe Core.Text)
describeTrustsResponse_nextToken = Lens.lens (\DescribeTrustsResponse' {nextToken} -> nextToken) (\s@DescribeTrustsResponse' {} a -> s {nextToken = a} :: DescribeTrustsResponse)

-- | The list of Trust objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
describeTrustsResponse_trusts :: Lens.Lens' DescribeTrustsResponse (Core.Maybe [Trust])
describeTrustsResponse_trusts = Lens.lens (\DescribeTrustsResponse' {trusts} -> trusts) (\s@DescribeTrustsResponse' {} a -> s {trusts = a} :: DescribeTrustsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTrustsResponse_httpStatus :: Lens.Lens' DescribeTrustsResponse Core.Int
describeTrustsResponse_httpStatus = Lens.lens (\DescribeTrustsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrustsResponse' {} a -> s {httpStatus = a} :: DescribeTrustsResponse)

instance Core.NFData DescribeTrustsResponse
