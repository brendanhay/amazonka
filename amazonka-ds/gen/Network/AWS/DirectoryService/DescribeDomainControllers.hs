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
-- Module      : Network.AWS.DirectoryService.DescribeDomainControllers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about any domain controllers in your directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeDomainControllers
  ( -- * Creating a Request
    DescribeDomainControllers (..),
    newDescribeDomainControllers,

    -- * Request Lenses
    describeDomainControllers_nextToken,
    describeDomainControllers_domainControllerIds,
    describeDomainControllers_limit,
    describeDomainControllers_directoryId,

    -- * Destructuring the Response
    DescribeDomainControllersResponse (..),
    newDescribeDomainControllersResponse,

    -- * Response Lenses
    describeDomainControllersResponse_domainControllers,
    describeDomainControllersResponse_nextToken,
    describeDomainControllersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDomainControllers' smart constructor.
data DescribeDomainControllers = DescribeDomainControllers'
  { -- | The /DescribeDomainControllers.NextToken/ value from a previous call to
    -- DescribeDomainControllers. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of identifiers for the domain controllers whose information will
    -- be provided.
    domainControllerIds :: Core.Maybe [Core.Text],
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | Identifier of the directory for which to retrieve the domain controller
    -- information.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDomainControllers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDomainControllers_nextToken' - The /DescribeDomainControllers.NextToken/ value from a previous call to
-- DescribeDomainControllers. Pass null if this is the first call.
--
-- 'domainControllerIds', 'describeDomainControllers_domainControllerIds' - A list of identifiers for the domain controllers whose information will
-- be provided.
--
-- 'limit', 'describeDomainControllers_limit' - The maximum number of items to return.
--
-- 'directoryId', 'describeDomainControllers_directoryId' - Identifier of the directory for which to retrieve the domain controller
-- information.
newDescribeDomainControllers ::
  -- | 'directoryId'
  Core.Text ->
  DescribeDomainControllers
newDescribeDomainControllers pDirectoryId_ =
  DescribeDomainControllers'
    { nextToken =
        Core.Nothing,
      domainControllerIds = Core.Nothing,
      limit = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The /DescribeDomainControllers.NextToken/ value from a previous call to
-- DescribeDomainControllers. Pass null if this is the first call.
describeDomainControllers_nextToken :: Lens.Lens' DescribeDomainControllers (Core.Maybe Core.Text)
describeDomainControllers_nextToken = Lens.lens (\DescribeDomainControllers' {nextToken} -> nextToken) (\s@DescribeDomainControllers' {} a -> s {nextToken = a} :: DescribeDomainControllers)

-- | A list of identifiers for the domain controllers whose information will
-- be provided.
describeDomainControllers_domainControllerIds :: Lens.Lens' DescribeDomainControllers (Core.Maybe [Core.Text])
describeDomainControllers_domainControllerIds = Lens.lens (\DescribeDomainControllers' {domainControllerIds} -> domainControllerIds) (\s@DescribeDomainControllers' {} a -> s {domainControllerIds = a} :: DescribeDomainControllers) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return.
describeDomainControllers_limit :: Lens.Lens' DescribeDomainControllers (Core.Maybe Core.Natural)
describeDomainControllers_limit = Lens.lens (\DescribeDomainControllers' {limit} -> limit) (\s@DescribeDomainControllers' {} a -> s {limit = a} :: DescribeDomainControllers)

-- | Identifier of the directory for which to retrieve the domain controller
-- information.
describeDomainControllers_directoryId :: Lens.Lens' DescribeDomainControllers Core.Text
describeDomainControllers_directoryId = Lens.lens (\DescribeDomainControllers' {directoryId} -> directoryId) (\s@DescribeDomainControllers' {} a -> s {directoryId = a} :: DescribeDomainControllers)

instance Core.AWSPager DescribeDomainControllers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDomainControllersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDomainControllersResponse_domainControllers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDomainControllers_nextToken
          Lens..~ rs
          Lens.^? describeDomainControllersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeDomainControllers where
  type
    AWSResponse DescribeDomainControllers =
      DescribeDomainControllersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainControllersResponse'
            Core.<$> (x Core..?> "DomainControllers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDomainControllers

instance Core.NFData DescribeDomainControllers

instance Core.ToHeaders DescribeDomainControllers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeDomainControllers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDomainControllers where
  toJSON DescribeDomainControllers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("DomainControllerIds" Core..=)
              Core.<$> domainControllerIds,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeDomainControllers where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDomainControllers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDomainControllersResponse' smart constructor.
data DescribeDomainControllersResponse = DescribeDomainControllersResponse'
  { -- | List of the DomainController objects that were retrieved.
    domainControllers :: Core.Maybe [DomainController],
    -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
    -- retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDomainControllersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainControllers', 'describeDomainControllersResponse_domainControllers' - List of the DomainController objects that were retrieved.
--
-- 'nextToken', 'describeDomainControllersResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
-- retrieve the next set of items.
--
-- 'httpStatus', 'describeDomainControllersResponse_httpStatus' - The response's http status code.
newDescribeDomainControllersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDomainControllersResponse
newDescribeDomainControllersResponse pHttpStatus_ =
  DescribeDomainControllersResponse'
    { domainControllers =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of the DomainController objects that were retrieved.
describeDomainControllersResponse_domainControllers :: Lens.Lens' DescribeDomainControllersResponse (Core.Maybe [DomainController])
describeDomainControllersResponse_domainControllers = Lens.lens (\DescribeDomainControllersResponse' {domainControllers} -> domainControllers) (\s@DescribeDomainControllersResponse' {} a -> s {domainControllers = a} :: DescribeDomainControllersResponse) Core.. Lens.mapping Lens._Coerce

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
-- retrieve the next set of items.
describeDomainControllersResponse_nextToken :: Lens.Lens' DescribeDomainControllersResponse (Core.Maybe Core.Text)
describeDomainControllersResponse_nextToken = Lens.lens (\DescribeDomainControllersResponse' {nextToken} -> nextToken) (\s@DescribeDomainControllersResponse' {} a -> s {nextToken = a} :: DescribeDomainControllersResponse)

-- | The response's http status code.
describeDomainControllersResponse_httpStatus :: Lens.Lens' DescribeDomainControllersResponse Core.Int
describeDomainControllersResponse_httpStatus = Lens.lens (\DescribeDomainControllersResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainControllersResponse' {} a -> s {httpStatus = a} :: DescribeDomainControllersResponse)

instance
  Core.NFData
    DescribeDomainControllersResponse
