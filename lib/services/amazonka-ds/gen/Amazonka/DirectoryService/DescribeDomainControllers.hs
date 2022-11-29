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
-- Module      : Amazonka.DirectoryService.DescribeDomainControllers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about any domain controllers in your directory.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.DescribeDomainControllers
  ( -- * Creating a Request
    DescribeDomainControllers (..),
    newDescribeDomainControllers,

    -- * Request Lenses
    describeDomainControllers_nextToken,
    describeDomainControllers_limit,
    describeDomainControllers_domainControllerIds,
    describeDomainControllers_directoryId,

    -- * Destructuring the Response
    DescribeDomainControllersResponse (..),
    newDescribeDomainControllersResponse,

    -- * Response Lenses
    describeDomainControllersResponse_nextToken,
    describeDomainControllersResponse_domainControllers,
    describeDomainControllersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDomainControllers' smart constructor.
data DescribeDomainControllers = DescribeDomainControllers'
  { -- | The /DescribeDomainControllers.NextToken/ value from a previous call to
    -- DescribeDomainControllers. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A list of identifiers for the domain controllers whose information will
    -- be provided.
    domainControllerIds :: Prelude.Maybe [Prelude.Text],
    -- | Identifier of the directory for which to retrieve the domain controller
    -- information.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'limit', 'describeDomainControllers_limit' - The maximum number of items to return.
--
-- 'domainControllerIds', 'describeDomainControllers_domainControllerIds' - A list of identifiers for the domain controllers whose information will
-- be provided.
--
-- 'directoryId', 'describeDomainControllers_directoryId' - Identifier of the directory for which to retrieve the domain controller
-- information.
newDescribeDomainControllers ::
  -- | 'directoryId'
  Prelude.Text ->
  DescribeDomainControllers
newDescribeDomainControllers pDirectoryId_ =
  DescribeDomainControllers'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      domainControllerIds = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The /DescribeDomainControllers.NextToken/ value from a previous call to
-- DescribeDomainControllers. Pass null if this is the first call.
describeDomainControllers_nextToken :: Lens.Lens' DescribeDomainControllers (Prelude.Maybe Prelude.Text)
describeDomainControllers_nextToken = Lens.lens (\DescribeDomainControllers' {nextToken} -> nextToken) (\s@DescribeDomainControllers' {} a -> s {nextToken = a} :: DescribeDomainControllers)

-- | The maximum number of items to return.
describeDomainControllers_limit :: Lens.Lens' DescribeDomainControllers (Prelude.Maybe Prelude.Natural)
describeDomainControllers_limit = Lens.lens (\DescribeDomainControllers' {limit} -> limit) (\s@DescribeDomainControllers' {} a -> s {limit = a} :: DescribeDomainControllers)

-- | A list of identifiers for the domain controllers whose information will
-- be provided.
describeDomainControllers_domainControllerIds :: Lens.Lens' DescribeDomainControllers (Prelude.Maybe [Prelude.Text])
describeDomainControllers_domainControllerIds = Lens.lens (\DescribeDomainControllers' {domainControllerIds} -> domainControllerIds) (\s@DescribeDomainControllers' {} a -> s {domainControllerIds = a} :: DescribeDomainControllers) Prelude.. Lens.mapping Lens.coerced

-- | Identifier of the directory for which to retrieve the domain controller
-- information.
describeDomainControllers_directoryId :: Lens.Lens' DescribeDomainControllers Prelude.Text
describeDomainControllers_directoryId = Lens.lens (\DescribeDomainControllers' {directoryId} -> directoryId) (\s@DescribeDomainControllers' {} a -> s {directoryId = a} :: DescribeDomainControllers)

instance Core.AWSPager DescribeDomainControllers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDomainControllersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDomainControllersResponse_domainControllers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDomainControllers_nextToken
          Lens..~ rs
          Lens.^? describeDomainControllersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDomainControllers where
  type
    AWSResponse DescribeDomainControllers =
      DescribeDomainControllersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainControllersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DomainControllers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomainControllers where
  hashWithSalt _salt DescribeDomainControllers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` domainControllerIds
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DescribeDomainControllers where
  rnf DescribeDomainControllers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf domainControllerIds
      `Prelude.seq` Prelude.rnf directoryId

instance Core.ToHeaders DescribeDomainControllers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeDomainControllers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDomainControllers where
  toJSON DescribeDomainControllers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            ("DomainControllerIds" Core..=)
              Prelude.<$> domainControllerIds,
            Prelude.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeDomainControllers where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDomainControllers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDomainControllersResponse' smart constructor.
data DescribeDomainControllersResponse = DescribeDomainControllersResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
    -- retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of the DomainController objects that were retrieved.
    domainControllers :: Prelude.Maybe [DomainController],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainControllersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDomainControllersResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
-- retrieve the next set of items.
--
-- 'domainControllers', 'describeDomainControllersResponse_domainControllers' - List of the DomainController objects that were retrieved.
--
-- 'httpStatus', 'describeDomainControllersResponse_httpStatus' - The response's http status code.
newDescribeDomainControllersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainControllersResponse
newDescribeDomainControllersResponse pHttpStatus_ =
  DescribeDomainControllersResponse'
    { nextToken =
        Prelude.Nothing,
      domainControllers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to DescribeDomainControllers
-- retrieve the next set of items.
describeDomainControllersResponse_nextToken :: Lens.Lens' DescribeDomainControllersResponse (Prelude.Maybe Prelude.Text)
describeDomainControllersResponse_nextToken = Lens.lens (\DescribeDomainControllersResponse' {nextToken} -> nextToken) (\s@DescribeDomainControllersResponse' {} a -> s {nextToken = a} :: DescribeDomainControllersResponse)

-- | List of the DomainController objects that were retrieved.
describeDomainControllersResponse_domainControllers :: Lens.Lens' DescribeDomainControllersResponse (Prelude.Maybe [DomainController])
describeDomainControllersResponse_domainControllers = Lens.lens (\DescribeDomainControllersResponse' {domainControllers} -> domainControllers) (\s@DescribeDomainControllersResponse' {} a -> s {domainControllers = a} :: DescribeDomainControllersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDomainControllersResponse_httpStatus :: Lens.Lens' DescribeDomainControllersResponse Prelude.Int
describeDomainControllersResponse_httpStatus = Lens.lens (\DescribeDomainControllersResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainControllersResponse' {} a -> s {httpStatus = a} :: DescribeDomainControllersResponse)

instance
  Prelude.NFData
    DescribeDomainControllersResponse
  where
  rnf DescribeDomainControllersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainControllers
      `Prelude.seq` Prelude.rnf httpStatus
