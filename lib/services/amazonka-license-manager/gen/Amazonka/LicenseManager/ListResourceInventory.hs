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
-- Module      : Amazonka.LicenseManager.ListResourceInventory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources managed using Systems Manager inventory.
--
-- This operation returns paginated results.
module Amazonka.LicenseManager.ListResourceInventory
  ( -- * Creating a Request
    ListResourceInventory (..),
    newListResourceInventory,

    -- * Request Lenses
    listResourceInventory_nextToken,
    listResourceInventory_filters,
    listResourceInventory_maxResults,

    -- * Destructuring the Response
    ListResourceInventoryResponse (..),
    newListResourceInventoryResponse,

    -- * Response Lenses
    listResourceInventoryResponse_nextToken,
    listResourceInventoryResponse_resourceInventoryList,
    listResourceInventoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceInventory' smart constructor.
data ListResourceInventory = ListResourceInventory'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filters and logical
    -- operators are supported:
    --
    -- -   @account_id@ - The ID of the Amazon Web Services account that owns
    --     the resource. Logical operators are @EQUALS@ | @NOT_EQUALS@.
    --
    -- -   @application_name@ - The name of the application. Logical operators
    --     are @EQUALS@ | @BEGINS_WITH@.
    --
    -- -   @license_included@ - The type of license included. Logical operators
    --     are @EQUALS@ | @NOT_EQUALS@. Possible values are
    --     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
    --     @windows-server-datacenter@.
    --
    -- -   @platform@ - The platform of the resource. Logical operators are
    --     @EQUALS@ | @BEGINS_WITH@.
    --
    -- -   @resource_id@ - The ID of the resource. Logical operators are
    --     @EQUALS@ | @NOT_EQUALS@.
    --
    -- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
    --     resource. Logical operators are @EQUALS@ (single account) or
    --     @EQUALS@ | @NOT_EQUALS@ (cross account).
    filters :: Prelude.Maybe [InventoryFilter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceInventory_nextToken' - Token for the next set of results.
--
-- 'filters', 'listResourceInventory_filters' - Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @account_id@ - The ID of the Amazon Web Services account that owns
--     the resource. Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @application_name@ - The name of the application. Logical operators
--     are @EQUALS@ | @BEGINS_WITH@.
--
-- -   @license_included@ - The type of license included. Logical operators
--     are @EQUALS@ | @NOT_EQUALS@. Possible values are
--     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
--     @windows-server-datacenter@.
--
-- -   @platform@ - The platform of the resource. Logical operators are
--     @EQUALS@ | @BEGINS_WITH@.
--
-- -   @resource_id@ - The ID of the resource. Logical operators are
--     @EQUALS@ | @NOT_EQUALS@.
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Logical operators are @EQUALS@ (single account) or
--     @EQUALS@ | @NOT_EQUALS@ (cross account).
--
-- 'maxResults', 'listResourceInventory_maxResults' - Maximum number of results to return in a single call.
newListResourceInventory ::
  ListResourceInventory
newListResourceInventory =
  ListResourceInventory'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listResourceInventory_nextToken :: Lens.Lens' ListResourceInventory (Prelude.Maybe Prelude.Text)
listResourceInventory_nextToken = Lens.lens (\ListResourceInventory' {nextToken} -> nextToken) (\s@ListResourceInventory' {} a -> s {nextToken = a} :: ListResourceInventory)

-- | Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @account_id@ - The ID of the Amazon Web Services account that owns
--     the resource. Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @application_name@ - The name of the application. Logical operators
--     are @EQUALS@ | @BEGINS_WITH@.
--
-- -   @license_included@ - The type of license included. Logical operators
--     are @EQUALS@ | @NOT_EQUALS@. Possible values are
--     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
--     @windows-server-datacenter@.
--
-- -   @platform@ - The platform of the resource. Logical operators are
--     @EQUALS@ | @BEGINS_WITH@.
--
-- -   @resource_id@ - The ID of the resource. Logical operators are
--     @EQUALS@ | @NOT_EQUALS@.
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Logical operators are @EQUALS@ (single account) or
--     @EQUALS@ | @NOT_EQUALS@ (cross account).
listResourceInventory_filters :: Lens.Lens' ListResourceInventory (Prelude.Maybe [InventoryFilter])
listResourceInventory_filters = Lens.lens (\ListResourceInventory' {filters} -> filters) (\s@ListResourceInventory' {} a -> s {filters = a} :: ListResourceInventory) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listResourceInventory_maxResults :: Lens.Lens' ListResourceInventory (Prelude.Maybe Prelude.Int)
listResourceInventory_maxResults = Lens.lens (\ListResourceInventory' {maxResults} -> maxResults) (\s@ListResourceInventory' {} a -> s {maxResults = a} :: ListResourceInventory)

instance Core.AWSPager ListResourceInventory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceInventoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceInventoryResponse_resourceInventoryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceInventory_nextToken
          Lens..~ rs
          Lens.^? listResourceInventoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceInventory where
  type
    AWSResponse ListResourceInventory =
      ListResourceInventoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceInventoryResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ResourceInventoryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceInventory where
  hashWithSalt _salt ListResourceInventory' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListResourceInventory where
  rnf ListResourceInventory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListResourceInventory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.ListResourceInventory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceInventory where
  toJSON ListResourceInventory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResourceInventory where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourceInventory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceInventoryResponse' smart constructor.
data ListResourceInventoryResponse = ListResourceInventoryResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the resources.
    resourceInventoryList :: Prelude.Maybe [ResourceInventory],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceInventoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceInventoryResponse_nextToken' - Token for the next set of results.
--
-- 'resourceInventoryList', 'listResourceInventoryResponse_resourceInventoryList' - Information about the resources.
--
-- 'httpStatus', 'listResourceInventoryResponse_httpStatus' - The response's http status code.
newListResourceInventoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceInventoryResponse
newListResourceInventoryResponse pHttpStatus_ =
  ListResourceInventoryResponse'
    { nextToken =
        Prelude.Nothing,
      resourceInventoryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listResourceInventoryResponse_nextToken :: Lens.Lens' ListResourceInventoryResponse (Prelude.Maybe Prelude.Text)
listResourceInventoryResponse_nextToken = Lens.lens (\ListResourceInventoryResponse' {nextToken} -> nextToken) (\s@ListResourceInventoryResponse' {} a -> s {nextToken = a} :: ListResourceInventoryResponse)

-- | Information about the resources.
listResourceInventoryResponse_resourceInventoryList :: Lens.Lens' ListResourceInventoryResponse (Prelude.Maybe [ResourceInventory])
listResourceInventoryResponse_resourceInventoryList = Lens.lens (\ListResourceInventoryResponse' {resourceInventoryList} -> resourceInventoryList) (\s@ListResourceInventoryResponse' {} a -> s {resourceInventoryList = a} :: ListResourceInventoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceInventoryResponse_httpStatus :: Lens.Lens' ListResourceInventoryResponse Prelude.Int
listResourceInventoryResponse_httpStatus = Lens.lens (\ListResourceInventoryResponse' {httpStatus} -> httpStatus) (\s@ListResourceInventoryResponse' {} a -> s {httpStatus = a} :: ListResourceInventoryResponse)

instance Prelude.NFData ListResourceInventoryResponse where
  rnf ListResourceInventoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceInventoryList
      `Prelude.seq` Prelude.rnf httpStatus
