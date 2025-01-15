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
-- Module      : Amazonka.FMS.ListDiscoveredResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources in the organization\'s accounts that are
-- available to be associated with a resource set.
module Amazonka.FMS.ListDiscoveredResources
  ( -- * Creating a Request
    ListDiscoveredResources (..),
    newListDiscoveredResources,

    -- * Request Lenses
    listDiscoveredResources_maxResults,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_memberAccountIds,
    listDiscoveredResources_resourceType,

    -- * Destructuring the Response
    ListDiscoveredResourcesResponse (..),
    newListDiscoveredResourcesResponse,

    -- * Response Lenses
    listDiscoveredResourcesResponse_items,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account IDs to discover resources in. Only one
    -- account is supported per request. The account must be a member of your
    -- organization.
    memberAccountIds :: [Prelude.Text],
    -- | The type of resources to discover.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDiscoveredResources_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listDiscoveredResources_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'memberAccountIds', 'listDiscoveredResources_memberAccountIds' - The Amazon Web Services account IDs to discover resources in. Only one
-- account is supported per request. The account must be a member of your
-- organization.
--
-- 'resourceType', 'listDiscoveredResources_resourceType' - The type of resources to discover.
newListDiscoveredResources ::
  -- | 'resourceType'
  Prelude.Text ->
  ListDiscoveredResources
newListDiscoveredResources pResourceType_ =
  ListDiscoveredResources'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      memberAccountIds = Prelude.mempty,
      resourceType = pResourceType_
    }

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listDiscoveredResources_maxResults :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Natural)
listDiscoveredResources_maxResults = Lens.lens (\ListDiscoveredResources' {maxResults} -> maxResults) (\s@ListDiscoveredResources' {} a -> s {maxResults = a} :: ListDiscoveredResources)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The Amazon Web Services account IDs to discover resources in. Only one
-- account is supported per request. The account must be a member of your
-- organization.
listDiscoveredResources_memberAccountIds :: Lens.Lens' ListDiscoveredResources [Prelude.Text]
listDiscoveredResources_memberAccountIds = Lens.lens (\ListDiscoveredResources' {memberAccountIds} -> memberAccountIds) (\s@ListDiscoveredResources' {} a -> s {memberAccountIds = a} :: ListDiscoveredResources) Prelude.. Lens.coerced

-- | The type of resources to discover.
listDiscoveredResources_resourceType :: Lens.Lens' ListDiscoveredResources Prelude.Text
listDiscoveredResources_resourceType = Lens.lens (\ListDiscoveredResources' {resourceType} -> resourceType) (\s@ListDiscoveredResources' {} a -> s {resourceType = a} :: ListDiscoveredResources)

instance Core.AWSRequest ListDiscoveredResources where
  type
    AWSResponse ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoveredResources where
  hashWithSalt _salt ListDiscoveredResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` memberAccountIds
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListDiscoveredResources where
  rnf ListDiscoveredResources' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf memberAccountIds `Prelude.seq`
          Prelude.rnf resourceType

instance Data.ToHeaders ListDiscoveredResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListDiscoveredResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("MemberAccountIds" Data..= memberAccountIds),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath ListDiscoveredResources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDiscoveredResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | Details of the resources that were discovered.
    items :: Prelude.Maybe [DiscoveredResource],
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listDiscoveredResourcesResponse_items' - Details of the resources that were discovered.
--
-- 'nextToken', 'listDiscoveredResourcesResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'httpStatus', 'listDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the resources that were discovered.
listDiscoveredResourcesResponse_items :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe [DiscoveredResource])
listDiscoveredResourcesResponse_items = Lens.lens (\ListDiscoveredResourcesResponse' {items} -> items) (\s@ListDiscoveredResourcesResponse' {} a -> s {items = a} :: ListDiscoveredResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe Prelude.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Prelude.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance
  Prelude.NFData
    ListDiscoveredResourcesResponse
  where
  rnf ListDiscoveredResourcesResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
