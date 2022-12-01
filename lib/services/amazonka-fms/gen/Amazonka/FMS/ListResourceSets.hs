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
-- Module      : Amazonka.FMS.ListResourceSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ResourceSetSummary@ objects.
module Amazonka.FMS.ListResourceSets
  ( -- * Creating a Request
    ListResourceSets (..),
    newListResourceSets,

    -- * Request Lenses
    listResourceSets_nextToken,
    listResourceSets_maxResults,

    -- * Destructuring the Response
    ListResourceSetsResponse (..),
    newListResourceSetsResponse,

    -- * Response Lenses
    listResourceSetsResponse_nextToken,
    listResourceSetsResponse_resourceSets,
    listResourceSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceSets' smart constructor.
data ListResourceSets = ListResourceSets'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSets_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'maxResults', 'listResourceSets_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
newListResourceSets ::
  ListResourceSets
newListResourceSets =
  ListResourceSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listResourceSets_nextToken :: Lens.Lens' ListResourceSets (Prelude.Maybe Prelude.Text)
listResourceSets_nextToken = Lens.lens (\ListResourceSets' {nextToken} -> nextToken) (\s@ListResourceSets' {} a -> s {nextToken = a} :: ListResourceSets)

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listResourceSets_maxResults :: Lens.Lens' ListResourceSets (Prelude.Maybe Prelude.Natural)
listResourceSets_maxResults = Lens.lens (\ListResourceSets' {maxResults} -> maxResults) (\s@ListResourceSets' {} a -> s {maxResults = a} :: ListResourceSets)

instance Core.AWSRequest ListResourceSets where
  type
    AWSResponse ListResourceSets =
      ListResourceSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceSetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ResourceSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceSets where
  hashWithSalt _salt ListResourceSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListResourceSets where
  rnf ListResourceSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListResourceSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.ListResourceSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceSets where
  toJSON ListResourceSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResourceSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourceSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceSetsResponse' smart constructor.
data ListResourceSetsResponse = ListResourceSetsResponse'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @ResourceSetSummary@ objects.
    resourceSets :: Prelude.Maybe [ResourceSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSetsResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'resourceSets', 'listResourceSetsResponse_resourceSets' - An array of @ResourceSetSummary@ objects.
--
-- 'httpStatus', 'listResourceSetsResponse_httpStatus' - The response's http status code.
newListResourceSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceSetsResponse
newListResourceSetsResponse pHttpStatus_ =
  ListResourceSetsResponse'
    { nextToken =
        Prelude.Nothing,
      resourceSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listResourceSetsResponse_nextToken :: Lens.Lens' ListResourceSetsResponse (Prelude.Maybe Prelude.Text)
listResourceSetsResponse_nextToken = Lens.lens (\ListResourceSetsResponse' {nextToken} -> nextToken) (\s@ListResourceSetsResponse' {} a -> s {nextToken = a} :: ListResourceSetsResponse)

-- | An array of @ResourceSetSummary@ objects.
listResourceSetsResponse_resourceSets :: Lens.Lens' ListResourceSetsResponse (Prelude.Maybe [ResourceSetSummary])
listResourceSetsResponse_resourceSets = Lens.lens (\ListResourceSetsResponse' {resourceSets} -> resourceSets) (\s@ListResourceSetsResponse' {} a -> s {resourceSets = a} :: ListResourceSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceSetsResponse_httpStatus :: Lens.Lens' ListResourceSetsResponse Prelude.Int
listResourceSetsResponse_httpStatus = Lens.lens (\ListResourceSetsResponse' {httpStatus} -> httpStatus) (\s@ListResourceSetsResponse' {} a -> s {httpStatus = a} :: ListResourceSetsResponse)

instance Prelude.NFData ListResourceSetsResponse where
  rnf ListResourceSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceSets
      `Prelude.seq` Prelude.rnf httpStatus
