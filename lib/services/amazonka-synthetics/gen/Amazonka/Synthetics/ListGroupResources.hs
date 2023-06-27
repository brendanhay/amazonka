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
-- Module      : Amazonka.Synthetics.ListGroupResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a list of the ARNs of the canaries that are
-- associated with the specified group.
module Amazonka.Synthetics.ListGroupResources
  ( -- * Creating a Request
    ListGroupResources (..),
    newListGroupResources,

    -- * Request Lenses
    listGroupResources_maxResults,
    listGroupResources_nextToken,
    listGroupResources_groupIdentifier,

    -- * Destructuring the Response
    ListGroupResourcesResponse (..),
    newListGroupResourcesResponse,

    -- * Response Lenses
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newListGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { -- | Specify this parameter to limit how many canary ARNs are returned each
    -- time you use the @ListGroupResources@ operation. If you omit this
    -- parameter, the default of 20 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent operation to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the group to return information for. You can specify the group
    -- name, the ARN, or the group ID as the @GroupIdentifier@.
    groupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGroupResources_maxResults' - Specify this parameter to limit how many canary ARNs are returned each
-- time you use the @ListGroupResources@ operation. If you omit this
-- parameter, the default of 20 is used.
--
-- 'nextToken', 'listGroupResources_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
--
-- 'groupIdentifier', 'listGroupResources_groupIdentifier' - Specifies the group to return information for. You can specify the group
-- name, the ARN, or the group ID as the @GroupIdentifier@.
newListGroupResources ::
  -- | 'groupIdentifier'
  Prelude.Text ->
  ListGroupResources
newListGroupResources pGroupIdentifier_ =
  ListGroupResources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      groupIdentifier = pGroupIdentifier_
    }

-- | Specify this parameter to limit how many canary ARNs are returned each
-- time you use the @ListGroupResources@ operation. If you omit this
-- parameter, the default of 20 is used.
listGroupResources_maxResults :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Natural)
listGroupResources_maxResults = Lens.lens (\ListGroupResources' {maxResults} -> maxResults) (\s@ListGroupResources' {} a -> s {maxResults = a} :: ListGroupResources)

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
listGroupResources_nextToken :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Text)
listGroupResources_nextToken = Lens.lens (\ListGroupResources' {nextToken} -> nextToken) (\s@ListGroupResources' {} a -> s {nextToken = a} :: ListGroupResources)

-- | Specifies the group to return information for. You can specify the group
-- name, the ARN, or the group ID as the @GroupIdentifier@.
listGroupResources_groupIdentifier :: Lens.Lens' ListGroupResources Prelude.Text
listGroupResources_groupIdentifier = Lens.lens (\ListGroupResources' {groupIdentifier} -> groupIdentifier) (\s@ListGroupResources' {} a -> s {groupIdentifier = a} :: ListGroupResources)

instance Core.AWSRequest ListGroupResources where
  type
    AWSResponse ListGroupResources =
      ListGroupResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupResources where
  hashWithSalt _salt ListGroupResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` groupIdentifier

instance Prelude.NFData ListGroupResources where
  rnf ListGroupResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf groupIdentifier

instance Data.ToHeaders ListGroupResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGroupResources where
  toJSON ListGroupResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListGroupResources where
  toPath ListGroupResources' {..} =
    Prelude.mconcat
      ["/group/", Data.toBS groupIdentifier, "/resources"]

instance Data.ToQuery ListGroupResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @ListGroupResources@ operation to retrieve
    -- the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of ARNs. These ARNs are for the canaries that are associated
    -- with the group.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupResourcesResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListGroupResources@ operation to retrieve
-- the next set of results.
--
-- 'resources', 'listGroupResourcesResponse_resources' - An array of ARNs. These ARNs are for the canaries that are associated
-- with the group.
--
-- 'httpStatus', 'listGroupResourcesResponse_httpStatus' - The response's http status code.
newListGroupResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupResourcesResponse
newListGroupResourcesResponse pHttpStatus_ =
  ListGroupResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListGroupResources@ operation to retrieve
-- the next set of results.
listGroupResourcesResponse_nextToken :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe Prelude.Text)
listGroupResourcesResponse_nextToken = Lens.lens (\ListGroupResourcesResponse' {nextToken} -> nextToken) (\s@ListGroupResourcesResponse' {} a -> s {nextToken = a} :: ListGroupResourcesResponse)

-- | An array of ARNs. These ARNs are for the canaries that are associated
-- with the group.
listGroupResourcesResponse_resources :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe [Prelude.Text])
listGroupResourcesResponse_resources = Lens.lens (\ListGroupResourcesResponse' {resources} -> resources) (\s@ListGroupResourcesResponse' {} a -> s {resources = a} :: ListGroupResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGroupResourcesResponse_httpStatus :: Lens.Lens' ListGroupResourcesResponse Prelude.Int
listGroupResourcesResponse_httpStatus = Lens.lens (\ListGroupResourcesResponse' {httpStatus} -> httpStatus) (\s@ListGroupResourcesResponse' {} a -> s {httpStatus = a} :: ListGroupResourcesResponse)

instance Prelude.NFData ListGroupResourcesResponse where
  rnf ListGroupResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
