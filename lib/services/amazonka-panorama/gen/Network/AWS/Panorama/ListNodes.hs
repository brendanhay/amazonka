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
-- Module      : Network.AWS.Panorama.ListNodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of nodes.
module Network.AWS.Panorama.ListNodes
  ( -- * Creating a Request
    ListNodes (..),
    newListNodes,

    -- * Request Lenses
    listNodes_patchVersion,
    listNodes_packageName,
    listNodes_packageVersion,
    listNodes_category,
    listNodes_nextToken,
    listNodes_ownerAccount,
    listNodes_maxResults,

    -- * Destructuring the Response
    ListNodesResponse (..),
    newListNodesResponse,

    -- * Response Lenses
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Panorama.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListNodes' smart constructor.
data ListNodes = ListNodes'
  { -- | Search for nodes by patch version.
    patchVersion :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by version.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by category.
    category :: Prelude.Maybe NodeCategory,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by the account ID of the nodes\' owner.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of nodes to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchVersion', 'listNodes_patchVersion' - Search for nodes by patch version.
--
-- 'packageName', 'listNodes_packageName' - Search for nodes by name.
--
-- 'packageVersion', 'listNodes_packageVersion' - Search for nodes by version.
--
-- 'category', 'listNodes_category' - Search for nodes by category.
--
-- 'nextToken', 'listNodes_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'ownerAccount', 'listNodes_ownerAccount' - Search for nodes by the account ID of the nodes\' owner.
--
-- 'maxResults', 'listNodes_maxResults' - The maximum number of nodes to return in one page of results.
newListNodes ::
  ListNodes
newListNodes =
  ListNodes'
    { patchVersion = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      category = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Search for nodes by patch version.
listNodes_patchVersion :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_patchVersion = Lens.lens (\ListNodes' {patchVersion} -> patchVersion) (\s@ListNodes' {} a -> s {patchVersion = a} :: ListNodes)

-- | Search for nodes by name.
listNodes_packageName :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_packageName = Lens.lens (\ListNodes' {packageName} -> packageName) (\s@ListNodes' {} a -> s {packageName = a} :: ListNodes)

-- | Search for nodes by version.
listNodes_packageVersion :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_packageVersion = Lens.lens (\ListNodes' {packageVersion} -> packageVersion) (\s@ListNodes' {} a -> s {packageVersion = a} :: ListNodes)

-- | Search for nodes by category.
listNodes_category :: Lens.Lens' ListNodes (Prelude.Maybe NodeCategory)
listNodes_category = Lens.lens (\ListNodes' {category} -> category) (\s@ListNodes' {} a -> s {category = a} :: ListNodes)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listNodes_nextToken :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_nextToken = Lens.lens (\ListNodes' {nextToken} -> nextToken) (\s@ListNodes' {} a -> s {nextToken = a} :: ListNodes)

-- | Search for nodes by the account ID of the nodes\' owner.
listNodes_ownerAccount :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_ownerAccount = Lens.lens (\ListNodes' {ownerAccount} -> ownerAccount) (\s@ListNodes' {} a -> s {ownerAccount = a} :: ListNodes)

-- | The maximum number of nodes to return in one page of results.
listNodes_maxResults :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Natural)
listNodes_maxResults = Lens.lens (\ListNodes' {maxResults} -> maxResults) (\s@ListNodes' {} a -> s {maxResults = a} :: ListNodes)

instance Core.AWSRequest ListNodes where
  type AWSResponse ListNodes = ListNodesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNodesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNodes

instance Prelude.NFData ListNodes

instance Core.ToHeaders ListNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListNodes where
  toPath = Prelude.const "/nodes"

instance Core.ToQuery ListNodes where
  toQuery ListNodes' {..} =
    Prelude.mconcat
      [ "patchVersion" Core.=: patchVersion,
        "packageName" Core.=: packageName,
        "packageVersion" Core.=: packageVersion,
        "category" Core.=: category,
        "nextToken" Core.=: nextToken,
        "ownerAccount" Core.=: ownerAccount,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListNodesResponse' smart constructor.
data ListNodesResponse = ListNodesResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of nodes.
    nodes :: Prelude.Maybe [Node],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNodesResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'nodes', 'listNodesResponse_nodes' - A list of nodes.
--
-- 'httpStatus', 'listNodesResponse_httpStatus' - The response's http status code.
newListNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNodesResponse
newListNodesResponse pHttpStatus_ =
  ListNodesResponse'
    { nextToken = Prelude.Nothing,
      nodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that\'s included if more results are available.
listNodesResponse_nextToken :: Lens.Lens' ListNodesResponse (Prelude.Maybe Prelude.Text)
listNodesResponse_nextToken = Lens.lens (\ListNodesResponse' {nextToken} -> nextToken) (\s@ListNodesResponse' {} a -> s {nextToken = a} :: ListNodesResponse)

-- | A list of nodes.
listNodesResponse_nodes :: Lens.Lens' ListNodesResponse (Prelude.Maybe [Node])
listNodesResponse_nodes = Lens.lens (\ListNodesResponse' {nodes} -> nodes) (\s@ListNodesResponse' {} a -> s {nodes = a} :: ListNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNodesResponse_httpStatus :: Lens.Lens' ListNodesResponse Prelude.Int
listNodesResponse_httpStatus = Lens.lens (\ListNodesResponse' {httpStatus} -> httpStatus) (\s@ListNodesResponse' {} a -> s {httpStatus = a} :: ListNodesResponse)

instance Prelude.NFData ListNodesResponse
