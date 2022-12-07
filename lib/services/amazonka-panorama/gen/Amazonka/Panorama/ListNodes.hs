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
-- Module      : Amazonka.Panorama.ListNodes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of nodes.
module Amazonka.Panorama.ListNodes
  ( -- * Creating a Request
    ListNodes (..),
    newListNodes,

    -- * Request Lenses
    listNodes_nextToken,
    listNodes_packageName,
    listNodes_packageVersion,
    listNodes_maxResults,
    listNodes_category,
    listNodes_patchVersion,
    listNodes_ownerAccount,

    -- * Destructuring the Response
    ListNodesResponse (..),
    newListNodesResponse,

    -- * Response Lenses
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNodes' smart constructor.
data ListNodes = ListNodes'
  { -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by version.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of nodes to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Search for nodes by category.
    category :: Prelude.Maybe NodeCategory,
    -- | Search for nodes by patch version.
    patchVersion :: Prelude.Maybe Prelude.Text,
    -- | Search for nodes by the account ID of the nodes\' owner.
    ownerAccount :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listNodes_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'packageName', 'listNodes_packageName' - Search for nodes by name.
--
-- 'packageVersion', 'listNodes_packageVersion' - Search for nodes by version.
--
-- 'maxResults', 'listNodes_maxResults' - The maximum number of nodes to return in one page of results.
--
-- 'category', 'listNodes_category' - Search for nodes by category.
--
-- 'patchVersion', 'listNodes_patchVersion' - Search for nodes by patch version.
--
-- 'ownerAccount', 'listNodes_ownerAccount' - Search for nodes by the account ID of the nodes\' owner.
newListNodes ::
  ListNodes
newListNodes =
  ListNodes'
    { nextToken = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      category = Prelude.Nothing,
      patchVersion = Prelude.Nothing,
      ownerAccount = Prelude.Nothing
    }

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listNodes_nextToken :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_nextToken = Lens.lens (\ListNodes' {nextToken} -> nextToken) (\s@ListNodes' {} a -> s {nextToken = a} :: ListNodes)

-- | Search for nodes by name.
listNodes_packageName :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_packageName = Lens.lens (\ListNodes' {packageName} -> packageName) (\s@ListNodes' {} a -> s {packageName = a} :: ListNodes)

-- | Search for nodes by version.
listNodes_packageVersion :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_packageVersion = Lens.lens (\ListNodes' {packageVersion} -> packageVersion) (\s@ListNodes' {} a -> s {packageVersion = a} :: ListNodes)

-- | The maximum number of nodes to return in one page of results.
listNodes_maxResults :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Natural)
listNodes_maxResults = Lens.lens (\ListNodes' {maxResults} -> maxResults) (\s@ListNodes' {} a -> s {maxResults = a} :: ListNodes)

-- | Search for nodes by category.
listNodes_category :: Lens.Lens' ListNodes (Prelude.Maybe NodeCategory)
listNodes_category = Lens.lens (\ListNodes' {category} -> category) (\s@ListNodes' {} a -> s {category = a} :: ListNodes)

-- | Search for nodes by patch version.
listNodes_patchVersion :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_patchVersion = Lens.lens (\ListNodes' {patchVersion} -> patchVersion) (\s@ListNodes' {} a -> s {patchVersion = a} :: ListNodes)

-- | Search for nodes by the account ID of the nodes\' owner.
listNodes_ownerAccount :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_ownerAccount = Lens.lens (\ListNodes' {ownerAccount} -> ownerAccount) (\s@ListNodes' {} a -> s {ownerAccount = a} :: ListNodes)

instance Core.AWSRequest ListNodes where
  type AWSResponse ListNodes = ListNodesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNodesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNodes where
  hashWithSalt _salt ListNodes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` patchVersion
      `Prelude.hashWithSalt` ownerAccount

instance Prelude.NFData ListNodes where
  rnf ListNodes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf patchVersion
      `Prelude.seq` Prelude.rnf ownerAccount

instance Data.ToHeaders ListNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNodes where
  toPath = Prelude.const "/nodes"

instance Data.ToQuery ListNodes where
  toQuery ListNodes' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "packageName" Data.=: packageName,
        "packageVersion" Data.=: packageVersion,
        "maxResults" Data.=: maxResults,
        "category" Data.=: category,
        "patchVersion" Data.=: patchVersion,
        "ownerAccount" Data.=: ownerAccount
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

instance Prelude.NFData ListNodesResponse where
  rnf ListNodesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf httpStatus
