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
-- Module      : Amazonka.Route53RecoveryControlConfig.ListControlPanels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of control panels in an account or in a cluster.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryControlConfig.ListControlPanels
  ( -- * Creating a Request
    ListControlPanels (..),
    newListControlPanels,

    -- * Request Lenses
    listControlPanels_clusterArn,
    listControlPanels_maxResults,
    listControlPanels_nextToken,

    -- * Destructuring the Response
    ListControlPanelsResponse (..),
    newListControlPanelsResponse,

    -- * Response Lenses
    listControlPanelsResponse_controlPanels,
    listControlPanelsResponse_nextToken,
    listControlPanelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newListControlPanels' smart constructor.
data ListControlPanels = ListControlPanels'
  { -- | The Amazon Resource Name (ARN) of a cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlPanels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'listControlPanels_clusterArn' - The Amazon Resource Name (ARN) of a cluster.
--
-- 'maxResults', 'listControlPanels_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listControlPanels_nextToken' - The token that identifies which batch of results you want to see.
newListControlPanels ::
  ListControlPanels
newListControlPanels =
  ListControlPanels'
    { clusterArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a cluster.
listControlPanels_clusterArn :: Lens.Lens' ListControlPanels (Prelude.Maybe Prelude.Text)
listControlPanels_clusterArn = Lens.lens (\ListControlPanels' {clusterArn} -> clusterArn) (\s@ListControlPanels' {} a -> s {clusterArn = a} :: ListControlPanels)

-- | The number of objects that you want to return with this call.
listControlPanels_maxResults :: Lens.Lens' ListControlPanels (Prelude.Maybe Prelude.Natural)
listControlPanels_maxResults = Lens.lens (\ListControlPanels' {maxResults} -> maxResults) (\s@ListControlPanels' {} a -> s {maxResults = a} :: ListControlPanels)

-- | The token that identifies which batch of results you want to see.
listControlPanels_nextToken :: Lens.Lens' ListControlPanels (Prelude.Maybe Prelude.Text)
listControlPanels_nextToken = Lens.lens (\ListControlPanels' {nextToken} -> nextToken) (\s@ListControlPanels' {} a -> s {nextToken = a} :: ListControlPanels)

instance Core.AWSPager ListControlPanels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listControlPanelsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listControlPanelsResponse_controlPanels
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listControlPanels_nextToken
              Lens..~ rs
              Lens.^? listControlPanelsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListControlPanels where
  type
    AWSResponse ListControlPanels =
      ListControlPanelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListControlPanelsResponse'
            Prelude.<$> (x Data..?> "ControlPanels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListControlPanels where
  hashWithSalt _salt ListControlPanels' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListControlPanels where
  rnf ListControlPanels' {..} =
    Prelude.rnf clusterArn `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListControlPanels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListControlPanels where
  toPath = Prelude.const "/controlpanels"

instance Data.ToQuery ListControlPanels where
  toQuery ListControlPanels' {..} =
    Prelude.mconcat
      [ "ClusterArn" Data.=: clusterArn,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListControlPanelsResponse' smart constructor.
data ListControlPanelsResponse = ListControlPanelsResponse'
  { -- | The result of a successful ListControlPanel request.
    controlPanels :: Prelude.Maybe [ControlPanel],
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlPanelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanels', 'listControlPanelsResponse_controlPanels' - The result of a successful ListControlPanel request.
--
-- 'nextToken', 'listControlPanelsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'httpStatus', 'listControlPanelsResponse_httpStatus' - The response's http status code.
newListControlPanelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListControlPanelsResponse
newListControlPanelsResponse pHttpStatus_ =
  ListControlPanelsResponse'
    { controlPanels =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result of a successful ListControlPanel request.
listControlPanelsResponse_controlPanels :: Lens.Lens' ListControlPanelsResponse (Prelude.Maybe [ControlPanel])
listControlPanelsResponse_controlPanels = Lens.lens (\ListControlPanelsResponse' {controlPanels} -> controlPanels) (\s@ListControlPanelsResponse' {} a -> s {controlPanels = a} :: ListControlPanelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results you want to see.
listControlPanelsResponse_nextToken :: Lens.Lens' ListControlPanelsResponse (Prelude.Maybe Prelude.Text)
listControlPanelsResponse_nextToken = Lens.lens (\ListControlPanelsResponse' {nextToken} -> nextToken) (\s@ListControlPanelsResponse' {} a -> s {nextToken = a} :: ListControlPanelsResponse)

-- | The response's http status code.
listControlPanelsResponse_httpStatus :: Lens.Lens' ListControlPanelsResponse Prelude.Int
listControlPanelsResponse_httpStatus = Lens.lens (\ListControlPanelsResponse' {httpStatus} -> httpStatus) (\s@ListControlPanelsResponse' {} a -> s {httpStatus = a} :: ListControlPanelsResponse)

instance Prelude.NFData ListControlPanelsResponse where
  rnf ListControlPanelsResponse' {..} =
    Prelude.rnf controlPanels `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
