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
-- Module      : Amazonka.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a
-- cluster.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListBootstrapActions
  ( -- * Creating a Request
    ListBootstrapActions (..),
    newListBootstrapActions,

    -- * Request Lenses
    listBootstrapActions_marker,
    listBootstrapActions_clusterId,

    -- * Destructuring the Response
    ListBootstrapActionsResponse (..),
    newListBootstrapActionsResponse,

    -- * Response Lenses
    listBootstrapActionsResponse_bootstrapActions,
    listBootstrapActionsResponse_marker,
    listBootstrapActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input determines which bootstrap actions to retrieve.
--
-- /See:/ 'newListBootstrapActions' smart constructor.
data ListBootstrapActions = ListBootstrapActions'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier for the bootstrap actions to list.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBootstrapActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listBootstrapActions_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listBootstrapActions_clusterId' - The cluster identifier for the bootstrap actions to list.
newListBootstrapActions ::
  -- | 'clusterId'
  Prelude.Text ->
  ListBootstrapActions
newListBootstrapActions pClusterId_ =
  ListBootstrapActions'
    { marker = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listBootstrapActions_marker :: Lens.Lens' ListBootstrapActions (Prelude.Maybe Prelude.Text)
listBootstrapActions_marker = Lens.lens (\ListBootstrapActions' {marker} -> marker) (\s@ListBootstrapActions' {} a -> s {marker = a} :: ListBootstrapActions)

-- | The cluster identifier for the bootstrap actions to list.
listBootstrapActions_clusterId :: Lens.Lens' ListBootstrapActions Prelude.Text
listBootstrapActions_clusterId = Lens.lens (\ListBootstrapActions' {clusterId} -> clusterId) (\s@ListBootstrapActions' {} a -> s {clusterId = a} :: ListBootstrapActions)

instance Core.AWSPager ListBootstrapActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBootstrapActionsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBootstrapActionsResponse_bootstrapActions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBootstrapActions_marker
          Lens..~ rs
          Lens.^? listBootstrapActionsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListBootstrapActions where
  type
    AWSResponse ListBootstrapActions =
      ListBootstrapActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBootstrapActionsResponse'
            Prelude.<$> ( x
                            Data..?> "BootstrapActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBootstrapActions where
  hashWithSalt _salt ListBootstrapActions' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListBootstrapActions where
  rnf ListBootstrapActions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders ListBootstrapActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ListBootstrapActions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBootstrapActions where
  toJSON ListBootstrapActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ListBootstrapActions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBootstrapActions where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the bootstrap actions detail.
--
-- /See:/ 'newListBootstrapActionsResponse' smart constructor.
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
  { -- | The bootstrap actions associated with the cluster.
    bootstrapActions :: Prelude.Maybe [Command],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBootstrapActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bootstrapActions', 'listBootstrapActionsResponse_bootstrapActions' - The bootstrap actions associated with the cluster.
--
-- 'marker', 'listBootstrapActionsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listBootstrapActionsResponse_httpStatus' - The response's http status code.
newListBootstrapActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBootstrapActionsResponse
newListBootstrapActionsResponse pHttpStatus_ =
  ListBootstrapActionsResponse'
    { bootstrapActions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bootstrap actions associated with the cluster.
listBootstrapActionsResponse_bootstrapActions :: Lens.Lens' ListBootstrapActionsResponse (Prelude.Maybe [Command])
listBootstrapActionsResponse_bootstrapActions = Lens.lens (\ListBootstrapActionsResponse' {bootstrapActions} -> bootstrapActions) (\s@ListBootstrapActionsResponse' {} a -> s {bootstrapActions = a} :: ListBootstrapActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
listBootstrapActionsResponse_marker :: Lens.Lens' ListBootstrapActionsResponse (Prelude.Maybe Prelude.Text)
listBootstrapActionsResponse_marker = Lens.lens (\ListBootstrapActionsResponse' {marker} -> marker) (\s@ListBootstrapActionsResponse' {} a -> s {marker = a} :: ListBootstrapActionsResponse)

-- | The response's http status code.
listBootstrapActionsResponse_httpStatus :: Lens.Lens' ListBootstrapActionsResponse Prelude.Int
listBootstrapActionsResponse_httpStatus = Lens.lens (\ListBootstrapActionsResponse' {httpStatus} -> httpStatus) (\s@ListBootstrapActionsResponse' {} a -> s {httpStatus = a} :: ListBootstrapActionsResponse)

instance Prelude.NFData ListBootstrapActionsResponse where
  rnf ListBootstrapActionsResponse' {..} =
    Prelude.rnf bootstrapActions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
