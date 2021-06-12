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
-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a
-- cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListBootstrapActions
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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which bootstrap actions to retrieve.
--
-- /See:/ 'newListBootstrapActions' smart constructor.
data ListBootstrapActions = ListBootstrapActions'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The cluster identifier for the bootstrap actions to list.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListBootstrapActions
newListBootstrapActions pClusterId_ =
  ListBootstrapActions'
    { marker = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listBootstrapActions_marker :: Lens.Lens' ListBootstrapActions (Core.Maybe Core.Text)
listBootstrapActions_marker = Lens.lens (\ListBootstrapActions' {marker} -> marker) (\s@ListBootstrapActions' {} a -> s {marker = a} :: ListBootstrapActions)

-- | The cluster identifier for the bootstrap actions to list.
listBootstrapActions_clusterId :: Lens.Lens' ListBootstrapActions Core.Text
listBootstrapActions_clusterId = Lens.lens (\ListBootstrapActions' {clusterId} -> clusterId) (\s@ListBootstrapActions' {} a -> s {clusterId = a} :: ListBootstrapActions)

instance Core.AWSPager ListBootstrapActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBootstrapActionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBootstrapActionsResponse_bootstrapActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBootstrapActions_marker
          Lens..~ rs
          Lens.^? listBootstrapActionsResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListBootstrapActions where
  type
    AWSResponse ListBootstrapActions =
      ListBootstrapActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBootstrapActionsResponse'
            Core.<$> (x Core..?> "BootstrapActions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBootstrapActions

instance Core.NFData ListBootstrapActions

instance Core.ToHeaders ListBootstrapActions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListBootstrapActions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBootstrapActions where
  toJSON ListBootstrapActions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListBootstrapActions where
  toPath = Core.const "/"

instance Core.ToQuery ListBootstrapActions where
  toQuery = Core.const Core.mempty

-- | This output contains the bootstrap actions detail.
--
-- /See:/ 'newListBootstrapActionsResponse' smart constructor.
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
  { -- | The bootstrap actions associated with the cluster.
    bootstrapActions :: Core.Maybe [Command],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListBootstrapActionsResponse
newListBootstrapActionsResponse pHttpStatus_ =
  ListBootstrapActionsResponse'
    { bootstrapActions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bootstrap actions associated with the cluster.
listBootstrapActionsResponse_bootstrapActions :: Lens.Lens' ListBootstrapActionsResponse (Core.Maybe [Command])
listBootstrapActionsResponse_bootstrapActions = Lens.lens (\ListBootstrapActionsResponse' {bootstrapActions} -> bootstrapActions) (\s@ListBootstrapActionsResponse' {} a -> s {bootstrapActions = a} :: ListBootstrapActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listBootstrapActionsResponse_marker :: Lens.Lens' ListBootstrapActionsResponse (Core.Maybe Core.Text)
listBootstrapActionsResponse_marker = Lens.lens (\ListBootstrapActionsResponse' {marker} -> marker) (\s@ListBootstrapActionsResponse' {} a -> s {marker = a} :: ListBootstrapActionsResponse)

-- | The response's http status code.
listBootstrapActionsResponse_httpStatus :: Lens.Lens' ListBootstrapActionsResponse Core.Int
listBootstrapActionsResponse_httpStatus = Lens.lens (\ListBootstrapActionsResponse' {httpStatus} -> httpStatus) (\s@ListBootstrapActionsResponse' {} a -> s {httpStatus = a} :: ListBootstrapActionsResponse)

instance Core.NFData ListBootstrapActionsResponse
