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
-- Module      : Amazonka.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListInstanceGroups
  ( -- * Creating a Request
    ListInstanceGroups (..),
    newListInstanceGroups,

    -- * Request Lenses
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,

    -- * Destructuring the Response
    ListInstanceGroupsResponse (..),
    newListInstanceGroupsResponse,

    -- * Response Lenses
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroups' smart constructor.
data ListInstanceGroups = ListInstanceGroups'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster for which to list the instance groups.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceGroups_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listInstanceGroups_clusterId' - The identifier of the cluster for which to list the instance groups.
newListInstanceGroups ::
  -- | 'clusterId'
  Prelude.Text ->
  ListInstanceGroups
newListInstanceGroups pClusterId_ =
  ListInstanceGroups'
    { marker = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceGroups_marker :: Lens.Lens' ListInstanceGroups (Prelude.Maybe Prelude.Text)
listInstanceGroups_marker = Lens.lens (\ListInstanceGroups' {marker} -> marker) (\s@ListInstanceGroups' {} a -> s {marker = a} :: ListInstanceGroups)

-- | The identifier of the cluster for which to list the instance groups.
listInstanceGroups_clusterId :: Lens.Lens' ListInstanceGroups Prelude.Text
listInstanceGroups_clusterId = Lens.lens (\ListInstanceGroups' {clusterId} -> clusterId) (\s@ListInstanceGroups' {} a -> s {clusterId = a} :: ListInstanceGroups)

instance Core.AWSPager ListInstanceGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_instanceGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceGroups_marker
          Lens..~ rs
          Lens.^? listInstanceGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceGroups where
  type
    AWSResponse ListInstanceGroups =
      ListInstanceGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceGroupsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "InstanceGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceGroups where
  hashWithSalt _salt ListInstanceGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListInstanceGroups where
  rnf ListInstanceGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf clusterId

instance Core.ToHeaders ListInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListInstanceGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInstanceGroups where
  toJSON ListInstanceGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            Prelude.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListInstanceGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInstanceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroupsResponse' smart constructor.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of instance groups for the cluster and given filters.
    instanceGroups :: Prelude.Maybe [InstanceGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceGroupsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'instanceGroups', 'listInstanceGroupsResponse_instanceGroups' - The list of instance groups for the cluster and given filters.
--
-- 'httpStatus', 'listInstanceGroupsResponse_httpStatus' - The response's http status code.
newListInstanceGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceGroupsResponse
newListInstanceGroupsResponse pHttpStatus_ =
  ListInstanceGroupsResponse'
    { marker =
        Prelude.Nothing,
      instanceGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceGroupsResponse_marker :: Lens.Lens' ListInstanceGroupsResponse (Prelude.Maybe Prelude.Text)
listInstanceGroupsResponse_marker = Lens.lens (\ListInstanceGroupsResponse' {marker} -> marker) (\s@ListInstanceGroupsResponse' {} a -> s {marker = a} :: ListInstanceGroupsResponse)

-- | The list of instance groups for the cluster and given filters.
listInstanceGroupsResponse_instanceGroups :: Lens.Lens' ListInstanceGroupsResponse (Prelude.Maybe [InstanceGroup])
listInstanceGroupsResponse_instanceGroups = Lens.lens (\ListInstanceGroupsResponse' {instanceGroups} -> instanceGroups) (\s@ListInstanceGroupsResponse' {} a -> s {instanceGroups = a} :: ListInstanceGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstanceGroupsResponse_httpStatus :: Lens.Lens' ListInstanceGroupsResponse Prelude.Int
listInstanceGroupsResponse_httpStatus = Lens.lens (\ListInstanceGroupsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceGroupsResponse' {} a -> s {httpStatus = a} :: ListInstanceGroupsResponse)

instance Prelude.NFData ListInstanceGroupsResponse where
  rnf ListInstanceGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf instanceGroups
      `Prelude.seq` Prelude.rnf httpStatus
