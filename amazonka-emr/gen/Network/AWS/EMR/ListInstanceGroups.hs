{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceGroups
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
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroups' smart constructor.
data ListInstanceGroups = ListInstanceGroups'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster for which to list the instance groups.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListInstanceGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_instanceGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listInstanceGroups_marker
          Lens..~ rs
          Lens.^? listInstanceGroupsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListInstanceGroups where
  type
    Rs ListInstanceGroups =
      ListInstanceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceGroupsResponse'
            Prelude.<$> ( x Prelude..?> "InstanceGroups"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceGroups

instance Prelude.NFData ListInstanceGroups

instance Prelude.ToHeaders ListInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ListInstanceGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListInstanceGroups where
  toJSON ListInstanceGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Marker" Prelude..=) Prelude.<$> marker,
            Prelude.Just ("ClusterId" Prelude..= clusterId)
          ]
      )

instance Prelude.ToPath ListInstanceGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListInstanceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroupsResponse' smart constructor.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
  { -- | The list of instance groups for the cluster and given filters.
    instanceGroups :: Prelude.Maybe [InstanceGroup],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceGroups', 'listInstanceGroupsResponse_instanceGroups' - The list of instance groups for the cluster and given filters.
--
-- 'marker', 'listInstanceGroupsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listInstanceGroupsResponse_httpStatus' - The response's http status code.
newListInstanceGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceGroupsResponse
newListInstanceGroupsResponse pHttpStatus_ =
  ListInstanceGroupsResponse'
    { instanceGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of instance groups for the cluster and given filters.
listInstanceGroupsResponse_instanceGroups :: Lens.Lens' ListInstanceGroupsResponse (Prelude.Maybe [InstanceGroup])
listInstanceGroupsResponse_instanceGroups = Lens.lens (\ListInstanceGroupsResponse' {instanceGroups} -> instanceGroups) (\s@ListInstanceGroupsResponse' {} a -> s {instanceGroups = a} :: ListInstanceGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceGroupsResponse_marker :: Lens.Lens' ListInstanceGroupsResponse (Prelude.Maybe Prelude.Text)
listInstanceGroupsResponse_marker = Lens.lens (\ListInstanceGroupsResponse' {marker} -> marker) (\s@ListInstanceGroupsResponse' {} a -> s {marker = a} :: ListInstanceGroupsResponse)

-- | The response's http status code.
listInstanceGroupsResponse_httpStatus :: Lens.Lens' ListInstanceGroupsResponse Prelude.Int
listInstanceGroupsResponse_httpStatus = Lens.lens (\ListInstanceGroupsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceGroupsResponse' {} a -> s {httpStatus = a} :: ListInstanceGroupsResponse)

instance Prelude.NFData ListInstanceGroupsResponse
