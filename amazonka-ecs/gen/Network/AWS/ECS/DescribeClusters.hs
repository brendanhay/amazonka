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
-- Module      : Network.AWS.ECS.DescribeClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
module Network.AWS.ECS.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_include,
    describeClusters_clusters,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_failures,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | Whether to include additional information about your clusters in the
    -- response. If this field is omitted, the attachments, statistics, and
    -- tags are not included.
    --
    -- If @ATTACHMENTS@ is specified, the attachments for the container
    -- instances or tasks within the cluster are included.
    --
    -- If @SETTINGS@ is specified, the settings for the cluster are included.
    --
    -- If @STATISTICS@ is specified, the following additional information,
    -- separated by launch type, is included:
    --
    -- -   runningEC2TasksCount
    --
    -- -   runningFargateTasksCount
    --
    -- -   pendingEC2TasksCount
    --
    -- -   pendingFargateTasksCount
    --
    -- -   activeEC2ServiceCount
    --
    -- -   activeFargateServiceCount
    --
    -- -   drainingEC2ServiceCount
    --
    -- -   drainingFargateServiceCount
    --
    -- If @TAGS@ is specified, the metadata tags associated with the cluster
    -- are included.
    include :: Core.Maybe [ClusterField],
    -- | A list of up to 100 cluster names or full cluster Amazon Resource Name
    -- (ARN) entries. If you do not specify a cluster, the default cluster is
    -- assumed.
    clusters :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeClusters_include' - Whether to include additional information about your clusters in the
-- response. If this field is omitted, the attachments, statistics, and
-- tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included.
--
-- If @SETTINGS@ is specified, the settings for the cluster are included.
--
-- If @STATISTICS@ is specified, the following additional information,
-- separated by launch type, is included:
--
-- -   runningEC2TasksCount
--
-- -   runningFargateTasksCount
--
-- -   pendingEC2TasksCount
--
-- -   pendingFargateTasksCount
--
-- -   activeEC2ServiceCount
--
-- -   activeFargateServiceCount
--
-- -   drainingEC2ServiceCount
--
-- -   drainingFargateServiceCount
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster
-- are included.
--
-- 'clusters', 'describeClusters_clusters' - A list of up to 100 cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { include = Core.Nothing,
      clusters = Core.Nothing
    }

-- | Whether to include additional information about your clusters in the
-- response. If this field is omitted, the attachments, statistics, and
-- tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included.
--
-- If @SETTINGS@ is specified, the settings for the cluster are included.
--
-- If @STATISTICS@ is specified, the following additional information,
-- separated by launch type, is included:
--
-- -   runningEC2TasksCount
--
-- -   runningFargateTasksCount
--
-- -   pendingEC2TasksCount
--
-- -   pendingFargateTasksCount
--
-- -   activeEC2ServiceCount
--
-- -   activeFargateServiceCount
--
-- -   drainingEC2ServiceCount
--
-- -   drainingFargateServiceCount
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster
-- are included.
describeClusters_include :: Lens.Lens' DescribeClusters (Core.Maybe [ClusterField])
describeClusters_include = Lens.lens (\DescribeClusters' {include} -> include) (\s@DescribeClusters' {} a -> s {include = a} :: DescribeClusters) Core.. Lens.mapping Lens._Coerce

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
describeClusters_clusters :: Lens.Lens' DescribeClusters (Core.Maybe [Core.Text])
describeClusters_clusters = Lens.lens (\DescribeClusters' {clusters} -> clusters) (\s@DescribeClusters' {} a -> s {clusters = a} :: DescribeClusters) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Core.<$> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "clusters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusters

instance Core.NFData DescribeClusters

instance Core.ToHeaders DescribeClusters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeClusters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("include" Core..=) Core.<$> include,
            ("clusters" Core..=) Core.<$> clusters
          ]
      )

instance Core.ToPath DescribeClusters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The list of clusters.
    clusters :: Core.Maybe [Cluster],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'describeClustersResponse_failures' - Any failures associated with the call.
--
-- 'clusters', 'describeClustersResponse_clusters' - The list of clusters.
--
-- 'httpStatus', 'describeClustersResponse_httpStatus' - The response's http status code.
newDescribeClustersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { failures = Core.Nothing,
      clusters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
describeClustersResponse_failures :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Failure])
describeClustersResponse_failures = Lens.lens (\DescribeClustersResponse' {failures} -> failures) (\s@DescribeClustersResponse' {} a -> s {failures = a} :: DescribeClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | The list of clusters.
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Core.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Core.NFData DescribeClustersResponse
