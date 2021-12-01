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
-- Module      : Amazonka.ECS.DescribeClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
module Amazonka.ECS.DescribeClusters
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

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | Whether to include additional information about the clusters in the
    -- response. If this field is omitted, this information isn\'t included.
    --
    -- If @ATTACHMENTS@ is specified, the attachments for the container
    -- instances or tasks within the cluster are included.
    --
    -- If @SETTINGS@ is specified, the settings for the cluster are included.
    --
    -- If @CONFIGURATIONS@ is specified, the configuration for the cluster is
    -- included.
    --
    -- If @STATISTICS@ is specified, the task and service count is included,
    -- separated by launch type.
    --
    -- If @TAGS@ is specified, the metadata tags associated with the cluster
    -- are included.
    include :: Prelude.Maybe [ClusterField],
    -- | A list of up to 100 cluster names or full cluster Amazon Resource Name
    -- (ARN) entries. If you do not specify a cluster, the default cluster is
    -- assumed.
    clusters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeClusters_include' - Whether to include additional information about the clusters in the
-- response. If this field is omitted, this information isn\'t included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included.
--
-- If @SETTINGS@ is specified, the settings for the cluster are included.
--
-- If @CONFIGURATIONS@ is specified, the configuration for the cluster is
-- included.
--
-- If @STATISTICS@ is specified, the task and service count is included,
-- separated by launch type.
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
    { include = Prelude.Nothing,
      clusters = Prelude.Nothing
    }

-- | Whether to include additional information about the clusters in the
-- response. If this field is omitted, this information isn\'t included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included.
--
-- If @SETTINGS@ is specified, the settings for the cluster are included.
--
-- If @CONFIGURATIONS@ is specified, the configuration for the cluster is
-- included.
--
-- If @STATISTICS@ is specified, the task and service count is included,
-- separated by launch type.
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster
-- are included.
describeClusters_include :: Lens.Lens' DescribeClusters (Prelude.Maybe [ClusterField])
describeClusters_include = Lens.lens (\DescribeClusters' {include} -> include) (\s@DescribeClusters' {} a -> s {include = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
describeClusters_clusters :: Lens.Lens' DescribeClusters (Prelude.Maybe [Prelude.Text])
describeClusters_clusters = Lens.lens (\DescribeClusters' {clusters} -> clusters) (\s@DescribeClusters' {} a -> s {clusters = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Prelude.<$> (x Core..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters where
  hashWithSalt salt' DescribeClusters' {..} =
    salt' `Prelude.hashWithSalt` clusters
      `Prelude.hashWithSalt` include

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf include
      `Prelude.seq` Prelude.rnf clusters

instance Core.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("include" Core..=) Prelude.<$> include,
            ("clusters" Core..=) Prelude.<$> clusters
          ]
      )

instance Core.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The list of clusters.
    clusters :: Prelude.Maybe [Cluster],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { failures =
        Prelude.Nothing,
      clusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
describeClustersResponse_failures :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Failure])
describeClustersResponse_failures = Lens.lens (\DescribeClustersResponse' {failures} -> failures) (\s@DescribeClustersResponse' {} a -> s {failures = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of clusters.
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse where
  rnf DescribeClustersResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf clusters
