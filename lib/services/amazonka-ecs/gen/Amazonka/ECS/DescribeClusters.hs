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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
module Amazonka.ECS.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_clusters,
    describeClusters_include,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_clusters,
    describeClustersResponse_failures,
    describeClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | A list of up to 100 cluster names or full cluster Amazon Resource Name
    -- (ARN) entries. If you do not specify a cluster, the default cluster is
    -- assumed.
    clusters :: Prelude.Maybe [Prelude.Text],
    -- | Determines whether to include additional information about the clusters
    -- in the response. If this field is omitted, this information isn\'t
    -- included.
    --
    -- If @ATTACHMENTS@ is specified, the attachments for the container
    -- instances or tasks within the cluster are included, for example the
    -- capacity providers.
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
    include :: Prelude.Maybe [ClusterField]
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
-- 'clusters', 'describeClusters_clusters' - A list of up to 100 cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
--
-- 'include', 'describeClusters_include' - Determines whether to include additional information about the clusters
-- in the response. If this field is omitted, this information isn\'t
-- included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included, for example the
-- capacity providers.
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
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { clusters = Prelude.Nothing,
      include = Prelude.Nothing
    }

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
describeClusters_clusters :: Lens.Lens' DescribeClusters (Prelude.Maybe [Prelude.Text])
describeClusters_clusters = Lens.lens (\DescribeClusters' {clusters} -> clusters) (\s@DescribeClusters' {} a -> s {clusters = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

-- | Determines whether to include additional information about the clusters
-- in the response. If this field is omitted, this information isn\'t
-- included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container
-- instances or tasks within the cluster are included, for example the
-- capacity providers.
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

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Prelude.<$> (x Data..?> "clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters where
  hashWithSalt _salt DescribeClusters' {..} =
    _salt
      `Prelude.hashWithSalt` clusters
      `Prelude.hashWithSalt` include

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf include

instance Data.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DescribeClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clusters" Data..=) Prelude.<$> clusters,
            ("include" Data..=) Prelude.<$> include
          ]
      )

instance Data.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | The list of clusters.
    clusters :: Prelude.Maybe [Cluster],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
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
-- 'clusters', 'describeClustersResponse_clusters' - The list of clusters.
--
-- 'failures', 'describeClustersResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'describeClustersResponse_httpStatus' - The response's http status code.
newDescribeClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { clusters =
        Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of clusters.
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any failures associated with the call.
describeClustersResponse_failures :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Failure])
describeClustersResponse_failures = Lens.lens (\DescribeClustersResponse' {failures} -> failures) (\s@DescribeClustersResponse' {} a -> s {failures = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse where
  rnf DescribeClustersResponse' {..} =
    Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
