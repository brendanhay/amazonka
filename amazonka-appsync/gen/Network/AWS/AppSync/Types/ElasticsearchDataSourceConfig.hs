{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Elasticsearch data source configuration.
--
-- /See:/ 'newElasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { -- | The endpoint.
    endpoint :: Core.Text,
    -- | The AWS Region.
    awsRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'elasticsearchDataSourceConfig_endpoint' - The endpoint.
--
-- 'awsRegion', 'elasticsearchDataSourceConfig_awsRegion' - The AWS Region.
newElasticsearchDataSourceConfig ::
  -- | 'endpoint'
  Core.Text ->
  -- | 'awsRegion'
  Core.Text ->
  ElasticsearchDataSourceConfig
newElasticsearchDataSourceConfig
  pEndpoint_
  pAwsRegion_ =
    ElasticsearchDataSourceConfig'
      { endpoint =
          pEndpoint_,
        awsRegion = pAwsRegion_
      }

-- | The endpoint.
elasticsearchDataSourceConfig_endpoint :: Lens.Lens' ElasticsearchDataSourceConfig Core.Text
elasticsearchDataSourceConfig_endpoint = Lens.lens (\ElasticsearchDataSourceConfig' {endpoint} -> endpoint) (\s@ElasticsearchDataSourceConfig' {} a -> s {endpoint = a} :: ElasticsearchDataSourceConfig)

-- | The AWS Region.
elasticsearchDataSourceConfig_awsRegion :: Lens.Lens' ElasticsearchDataSourceConfig Core.Text
elasticsearchDataSourceConfig_awsRegion = Lens.lens (\ElasticsearchDataSourceConfig' {awsRegion} -> awsRegion) (\s@ElasticsearchDataSourceConfig' {} a -> s {awsRegion = a} :: ElasticsearchDataSourceConfig)

instance Core.FromJSON ElasticsearchDataSourceConfig where
  parseJSON =
    Core.withObject
      "ElasticsearchDataSourceConfig"
      ( \x ->
          ElasticsearchDataSourceConfig'
            Core.<$> (x Core..: "endpoint")
            Core.<*> (x Core..: "awsRegion")
      )

instance Core.Hashable ElasticsearchDataSourceConfig

instance Core.NFData ElasticsearchDataSourceConfig

instance Core.ToJSON ElasticsearchDataSourceConfig where
  toJSON ElasticsearchDataSourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("endpoint" Core..= endpoint),
            Core.Just ("awsRegion" Core..= awsRegion)
          ]
      )
