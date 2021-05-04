{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Elasticsearch data source configuration.
--
-- /See:/ 'newElasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { -- | The endpoint.
    endpoint :: Prelude.Text,
    -- | The AWS Region.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
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
elasticsearchDataSourceConfig_endpoint :: Lens.Lens' ElasticsearchDataSourceConfig Prelude.Text
elasticsearchDataSourceConfig_endpoint = Lens.lens (\ElasticsearchDataSourceConfig' {endpoint} -> endpoint) (\s@ElasticsearchDataSourceConfig' {} a -> s {endpoint = a} :: ElasticsearchDataSourceConfig)

-- | The AWS Region.
elasticsearchDataSourceConfig_awsRegion :: Lens.Lens' ElasticsearchDataSourceConfig Prelude.Text
elasticsearchDataSourceConfig_awsRegion = Lens.lens (\ElasticsearchDataSourceConfig' {awsRegion} -> awsRegion) (\s@ElasticsearchDataSourceConfig' {} a -> s {awsRegion = a} :: ElasticsearchDataSourceConfig)

instance
  Prelude.FromJSON
    ElasticsearchDataSourceConfig
  where
  parseJSON =
    Prelude.withObject
      "ElasticsearchDataSourceConfig"
      ( \x ->
          ElasticsearchDataSourceConfig'
            Prelude.<$> (x Prelude..: "endpoint")
            Prelude.<*> (x Prelude..: "awsRegion")
      )

instance
  Prelude.Hashable
    ElasticsearchDataSourceConfig

instance Prelude.NFData ElasticsearchDataSourceConfig

instance Prelude.ToJSON ElasticsearchDataSourceConfig where
  toJSON ElasticsearchDataSourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("endpoint" Prelude..= endpoint),
            Prelude.Just ("awsRegion" Prelude..= awsRegion)
          ]
      )
