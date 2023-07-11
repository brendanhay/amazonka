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
-- Module      : Amazonka.AppSync.Types.ElasticsearchDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ElasticsearchDataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an OpenSearch data source configuration.
--
-- As of September 2021, Amazon Elasticsearch service is Amazon OpenSearch
-- Service. This configuration is deprecated. For new data sources, use
-- OpenSearchServiceDataSourceConfig to specify an OpenSearch data source.
--
-- /See:/ 'newElasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { -- | The endpoint.
    endpoint :: Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'awsRegion', 'elasticsearchDataSourceConfig_awsRegion' - The Amazon Web Services Region.
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

-- | The Amazon Web Services Region.
elasticsearchDataSourceConfig_awsRegion :: Lens.Lens' ElasticsearchDataSourceConfig Prelude.Text
elasticsearchDataSourceConfig_awsRegion = Lens.lens (\ElasticsearchDataSourceConfig' {awsRegion} -> awsRegion) (\s@ElasticsearchDataSourceConfig' {} a -> s {awsRegion = a} :: ElasticsearchDataSourceConfig)

instance Data.FromJSON ElasticsearchDataSourceConfig where
  parseJSON =
    Data.withObject
      "ElasticsearchDataSourceConfig"
      ( \x ->
          ElasticsearchDataSourceConfig'
            Prelude.<$> (x Data..: "endpoint")
            Prelude.<*> (x Data..: "awsRegion")
      )

instance
  Prelude.Hashable
    ElasticsearchDataSourceConfig
  where
  hashWithSalt _salt ElasticsearchDataSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` awsRegion

instance Prelude.NFData ElasticsearchDataSourceConfig where
  rnf ElasticsearchDataSourceConfig' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf awsRegion

instance Data.ToJSON ElasticsearchDataSourceConfig where
  toJSON ElasticsearchDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("endpoint" Data..= endpoint),
            Prelude.Just ("awsRegion" Data..= awsRegion)
          ]
      )
