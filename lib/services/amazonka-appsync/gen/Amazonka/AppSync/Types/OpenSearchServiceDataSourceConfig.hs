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
-- Module      : Amazonka.AppSync.Types.OpenSearchServiceDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.OpenSearchServiceDataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an OpenSearch data source configuration.
--
-- /See:/ 'newOpenSearchServiceDataSourceConfig' smart constructor.
data OpenSearchServiceDataSourceConfig = OpenSearchServiceDataSourceConfig'
  { -- | The endpoint.
    endpoint :: Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenSearchServiceDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'openSearchServiceDataSourceConfig_endpoint' - The endpoint.
--
-- 'awsRegion', 'openSearchServiceDataSourceConfig_awsRegion' - The Amazon Web Services Region.
newOpenSearchServiceDataSourceConfig ::
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  OpenSearchServiceDataSourceConfig
newOpenSearchServiceDataSourceConfig
  pEndpoint_
  pAwsRegion_ =
    OpenSearchServiceDataSourceConfig'
      { endpoint =
          pEndpoint_,
        awsRegion = pAwsRegion_
      }

-- | The endpoint.
openSearchServiceDataSourceConfig_endpoint :: Lens.Lens' OpenSearchServiceDataSourceConfig Prelude.Text
openSearchServiceDataSourceConfig_endpoint = Lens.lens (\OpenSearchServiceDataSourceConfig' {endpoint} -> endpoint) (\s@OpenSearchServiceDataSourceConfig' {} a -> s {endpoint = a} :: OpenSearchServiceDataSourceConfig)

-- | The Amazon Web Services Region.
openSearchServiceDataSourceConfig_awsRegion :: Lens.Lens' OpenSearchServiceDataSourceConfig Prelude.Text
openSearchServiceDataSourceConfig_awsRegion = Lens.lens (\OpenSearchServiceDataSourceConfig' {awsRegion} -> awsRegion) (\s@OpenSearchServiceDataSourceConfig' {} a -> s {awsRegion = a} :: OpenSearchServiceDataSourceConfig)

instance
  Data.FromJSON
    OpenSearchServiceDataSourceConfig
  where
  parseJSON =
    Data.withObject
      "OpenSearchServiceDataSourceConfig"
      ( \x ->
          OpenSearchServiceDataSourceConfig'
            Prelude.<$> (x Data..: "endpoint")
            Prelude.<*> (x Data..: "awsRegion")
      )

instance
  Prelude.Hashable
    OpenSearchServiceDataSourceConfig
  where
  hashWithSalt
    _salt
    OpenSearchServiceDataSourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` endpoint
        `Prelude.hashWithSalt` awsRegion

instance
  Prelude.NFData
    OpenSearchServiceDataSourceConfig
  where
  rnf OpenSearchServiceDataSourceConfig' {..} =
    Prelude.rnf endpoint `Prelude.seq`
      Prelude.rnf awsRegion

instance
  Data.ToJSON
    OpenSearchServiceDataSourceConfig
  where
  toJSON OpenSearchServiceDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("endpoint" Data..= endpoint),
            Prelude.Just ("awsRegion" Data..= awsRegion)
          ]
      )
