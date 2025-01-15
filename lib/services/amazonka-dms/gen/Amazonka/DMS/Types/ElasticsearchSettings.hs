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
-- Module      : Amazonka.DMS.Types.ElasticsearchSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ElasticsearchSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines an OpenSearch endpoint.
--
-- /See:/ 'newElasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { -- | The maximum number of seconds for which DMS retries failed API requests
    -- to the OpenSearch cluster.
    errorRetryDuration :: Prelude.Maybe Prelude.Int,
    -- | The maximum percentage of records that can fail to be written before a
    -- full load operation stops.
    --
    -- To avoid early failure, this counter is only effective after 1000
    -- records are transferred. OpenSearch also has the concept of error
    -- monitoring during the last 10 minutes of an Observation Window. If
    -- transfer of all records fail in the last 10 minutes, the full load
    -- operation stops.
    fullLoadErrorPercentage :: Prelude.Maybe Prelude.Int,
    -- | Set this option to @true@ for DMS to migrate documentation using the
    -- documentation type @_doc@. OpenSearch and an Elasticsearch cluster only
    -- support the _doc documentation type in versions 7. x and later. The
    -- default value is @false@.
    useNewMappingType :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) used by the service to access the IAM
    -- role. The role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Text,
    -- | The endpoint for the OpenSearch cluster. DMS uses HTTPS if a transport
    -- protocol (http\/https) is not specified.
    endpointUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorRetryDuration', 'elasticsearchSettings_errorRetryDuration' - The maximum number of seconds for which DMS retries failed API requests
-- to the OpenSearch cluster.
--
-- 'fullLoadErrorPercentage', 'elasticsearchSettings_fullLoadErrorPercentage' - The maximum percentage of records that can fail to be written before a
-- full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000
-- records are transferred. OpenSearch also has the concept of error
-- monitoring during the last 10 minutes of an Observation Window. If
-- transfer of all records fail in the last 10 minutes, the full load
-- operation stops.
--
-- 'useNewMappingType', 'elasticsearchSettings_useNewMappingType' - Set this option to @true@ for DMS to migrate documentation using the
-- documentation type @_doc@. OpenSearch and an Elasticsearch cluster only
-- support the _doc documentation type in versions 7. x and later. The
-- default value is @false@.
--
-- 'serviceAccessRoleArn', 'elasticsearchSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
--
-- 'endpointUri', 'elasticsearchSettings_endpointUri' - The endpoint for the OpenSearch cluster. DMS uses HTTPS if a transport
-- protocol (http\/https) is not specified.
newElasticsearchSettings ::
  -- | 'serviceAccessRoleArn'
  Prelude.Text ->
  -- | 'endpointUri'
  Prelude.Text ->
  ElasticsearchSettings
newElasticsearchSettings
  pServiceAccessRoleArn_
  pEndpointUri_ =
    ElasticsearchSettings'
      { errorRetryDuration =
          Prelude.Nothing,
        fullLoadErrorPercentage = Prelude.Nothing,
        useNewMappingType = Prelude.Nothing,
        serviceAccessRoleArn = pServiceAccessRoleArn_,
        endpointUri = pEndpointUri_
      }

-- | The maximum number of seconds for which DMS retries failed API requests
-- to the OpenSearch cluster.
elasticsearchSettings_errorRetryDuration :: Lens.Lens' ElasticsearchSettings (Prelude.Maybe Prelude.Int)
elasticsearchSettings_errorRetryDuration = Lens.lens (\ElasticsearchSettings' {errorRetryDuration} -> errorRetryDuration) (\s@ElasticsearchSettings' {} a -> s {errorRetryDuration = a} :: ElasticsearchSettings)

-- | The maximum percentage of records that can fail to be written before a
-- full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000
-- records are transferred. OpenSearch also has the concept of error
-- monitoring during the last 10 minutes of an Observation Window. If
-- transfer of all records fail in the last 10 minutes, the full load
-- operation stops.
elasticsearchSettings_fullLoadErrorPercentage :: Lens.Lens' ElasticsearchSettings (Prelude.Maybe Prelude.Int)
elasticsearchSettings_fullLoadErrorPercentage = Lens.lens (\ElasticsearchSettings' {fullLoadErrorPercentage} -> fullLoadErrorPercentage) (\s@ElasticsearchSettings' {} a -> s {fullLoadErrorPercentage = a} :: ElasticsearchSettings)

-- | Set this option to @true@ for DMS to migrate documentation using the
-- documentation type @_doc@. OpenSearch and an Elasticsearch cluster only
-- support the _doc documentation type in versions 7. x and later. The
-- default value is @false@.
elasticsearchSettings_useNewMappingType :: Lens.Lens' ElasticsearchSettings (Prelude.Maybe Prelude.Bool)
elasticsearchSettings_useNewMappingType = Lens.lens (\ElasticsearchSettings' {useNewMappingType} -> useNewMappingType) (\s@ElasticsearchSettings' {} a -> s {useNewMappingType = a} :: ElasticsearchSettings)

-- | The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
elasticsearchSettings_serviceAccessRoleArn :: Lens.Lens' ElasticsearchSettings Prelude.Text
elasticsearchSettings_serviceAccessRoleArn = Lens.lens (\ElasticsearchSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@ElasticsearchSettings' {} a -> s {serviceAccessRoleArn = a} :: ElasticsearchSettings)

-- | The endpoint for the OpenSearch cluster. DMS uses HTTPS if a transport
-- protocol (http\/https) is not specified.
elasticsearchSettings_endpointUri :: Lens.Lens' ElasticsearchSettings Prelude.Text
elasticsearchSettings_endpointUri = Lens.lens (\ElasticsearchSettings' {endpointUri} -> endpointUri) (\s@ElasticsearchSettings' {} a -> s {endpointUri = a} :: ElasticsearchSettings)

instance Data.FromJSON ElasticsearchSettings where
  parseJSON =
    Data.withObject
      "ElasticsearchSettings"
      ( \x ->
          ElasticsearchSettings'
            Prelude.<$> (x Data..:? "ErrorRetryDuration")
            Prelude.<*> (x Data..:? "FullLoadErrorPercentage")
            Prelude.<*> (x Data..:? "UseNewMappingType")
            Prelude.<*> (x Data..: "ServiceAccessRoleArn")
            Prelude.<*> (x Data..: "EndpointUri")
      )

instance Prelude.Hashable ElasticsearchSettings where
  hashWithSalt _salt ElasticsearchSettings' {..} =
    _salt
      `Prelude.hashWithSalt` errorRetryDuration
      `Prelude.hashWithSalt` fullLoadErrorPercentage
      `Prelude.hashWithSalt` useNewMappingType
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` endpointUri

instance Prelude.NFData ElasticsearchSettings where
  rnf ElasticsearchSettings' {..} =
    Prelude.rnf errorRetryDuration `Prelude.seq`
      Prelude.rnf fullLoadErrorPercentage `Prelude.seq`
        Prelude.rnf useNewMappingType `Prelude.seq`
          Prelude.rnf serviceAccessRoleArn `Prelude.seq`
            Prelude.rnf endpointUri

instance Data.ToJSON ElasticsearchSettings where
  toJSON ElasticsearchSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ErrorRetryDuration" Data..=)
              Prelude.<$> errorRetryDuration,
            ("FullLoadErrorPercentage" Data..=)
              Prelude.<$> fullLoadErrorPercentage,
            ("UseNewMappingType" Data..=)
              Prelude.<$> useNewMappingType,
            Prelude.Just
              ( "ServiceAccessRoleArn"
                  Data..= serviceAccessRoleArn
              ),
            Prelude.Just ("EndpointUri" Data..= endpointUri)
          ]
      )
