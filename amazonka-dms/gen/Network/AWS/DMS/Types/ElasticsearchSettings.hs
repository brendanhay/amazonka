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
-- Module      : Network.AWS.DMS.Types.ElasticsearchSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ElasticsearchSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines an Elasticsearch endpoint.
--
-- /See:/ 'newElasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { -- | The maximum number of seconds for which DMS retries failed API requests
    -- to the Elasticsearch cluster.
    errorRetryDuration :: Prelude.Maybe Prelude.Int,
    -- | The maximum percentage of records that can fail to be written before a
    -- full load operation stops.
    --
    -- To avoid early failure, this counter is only effective after 1000
    -- records are transferred. Elasticsearch also has the concept of error
    -- monitoring during the last 10 minutes of an Observation Window. If
    -- transfer of all records fail in the last 10 minutes, the full load
    -- operation stops.
    fullLoadErrorPercentage :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) used by service to access the IAM role.
    serviceAccessRoleArn :: Prelude.Text,
    -- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a
    -- transport protocol (http\/https) is not specified.
    endpointUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorRetryDuration', 'elasticsearchSettings_errorRetryDuration' - The maximum number of seconds for which DMS retries failed API requests
-- to the Elasticsearch cluster.
--
-- 'fullLoadErrorPercentage', 'elasticsearchSettings_fullLoadErrorPercentage' - The maximum percentage of records that can fail to be written before a
-- full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000
-- records are transferred. Elasticsearch also has the concept of error
-- monitoring during the last 10 minutes of an Observation Window. If
-- transfer of all records fail in the last 10 minutes, the full load
-- operation stops.
--
-- 'serviceAccessRoleArn', 'elasticsearchSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by service to access the IAM role.
--
-- 'endpointUri', 'elasticsearchSettings_endpointUri' - The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a
-- transport protocol (http\/https) is not specified.
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
        serviceAccessRoleArn = pServiceAccessRoleArn_,
        endpointUri = pEndpointUri_
      }

-- | The maximum number of seconds for which DMS retries failed API requests
-- to the Elasticsearch cluster.
elasticsearchSettings_errorRetryDuration :: Lens.Lens' ElasticsearchSettings (Prelude.Maybe Prelude.Int)
elasticsearchSettings_errorRetryDuration = Lens.lens (\ElasticsearchSettings' {errorRetryDuration} -> errorRetryDuration) (\s@ElasticsearchSettings' {} a -> s {errorRetryDuration = a} :: ElasticsearchSettings)

-- | The maximum percentage of records that can fail to be written before a
-- full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000
-- records are transferred. Elasticsearch also has the concept of error
-- monitoring during the last 10 minutes of an Observation Window. If
-- transfer of all records fail in the last 10 minutes, the full load
-- operation stops.
elasticsearchSettings_fullLoadErrorPercentage :: Lens.Lens' ElasticsearchSettings (Prelude.Maybe Prelude.Int)
elasticsearchSettings_fullLoadErrorPercentage = Lens.lens (\ElasticsearchSettings' {fullLoadErrorPercentage} -> fullLoadErrorPercentage) (\s@ElasticsearchSettings' {} a -> s {fullLoadErrorPercentage = a} :: ElasticsearchSettings)

-- | The Amazon Resource Name (ARN) used by service to access the IAM role.
elasticsearchSettings_serviceAccessRoleArn :: Lens.Lens' ElasticsearchSettings Prelude.Text
elasticsearchSettings_serviceAccessRoleArn = Lens.lens (\ElasticsearchSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@ElasticsearchSettings' {} a -> s {serviceAccessRoleArn = a} :: ElasticsearchSettings)

-- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a
-- transport protocol (http\/https) is not specified.
elasticsearchSettings_endpointUri :: Lens.Lens' ElasticsearchSettings Prelude.Text
elasticsearchSettings_endpointUri = Lens.lens (\ElasticsearchSettings' {endpointUri} -> endpointUri) (\s@ElasticsearchSettings' {} a -> s {endpointUri = a} :: ElasticsearchSettings)

instance Prelude.FromJSON ElasticsearchSettings where
  parseJSON =
    Prelude.withObject
      "ElasticsearchSettings"
      ( \x ->
          ElasticsearchSettings'
            Prelude.<$> (x Prelude..:? "ErrorRetryDuration")
            Prelude.<*> (x Prelude..:? "FullLoadErrorPercentage")
            Prelude.<*> (x Prelude..: "ServiceAccessRoleArn")
            Prelude.<*> (x Prelude..: "EndpointUri")
      )

instance Prelude.Hashable ElasticsearchSettings

instance Prelude.NFData ElasticsearchSettings

instance Prelude.ToJSON ElasticsearchSettings where
  toJSON ElasticsearchSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ErrorRetryDuration" Prelude..=)
              Prelude.<$> errorRetryDuration,
            ("FullLoadErrorPercentage" Prelude..=)
              Prelude.<$> fullLoadErrorPercentage,
            Prelude.Just
              ( "ServiceAccessRoleArn"
                  Prelude..= serviceAccessRoleArn
              ),
            Prelude.Just ("EndpointUri" Prelude..= endpointUri)
          ]
      )
