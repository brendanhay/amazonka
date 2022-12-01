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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceDestinationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceDestinationUpdate' smart constructor.
data AmazonopensearchserviceDestinationUpdate = AmazonopensearchserviceDestinationUpdate'
  { processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    roleARN :: Prelude.Maybe Prelude.Text,
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    domainARN :: Prelude.Maybe Prelude.Text,
    typeName :: Prelude.Maybe Prelude.Text,
    indexName :: Prelude.Maybe Prelude.Text,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationUpdate_processingConfiguration' - Undocumented member.
--
-- 'roleARN', 'amazonopensearchserviceDestinationUpdate_roleARN' - Undocumented member.
--
-- 's3Update', 'amazonopensearchserviceDestinationUpdate_s3Update' - Undocumented member.
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationUpdate_bufferingHints' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationUpdate_clusterEndpoint' - Undocumented member.
--
-- 'domainARN', 'amazonopensearchserviceDestinationUpdate_domainARN' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationUpdate_typeName' - Undocumented member.
--
-- 'indexName', 'amazonopensearchserviceDestinationUpdate_indexName' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationUpdate_indexRotationPeriod' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationUpdate_retryOptions' - Undocumented member.
newAmazonopensearchserviceDestinationUpdate ::
  AmazonopensearchserviceDestinationUpdate
newAmazonopensearchserviceDestinationUpdate =
  AmazonopensearchserviceDestinationUpdate'
    { processingConfiguration =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3Update = Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      domainARN = Prelude.Nothing,
      typeName = Prelude.Nothing,
      indexName = Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      indexRotationPeriod =
        Prelude.Nothing,
      retryOptions = Prelude.Nothing
    }

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationUpdate_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_roleARN = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_s3Update :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
amazonopensearchserviceDestinationUpdate_s3Update = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {s3Update} -> s3Update) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {s3Update = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationUpdate_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_domainARN = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_typeName :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_typeName = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_indexName :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_indexName = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationUpdate_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationUpdate_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationUpdate)

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationUpdate
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` s3Update
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` retryOptions

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationUpdate
  where
  rnf AmazonopensearchserviceDestinationUpdate' {..} =
    Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf s3Update
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf clusterEndpoint
      `Prelude.seq` Prelude.rnf domainARN
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf indexRotationPeriod
      `Prelude.seq` Prelude.rnf retryOptions

instance
  Core.ToJSON
    AmazonopensearchserviceDestinationUpdate
  where
  toJSON AmazonopensearchserviceDestinationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProcessingConfiguration" Core..=)
              Prelude.<$> processingConfiguration,
            ("RoleARN" Core..=) Prelude.<$> roleARN,
            ("S3Update" Core..=) Prelude.<$> s3Update,
            ("BufferingHints" Core..=)
              Prelude.<$> bufferingHints,
            ("ClusterEndpoint" Core..=)
              Prelude.<$> clusterEndpoint,
            ("DomainARN" Core..=) Prelude.<$> domainARN,
            ("TypeName" Core..=) Prelude.<$> typeName,
            ("IndexName" Core..=) Prelude.<$> indexName,
            ("CloudWatchLoggingOptions" Core..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("IndexRotationPeriod" Core..=)
              Prelude.<$> indexRotationPeriod,
            ("RetryOptions" Core..=) Prelude.<$> retryOptions
          ]
      )
