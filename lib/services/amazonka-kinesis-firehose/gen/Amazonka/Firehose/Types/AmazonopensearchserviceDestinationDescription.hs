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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceDestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationDescription
import Amazonka.Firehose.Types.VpcConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceDestinationDescription' smart constructor.
data AmazonopensearchserviceDestinationDescription = AmazonopensearchserviceDestinationDescription'
  { processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    roleARN :: Prelude.Maybe Prelude.Text,
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    domainARN :: Prelude.Maybe Prelude.Text,
    typeName :: Prelude.Maybe Prelude.Text,
    indexName :: Prelude.Maybe Prelude.Text,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription,
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'roleARN', 'amazonopensearchserviceDestinationDescription_roleARN' - Undocumented member.
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationDescription_bufferingHints' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationDescription_clusterEndpoint' - Undocumented member.
--
-- 'domainARN', 'amazonopensearchserviceDestinationDescription_domainARN' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationDescription_typeName' - Undocumented member.
--
-- 'indexName', 'amazonopensearchserviceDestinationDescription_indexName' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationDescription_s3BackupMode' - Undocumented member.
--
-- 'vpcConfigurationDescription', 'amazonopensearchserviceDestinationDescription_vpcConfigurationDescription' - Undocumented member.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationDescription_indexRotationPeriod' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationDescription_retryOptions' - Undocumented member.
--
-- 's3DestinationDescription', 'amazonopensearchserviceDestinationDescription_s3DestinationDescription' - Undocumented member.
newAmazonopensearchserviceDestinationDescription ::
  AmazonopensearchserviceDestinationDescription
newAmazonopensearchserviceDestinationDescription =
  AmazonopensearchserviceDestinationDescription'
    { processingConfiguration =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      bufferingHints =
        Prelude.Nothing,
      clusterEndpoint =
        Prelude.Nothing,
      domainARN = Prelude.Nothing,
      typeName = Prelude.Nothing,
      indexName = Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      s3BackupMode =
        Prelude.Nothing,
      vpcConfigurationDescription =
        Prelude.Nothing,
      indexRotationPeriod =
        Prelude.Nothing,
      retryOptions =
        Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing
    }

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationDescription_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_roleARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationDescription_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationDescription' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationDescription' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_domainARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_typeName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_typeName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_indexName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_indexName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationDescription_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe VpcConfigurationDescription)
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationDescription_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationDescription_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_s3DestinationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe S3DestinationDescription)
amazonopensearchserviceDestinationDescription_s3DestinationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3DestinationDescription = a} :: AmazonopensearchserviceDestinationDescription)

instance
  Data.FromJSON
    AmazonopensearchserviceDestinationDescription
  where
  parseJSON =
    Data.withObject
      "AmazonopensearchserviceDestinationDescription"
      ( \x ->
          AmazonopensearchserviceDestinationDescription'
            Prelude.<$> (x Data..:? "ProcessingConfiguration")
              Prelude.<*> (x Data..:? "RoleARN")
              Prelude.<*> (x Data..:? "BufferingHints")
              Prelude.<*> (x Data..:? "ClusterEndpoint")
              Prelude.<*> (x Data..:? "DomainARN")
              Prelude.<*> (x Data..:? "TypeName")
              Prelude.<*> (x Data..:? "IndexName")
              Prelude.<*> (x Data..:? "CloudWatchLoggingOptions")
              Prelude.<*> (x Data..:? "S3BackupMode")
              Prelude.<*> (x Data..:? "VpcConfigurationDescription")
              Prelude.<*> (x Data..:? "IndexRotationPeriod")
              Prelude.<*> (x Data..:? "RetryOptions")
              Prelude.<*> (x Data..:? "S3DestinationDescription")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationDescription
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` vpcConfigurationDescription
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3DestinationDescription

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationDescription
  where
  rnf
    AmazonopensearchserviceDestinationDescription' {..} =
      Prelude.rnf processingConfiguration
        `Prelude.seq` Prelude.rnf roleARN
        `Prelude.seq` Prelude.rnf bufferingHints
        `Prelude.seq` Prelude.rnf clusterEndpoint
        `Prelude.seq` Prelude.rnf domainARN
        `Prelude.seq` Prelude.rnf typeName
        `Prelude.seq` Prelude.rnf indexName
        `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
        `Prelude.seq` Prelude.rnf s3BackupMode
        `Prelude.seq` Prelude.rnf vpcConfigurationDescription
        `Prelude.seq` Prelude.rnf indexRotationPeriod
        `Prelude.seq` Prelude.rnf retryOptions
        `Prelude.seq` Prelude.rnf s3DestinationDescription
