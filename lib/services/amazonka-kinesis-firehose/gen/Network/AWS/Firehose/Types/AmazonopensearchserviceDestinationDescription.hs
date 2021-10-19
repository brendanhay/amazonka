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
-- Module      : Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.AmazonopensearchserviceBufferingHints
import Network.AWS.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Network.AWS.Firehose.Types.AmazonopensearchserviceRetryOptions
import Network.AWS.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.VpcConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceDestinationDescription' smart constructor.
data AmazonopensearchserviceDestinationDescription = AmazonopensearchserviceDestinationDescription'
  { indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    typeName :: Prelude.Maybe Prelude.Text,
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    domainARN :: Prelude.Maybe Prelude.Text,
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription,
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    roleARN :: Prelude.Maybe Prelude.Text,
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    indexName :: Prelude.Maybe Prelude.Text
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
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationDescription_indexRotationPeriod' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationDescription_typeName' - Undocumented member.
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationDescription_s3BackupMode' - Undocumented member.
--
-- 'domainARN', 'amazonopensearchserviceDestinationDescription_domainARN' - Undocumented member.
--
-- 'vpcConfigurationDescription', 'amazonopensearchserviceDestinationDescription_vpcConfigurationDescription' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 's3DestinationDescription', 'amazonopensearchserviceDestinationDescription_s3DestinationDescription' - Undocumented member.
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationDescription_bufferingHints' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationDescription_retryOptions' - Undocumented member.
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'roleARN', 'amazonopensearchserviceDestinationDescription_roleARN' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationDescription_clusterEndpoint' - Undocumented member.
--
-- 'indexName', 'amazonopensearchserviceDestinationDescription_indexName' - Undocumented member.
newAmazonopensearchserviceDestinationDescription ::
  AmazonopensearchserviceDestinationDescription
newAmazonopensearchserviceDestinationDescription =
  AmazonopensearchserviceDestinationDescription'
    { indexRotationPeriod =
        Prelude.Nothing,
      typeName = Prelude.Nothing,
      s3BackupMode =
        Prelude.Nothing,
      domainARN = Prelude.Nothing,
      vpcConfigurationDescription =
        Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing,
      bufferingHints =
        Prelude.Nothing,
      retryOptions =
        Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      clusterEndpoint =
        Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationDescription_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_typeName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_typeName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationDescription_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_domainARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe VpcConfigurationDescription)
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_s3DestinationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe S3DestinationDescription)
amazonopensearchserviceDestinationDescription_s3DestinationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3DestinationDescription = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationDescription_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationDescription' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationDescription_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationDescription_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_roleARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationDescription' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_indexName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_indexName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationDescription)

instance
  Core.FromJSON
    AmazonopensearchserviceDestinationDescription
  where
  parseJSON =
    Core.withObject
      "AmazonopensearchserviceDestinationDescription"
      ( \x ->
          AmazonopensearchserviceDestinationDescription'
            Prelude.<$> (x Core..:? "IndexRotationPeriod")
              Prelude.<*> (x Core..:? "TypeName")
              Prelude.<*> (x Core..:? "S3BackupMode")
              Prelude.<*> (x Core..:? "DomainARN")
              Prelude.<*> (x Core..:? "VpcConfigurationDescription")
              Prelude.<*> (x Core..:? "CloudWatchLoggingOptions")
              Prelude.<*> (x Core..:? "S3DestinationDescription")
              Prelude.<*> (x Core..:? "BufferingHints")
              Prelude.<*> (x Core..:? "RetryOptions")
              Prelude.<*> (x Core..:? "ProcessingConfiguration")
              Prelude.<*> (x Core..:? "RoleARN")
              Prelude.<*> (x Core..:? "ClusterEndpoint")
              Prelude.<*> (x Core..:? "IndexName")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationDescription

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationDescription
