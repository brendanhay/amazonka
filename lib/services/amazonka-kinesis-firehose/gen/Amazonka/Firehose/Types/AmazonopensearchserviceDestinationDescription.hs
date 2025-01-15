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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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

-- | The destination description in Amazon OpenSearch Service.
--
-- /See:/ 'newAmazonopensearchserviceDestinationDescription' smart constructor.
data AmazonopensearchserviceDestinationDescription = AmazonopensearchserviceDestinationDescription'
  { -- | The buffering options.
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Kinesis Data
    -- Firehose uses either this ClusterEndpoint or the DomainARN field to send
    -- data to Amazon OpenSearch Service.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon OpenSearch Service domain.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon OpenSearch Service index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon OpenSearch Service index rotation period
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The Amazon OpenSearch Service retry options.
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The Amazon OpenSearch Service type name. This applies to Elasticsearch
    -- 6.x and lower versions. For Elasticsearch 7.x and OpenSearch Service
    -- 1.x, there\'s no value for TypeName.
    typeName :: Prelude.Maybe Prelude.Text,
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription
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
-- 'bufferingHints', 'amazonopensearchserviceDestinationDescription_bufferingHints' - The buffering options.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationDescription_clusterEndpoint' - The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this ClusterEndpoint or the DomainARN field to send
-- data to Amazon OpenSearch Service.
--
-- 'domainARN', 'amazonopensearchserviceDestinationDescription_domainARN' - The ARN of the Amazon OpenSearch Service domain.
--
-- 'indexName', 'amazonopensearchserviceDestinationDescription_indexName' - The Amazon OpenSearch Service index name.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationDescription_indexRotationPeriod' - The Amazon OpenSearch Service index rotation period
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationDescription_retryOptions' - The Amazon OpenSearch Service retry options.
--
-- 'roleARN', 'amazonopensearchserviceDestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 's3DestinationDescription', 'amazonopensearchserviceDestinationDescription_s3DestinationDescription' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationDescription_typeName' - The Amazon OpenSearch Service type name. This applies to Elasticsearch
-- 6.x and lower versions. For Elasticsearch 7.x and OpenSearch Service
-- 1.x, there\'s no value for TypeName.
--
-- 'vpcConfigurationDescription', 'amazonopensearchserviceDestinationDescription_vpcConfigurationDescription' - Undocumented member.
newAmazonopensearchserviceDestinationDescription ::
  AmazonopensearchserviceDestinationDescription
newAmazonopensearchserviceDestinationDescription =
  AmazonopensearchserviceDestinationDescription'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      clusterEndpoint =
        Prelude.Nothing,
      domainARN = Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexRotationPeriod =
        Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      retryOptions =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3BackupMode =
        Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing,
      typeName = Prelude.Nothing,
      vpcConfigurationDescription =
        Prelude.Nothing
    }

-- | The buffering options.
amazonopensearchserviceDestinationDescription_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationDescription_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationDescription' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this ClusterEndpoint or the DomainARN field to send
-- data to Amazon OpenSearch Service.
amazonopensearchserviceDestinationDescription_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationDescription' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationDescription)

-- | The ARN of the Amazon OpenSearch Service domain.
amazonopensearchserviceDestinationDescription_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_domainARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon OpenSearch Service index name.
amazonopensearchserviceDestinationDescription_indexName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_indexName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon OpenSearch Service index rotation period
amazonopensearchserviceDestinationDescription_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationDescription_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationDescription' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationDescription_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon OpenSearch Service retry options.
amazonopensearchserviceDestinationDescription_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationDescription_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationDescription' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
amazonopensearchserviceDestinationDescription_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_roleARN = Lens.lens (\AmazonopensearchserviceDestinationDescription' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon S3 backup mode.
amazonopensearchserviceDestinationDescription_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationDescription_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_s3DestinationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe S3DestinationDescription)
amazonopensearchserviceDestinationDescription_s3DestinationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {s3DestinationDescription = a} :: AmazonopensearchserviceDestinationDescription)

-- | The Amazon OpenSearch Service type name. This applies to Elasticsearch
-- 6.x and lower versions. For Elasticsearch 7.x and OpenSearch Service
-- 1.x, there\'s no value for TypeName.
amazonopensearchserviceDestinationDescription_typeName :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationDescription_typeName = Lens.lens (\AmazonopensearchserviceDestinationDescription' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationDescription)

-- | Undocumented member.
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription :: Lens.Lens' AmazonopensearchserviceDestinationDescription (Prelude.Maybe VpcConfigurationDescription)
amazonopensearchserviceDestinationDescription_vpcConfigurationDescription = Lens.lens (\AmazonopensearchserviceDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@AmazonopensearchserviceDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: AmazonopensearchserviceDestinationDescription)

instance
  Data.FromJSON
    AmazonopensearchserviceDestinationDescription
  where
  parseJSON =
    Data.withObject
      "AmazonopensearchserviceDestinationDescription"
      ( \x ->
          AmazonopensearchserviceDestinationDescription'
            Prelude.<$> (x Data..:? "BufferingHints")
            Prelude.<*> (x Data..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Data..:? "ClusterEndpoint")
            Prelude.<*> (x Data..:? "DomainARN")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "IndexRotationPeriod")
            Prelude.<*> (x Data..:? "ProcessingConfiguration")
            Prelude.<*> (x Data..:? "RetryOptions")
            Prelude.<*> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "S3BackupMode")
            Prelude.<*> (x Data..:? "S3DestinationDescription")
            Prelude.<*> (x Data..:? "TypeName")
            Prelude.<*> (x Data..:? "VpcConfigurationDescription")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationDescription
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` s3DestinationDescription
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` vpcConfigurationDescription

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationDescription
  where
  rnf
    AmazonopensearchserviceDestinationDescription' {..} =
      Prelude.rnf bufferingHints `Prelude.seq`
        Prelude.rnf cloudWatchLoggingOptions `Prelude.seq`
          Prelude.rnf clusterEndpoint `Prelude.seq`
            Prelude.rnf domainARN `Prelude.seq`
              Prelude.rnf indexName `Prelude.seq`
                Prelude.rnf indexRotationPeriod `Prelude.seq`
                  Prelude.rnf processingConfiguration `Prelude.seq`
                    Prelude.rnf retryOptions `Prelude.seq`
                      Prelude.rnf roleARN `Prelude.seq`
                        Prelude.rnf s3BackupMode `Prelude.seq`
                          Prelude.rnf s3DestinationDescription `Prelude.seq`
                            Prelude.rnf typeName `Prelude.seq`
                              Prelude.rnf vpcConfigurationDescription
