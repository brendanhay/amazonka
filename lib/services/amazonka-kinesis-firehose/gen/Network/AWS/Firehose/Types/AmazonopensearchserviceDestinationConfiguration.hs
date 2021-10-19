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
-- Module      : Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.AmazonopensearchserviceBufferingHints
import Network.AWS.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Network.AWS.Firehose.Types.AmazonopensearchserviceRetryOptions
import Network.AWS.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.VpcConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceDestinationConfiguration' smart constructor.
data AmazonopensearchserviceDestinationConfiguration = AmazonopensearchserviceDestinationConfiguration'
  { indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    typeName :: Prelude.Maybe Prelude.Text,
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    domainARN :: Prelude.Maybe Prelude.Text,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    roleARN :: Prelude.Text,
    indexName :: Prelude.Text,
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationConfiguration_indexRotationPeriod' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationConfiguration_typeName' - Undocumented member.
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationConfiguration_s3BackupMode' - Undocumented member.
--
-- 'domainARN', 'amazonopensearchserviceDestinationConfiguration_domainARN' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'vpcConfiguration', 'amazonopensearchserviceDestinationConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationConfiguration_bufferingHints' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationConfiguration_retryOptions' - Undocumented member.
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationConfiguration_clusterEndpoint' - Undocumented member.
--
-- 'roleARN', 'amazonopensearchserviceDestinationConfiguration_roleARN' - Undocumented member.
--
-- 'indexName', 'amazonopensearchserviceDestinationConfiguration_indexName' - Undocumented member.
--
-- 's3Configuration', 'amazonopensearchserviceDestinationConfiguration_s3Configuration' - Undocumented member.
newAmazonopensearchserviceDestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  AmazonopensearchserviceDestinationConfiguration
newAmazonopensearchserviceDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    AmazonopensearchserviceDestinationConfiguration'
      { indexRotationPeriod =
          Prelude.Nothing,
        typeName = Prelude.Nothing,
        s3BackupMode =
          Prelude.Nothing,
        domainARN =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        vpcConfiguration =
          Prelude.Nothing,
        bufferingHints =
          Prelude.Nothing,
        retryOptions =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        clusterEndpoint =
          Prelude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration =
          pS3Configuration_
      }

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_typeName :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_typeName = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationConfiguration_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_domainARN = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_vpcConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe VpcConfiguration)
amazonopensearchserviceDestinationConfiguration_vpcConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationConfiguration_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationConfiguration_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationConfiguration_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration Prelude.Text
amazonopensearchserviceDestinationConfiguration_roleARN = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_indexName :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration Prelude.Text
amazonopensearchserviceDestinationConfiguration_indexName = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_s3Configuration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration S3DestinationConfiguration
amazonopensearchserviceDestinationConfiguration_s3Configuration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {s3Configuration = a} :: AmazonopensearchserviceDestinationConfiguration)

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationConfiguration

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationConfiguration

instance
  Core.ToJSON
    AmazonopensearchserviceDestinationConfiguration
  where
  toJSON
    AmazonopensearchserviceDestinationConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("IndexRotationPeriod" Core..=)
                Prelude.<$> indexRotationPeriod,
              ("TypeName" Core..=) Prelude.<$> typeName,
              ("S3BackupMode" Core..=) Prelude.<$> s3BackupMode,
              ("DomainARN" Core..=) Prelude.<$> domainARN,
              ("CloudWatchLoggingOptions" Core..=)
                Prelude.<$> cloudWatchLoggingOptions,
              ("VpcConfiguration" Core..=)
                Prelude.<$> vpcConfiguration,
              ("BufferingHints" Core..=)
                Prelude.<$> bufferingHints,
              ("RetryOptions" Core..=) Prelude.<$> retryOptions,
              ("ProcessingConfiguration" Core..=)
                Prelude.<$> processingConfiguration,
              ("ClusterEndpoint" Core..=)
                Prelude.<$> clusterEndpoint,
              Prelude.Just ("RoleARN" Core..= roleARN),
              Prelude.Just ("IndexName" Core..= indexName),
              Prelude.Just
                ("S3Configuration" Core..= s3Configuration)
            ]
        )
