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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationConfiguration
import Amazonka.Firehose.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceDestinationConfiguration' smart constructor.
data AmazonopensearchserviceDestinationConfiguration = AmazonopensearchserviceDestinationConfiguration'
  { vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    domainARN :: Prelude.Maybe Prelude.Text,
    typeName :: Prelude.Maybe Prelude.Text,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
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
-- 'vpcConfiguration', 'amazonopensearchserviceDestinationConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationConfiguration_bufferingHints' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationConfiguration_clusterEndpoint' - Undocumented member.
--
-- 'domainARN', 'amazonopensearchserviceDestinationConfiguration_domainARN' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationConfiguration_typeName' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationConfiguration_s3BackupMode' - Undocumented member.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationConfiguration_indexRotationPeriod' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationConfiguration_retryOptions' - Undocumented member.
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
      { vpcConfiguration =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        bufferingHints =
          Prelude.Nothing,
        clusterEndpoint =
          Prelude.Nothing,
        domainARN =
          Prelude.Nothing,
        typeName = Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        s3BackupMode =
          Prelude.Nothing,
        indexRotationPeriod =
          Prelude.Nothing,
        retryOptions =
          Prelude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration =
          pS3Configuration_
      }

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_vpcConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe VpcConfiguration)
amazonopensearchserviceDestinationConfiguration_vpcConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationConfiguration_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationConfiguration_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_domainARN = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_typeName :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_typeName = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationConfiguration_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationConfiguration_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

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
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` vpcConfiguration
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationConfiguration
  where
  rnf
    AmazonopensearchserviceDestinationConfiguration' {..} =
      Prelude.rnf vpcConfiguration
        `Prelude.seq` Prelude.rnf processingConfiguration
        `Prelude.seq` Prelude.rnf bufferingHints
        `Prelude.seq` Prelude.rnf clusterEndpoint
        `Prelude.seq` Prelude.rnf domainARN
        `Prelude.seq` Prelude.rnf typeName
        `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
        `Prelude.seq` Prelude.rnf s3BackupMode
        `Prelude.seq` Prelude.rnf indexRotationPeriod
        `Prelude.seq` Prelude.rnf retryOptions
        `Prelude.seq` Prelude.rnf roleARN
        `Prelude.seq` Prelude.rnf indexName
        `Prelude.seq` Prelude.rnf s3Configuration

instance
  Core.ToJSON
    AmazonopensearchserviceDestinationConfiguration
  where
  toJSON
    AmazonopensearchserviceDestinationConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("VpcConfiguration" Core..=)
                Prelude.<$> vpcConfiguration,
              ("ProcessingConfiguration" Core..=)
                Prelude.<$> processingConfiguration,
              ("BufferingHints" Core..=)
                Prelude.<$> bufferingHints,
              ("ClusterEndpoint" Core..=)
                Prelude.<$> clusterEndpoint,
              ("DomainARN" Core..=) Prelude.<$> domainARN,
              ("TypeName" Core..=) Prelude.<$> typeName,
              ("CloudWatchLoggingOptions" Core..=)
                Prelude.<$> cloudWatchLoggingOptions,
              ("S3BackupMode" Core..=) Prelude.<$> s3BackupMode,
              ("IndexRotationPeriod" Core..=)
                Prelude.<$> indexRotationPeriod,
              ("RetryOptions" Core..=) Prelude.<$> retryOptions,
              Prelude.Just ("RoleARN" Core..= roleARN),
              Prelude.Just ("IndexName" Core..= indexName),
              Prelude.Just
                ("S3Configuration" Core..= s3Configuration)
            ]
        )
