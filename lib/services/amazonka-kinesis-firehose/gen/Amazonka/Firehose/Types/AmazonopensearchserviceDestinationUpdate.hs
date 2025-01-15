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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in Amazon OpenSearch Service.
--
-- /See:/ 'newAmazonopensearchserviceDestinationUpdate' smart constructor.
data AmazonopensearchserviceDestinationUpdate = AmazonopensearchserviceDestinationUpdate'
  { -- | The buffering options. If no value is specified,
    -- AmazonopensearchBufferingHints object default values are used.
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Specify either
    -- this ClusterEndpoint or the DomainARN field.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon OpenSearch Service domain. The IAM role must have
    -- permissions for DescribeDomain, DescribeDomains, and
    -- DescribeDomainConfig after assuming the IAM role specified in RoleARN.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon OpenSearch Service index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon OpenSearch Service index rotation period. Index rotation
    -- appends a timestamp to IndexName to facilitate the expiration of old
    -- data.
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon OpenSearch Service. The default value is 300 (5
    -- minutes).
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Amazon OpenSearch Service Configuration
    -- API and for indexing documents.
    roleARN :: Prelude.Maybe Prelude.Text,
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    -- | The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
    -- can be only one type per index. If you try to specify a new type for an
    -- existing index that already has another type, Kinesis Data Firehose
    -- returns an error during runtime.
    --
    -- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
    -- delivery stream, Kinesis Data Firehose still delivers data to
    -- Elasticsearch with the old index name and type name. If you want to
    -- update your delivery stream with a new index name, provide an empty
    -- string for TypeName.
    typeName :: Prelude.Maybe Prelude.Text
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
-- 'bufferingHints', 'amazonopensearchserviceDestinationUpdate_bufferingHints' - The buffering options. If no value is specified,
-- AmazonopensearchBufferingHints object default values are used.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationUpdate_clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either
-- this ClusterEndpoint or the DomainARN field.
--
-- 'domainARN', 'amazonopensearchserviceDestinationUpdate_domainARN' - The ARN of the Amazon OpenSearch Service domain. The IAM role must have
-- permissions for DescribeDomain, DescribeDomains, and
-- DescribeDomainConfig after assuming the IAM role specified in RoleARN.
--
-- 'indexName', 'amazonopensearchserviceDestinationUpdate_indexName' - The Amazon OpenSearch Service index name.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationUpdate_indexRotationPeriod' - The Amazon OpenSearch Service index rotation period. Index rotation
-- appends a timestamp to IndexName to facilitate the expiration of old
-- data.
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationUpdate_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon OpenSearch Service. The default value is 300 (5
-- minutes).
--
-- 'roleARN', 'amazonopensearchserviceDestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon OpenSearch Service Configuration
-- API and for indexing documents.
--
-- 's3Update', 'amazonopensearchserviceDestinationUpdate_s3Update' - Undocumented member.
--
-- 'typeName', 'amazonopensearchserviceDestinationUpdate_typeName' - The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
-- can be only one type per index. If you try to specify a new type for an
-- existing index that already has another type, Kinesis Data Firehose
-- returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
-- delivery stream, Kinesis Data Firehose still delivers data to
-- Elasticsearch with the old index name and type name. If you want to
-- update your delivery stream with a new index name, provide an empty
-- string for TypeName.
newAmazonopensearchserviceDestinationUpdate ::
  AmazonopensearchserviceDestinationUpdate
newAmazonopensearchserviceDestinationUpdate =
  AmazonopensearchserviceDestinationUpdate'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      domainARN = Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexRotationPeriod =
        Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3Update = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The buffering options. If no value is specified,
-- AmazonopensearchBufferingHints object default values are used.
amazonopensearchserviceDestinationUpdate_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationUpdate_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The endpoint to use when communicating with the cluster. Specify either
-- this ClusterEndpoint or the DomainARN field.
amazonopensearchserviceDestinationUpdate_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The ARN of the Amazon OpenSearch Service domain. The IAM role must have
-- permissions for DescribeDomain, DescribeDomains, and
-- DescribeDomainConfig after assuming the IAM role specified in RoleARN.
amazonopensearchserviceDestinationUpdate_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_domainARN = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The Amazon OpenSearch Service index name.
amazonopensearchserviceDestinationUpdate_indexName :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_indexName = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The Amazon OpenSearch Service index rotation period. Index rotation
-- appends a timestamp to IndexName to facilitate the expiration of old
-- data.
amazonopensearchserviceDestinationUpdate_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationUpdate_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationUpdate_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon OpenSearch Service. The default value is 300 (5
-- minutes).
amazonopensearchserviceDestinationUpdate_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationUpdate_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon OpenSearch Service Configuration
-- API and for indexing documents.
amazonopensearchserviceDestinationUpdate_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_roleARN = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationUpdate)

-- | Undocumented member.
amazonopensearchserviceDestinationUpdate_s3Update :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
amazonopensearchserviceDestinationUpdate_s3Update = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {s3Update} -> s3Update) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {s3Update = a} :: AmazonopensearchserviceDestinationUpdate)

-- | The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
-- can be only one type per index. If you try to specify a new type for an
-- existing index that already has another type, Kinesis Data Firehose
-- returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
-- delivery stream, Kinesis Data Firehose still delivers data to
-- Elasticsearch with the old index name and type name. If you want to
-- update your delivery stream with a new index name, provide an empty
-- string for TypeName.
amazonopensearchserviceDestinationUpdate_typeName :: Lens.Lens' AmazonopensearchserviceDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationUpdate_typeName = Lens.lens (\AmazonopensearchserviceDestinationUpdate' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationUpdate' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationUpdate)

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationUpdate
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationUpdate' {..} =
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
        `Prelude.hashWithSalt` s3Update
        `Prelude.hashWithSalt` typeName

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationUpdate
  where
  rnf AmazonopensearchserviceDestinationUpdate' {..} =
    Prelude.rnf bufferingHints `Prelude.seq`
      Prelude.rnf cloudWatchLoggingOptions `Prelude.seq`
        Prelude.rnf clusterEndpoint `Prelude.seq`
          Prelude.rnf domainARN `Prelude.seq`
            Prelude.rnf indexName `Prelude.seq`
              Prelude.rnf indexRotationPeriod `Prelude.seq`
                Prelude.rnf processingConfiguration `Prelude.seq`
                  Prelude.rnf retryOptions `Prelude.seq`
                    Prelude.rnf roleARN `Prelude.seq`
                      Prelude.rnf s3Update `Prelude.seq`
                        Prelude.rnf typeName

instance
  Data.ToJSON
    AmazonopensearchserviceDestinationUpdate
  where
  toJSON AmazonopensearchserviceDestinationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BufferingHints" Data..=)
              Prelude.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("ClusterEndpoint" Data..=)
              Prelude.<$> clusterEndpoint,
            ("DomainARN" Data..=) Prelude.<$> domainARN,
            ("IndexName" Data..=) Prelude.<$> indexName,
            ("IndexRotationPeriod" Data..=)
              Prelude.<$> indexRotationPeriod,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("S3Update" Data..=) Prelude.<$> s3Update,
            ("TypeName" Data..=) Prelude.<$> typeName
          ]
      )
