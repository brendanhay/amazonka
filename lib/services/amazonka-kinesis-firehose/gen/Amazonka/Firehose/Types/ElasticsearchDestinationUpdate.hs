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
-- Module      : Amazonka.Firehose.Types.ElasticsearchDestinationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ElasticsearchDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ElasticsearchBufferingHints
import Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
import Amazonka.Firehose.Types.ElasticsearchRetryOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in Amazon ES.
--
-- /See:/ 'newElasticsearchDestinationUpdate' smart constructor.
data ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate'
  { -- | The buffering options. If no value is specified,
    -- @ElasticsearchBufferingHints@ object default values are used.
    bufferingHints :: Prelude.Maybe ElasticsearchBufferingHints,
    -- | The CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Specify either
    -- this @ClusterEndpoint@ or the @DomainARN@ field.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon ES domain. The IAM role must have permissions
    -- for @DescribeDomain@, @DescribeDomains@, and
    -- @DescribeDomainConfig@ after assuming the IAM role specified in
    -- @RoleARN@. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    --
    -- Specify either @ClusterEndpoint@ or @DomainARN@.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch index rotation period. Index rotation appends a
    -- timestamp to @IndexName@ to facilitate the expiration of old data. For
    -- more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
    -- Default value is @OneDay@.
    indexRotationPeriod :: Prelude.Maybe ElasticsearchIndexRotationPeriod,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon ES. The default value is 300 (5 minutes).
    retryOptions :: Prelude.Maybe ElasticsearchRetryOptions,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Amazon ES Configuration API and for
    -- indexing documents. For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 destination.
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    -- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
    -- one type per index. If you try to specify a new type for an existing
    -- index that already has another type, Kinesis Data Firehose returns an
    -- error during runtime.
    --
    -- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
    -- delivery stream, Kinesis Data Firehose still delivers data to
    -- Elasticsearch with the old index name and type name. If you want to
    -- update your delivery stream with a new index name, provide an empty
    -- string for @TypeName@.
    typeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'elasticsearchDestinationUpdate_bufferingHints' - The buffering options. If no value is specified,
-- @ElasticsearchBufferingHints@ object default values are used.
--
-- 'cloudWatchLoggingOptions', 'elasticsearchDestinationUpdate_cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- 'clusterEndpoint', 'elasticsearchDestinationUpdate_clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- 'domainARN', 'elasticsearchDestinationUpdate_domainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeDomain@, @DescribeDomains@, and
-- @DescribeDomainConfig@ after assuming the IAM role specified in
-- @RoleARN@. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
--
-- 'indexName', 'elasticsearchDestinationUpdate_indexName' - The Elasticsearch index name.
--
-- 'indexRotationPeriod', 'elasticsearchDestinationUpdate_indexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to @IndexName@ to facilitate the expiration of old data. For
-- more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- Default value is @OneDay@.
--
-- 'processingConfiguration', 'elasticsearchDestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'retryOptions', 'elasticsearchDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
--
-- 'roleARN', 'elasticsearchDestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon ES Configuration API and for
-- indexing documents. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 's3Update', 'elasticsearchDestinationUpdate_s3Update' - The Amazon S3 destination.
--
-- 'typeName', 'elasticsearchDestinationUpdate_typeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
-- delivery stream, Kinesis Data Firehose still delivers data to
-- Elasticsearch with the old index name and type name. If you want to
-- update your delivery stream with a new index name, provide an empty
-- string for @TypeName@.
newElasticsearchDestinationUpdate ::
  ElasticsearchDestinationUpdate
newElasticsearchDestinationUpdate =
  ElasticsearchDestinationUpdate'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      domainARN = Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexRotationPeriod = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3Update = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The buffering options. If no value is specified,
-- @ElasticsearchBufferingHints@ object default values are used.
elasticsearchDestinationUpdate_bufferingHints :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe ElasticsearchBufferingHints)
elasticsearchDestinationUpdate_bufferingHints = Lens.lens (\ElasticsearchDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@ElasticsearchDestinationUpdate' {} a -> s {bufferingHints = a} :: ElasticsearchDestinationUpdate)

-- | The CloudWatch logging options for your delivery stream.
elasticsearchDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
elasticsearchDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\ElasticsearchDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ElasticsearchDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationUpdate)

-- | The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
elasticsearchDestinationUpdate_clusterEndpoint :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe Prelude.Text)
elasticsearchDestinationUpdate_clusterEndpoint = Lens.lens (\ElasticsearchDestinationUpdate' {clusterEndpoint} -> clusterEndpoint) (\s@ElasticsearchDestinationUpdate' {} a -> s {clusterEndpoint = a} :: ElasticsearchDestinationUpdate)

-- | The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeDomain@, @DescribeDomains@, and
-- @DescribeDomainConfig@ after assuming the IAM role specified in
-- @RoleARN@. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
elasticsearchDestinationUpdate_domainARN :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe Prelude.Text)
elasticsearchDestinationUpdate_domainARN = Lens.lens (\ElasticsearchDestinationUpdate' {domainARN} -> domainARN) (\s@ElasticsearchDestinationUpdate' {} a -> s {domainARN = a} :: ElasticsearchDestinationUpdate)

-- | The Elasticsearch index name.
elasticsearchDestinationUpdate_indexName :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe Prelude.Text)
elasticsearchDestinationUpdate_indexName = Lens.lens (\ElasticsearchDestinationUpdate' {indexName} -> indexName) (\s@ElasticsearchDestinationUpdate' {} a -> s {indexName = a} :: ElasticsearchDestinationUpdate)

-- | The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to @IndexName@ to facilitate the expiration of old data. For
-- more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- Default value is @OneDay@.
elasticsearchDestinationUpdate_indexRotationPeriod :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe ElasticsearchIndexRotationPeriod)
elasticsearchDestinationUpdate_indexRotationPeriod = Lens.lens (\ElasticsearchDestinationUpdate' {indexRotationPeriod} -> indexRotationPeriod) (\s@ElasticsearchDestinationUpdate' {} a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationUpdate)

-- | The data processing configuration.
elasticsearchDestinationUpdate_processingConfiguration :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
elasticsearchDestinationUpdate_processingConfiguration = Lens.lens (\ElasticsearchDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@ElasticsearchDestinationUpdate' {} a -> s {processingConfiguration = a} :: ElasticsearchDestinationUpdate)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
elasticsearchDestinationUpdate_retryOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe ElasticsearchRetryOptions)
elasticsearchDestinationUpdate_retryOptions = Lens.lens (\ElasticsearchDestinationUpdate' {retryOptions} -> retryOptions) (\s@ElasticsearchDestinationUpdate' {} a -> s {retryOptions = a} :: ElasticsearchDestinationUpdate)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon ES Configuration API and for
-- indexing documents. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
elasticsearchDestinationUpdate_roleARN :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe Prelude.Text)
elasticsearchDestinationUpdate_roleARN = Lens.lens (\ElasticsearchDestinationUpdate' {roleARN} -> roleARN) (\s@ElasticsearchDestinationUpdate' {} a -> s {roleARN = a} :: ElasticsearchDestinationUpdate)

-- | The Amazon S3 destination.
elasticsearchDestinationUpdate_s3Update :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
elasticsearchDestinationUpdate_s3Update = Lens.lens (\ElasticsearchDestinationUpdate' {s3Update} -> s3Update) (\s@ElasticsearchDestinationUpdate' {} a -> s {s3Update = a} :: ElasticsearchDestinationUpdate)

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your
-- delivery stream, Kinesis Data Firehose still delivers data to
-- Elasticsearch with the old index name and type name. If you want to
-- update your delivery stream with a new index name, provide an empty
-- string for @TypeName@.
elasticsearchDestinationUpdate_typeName :: Lens.Lens' ElasticsearchDestinationUpdate (Prelude.Maybe Prelude.Text)
elasticsearchDestinationUpdate_typeName = Lens.lens (\ElasticsearchDestinationUpdate' {typeName} -> typeName) (\s@ElasticsearchDestinationUpdate' {} a -> s {typeName = a} :: ElasticsearchDestinationUpdate)

instance
  Prelude.Hashable
    ElasticsearchDestinationUpdate
  where
  hashWithSalt
    _salt
    ElasticsearchDestinationUpdate' {..} =
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
    ElasticsearchDestinationUpdate
  where
  rnf ElasticsearchDestinationUpdate' {..} =
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

instance Data.ToJSON ElasticsearchDestinationUpdate where
  toJSON ElasticsearchDestinationUpdate' {..} =
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
