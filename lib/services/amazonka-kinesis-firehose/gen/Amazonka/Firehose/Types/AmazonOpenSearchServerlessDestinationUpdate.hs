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
-- Module      : Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessBufferingHints
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in the Serverless offering for
-- Amazon OpenSearch Service.
--
-- /See:/ 'newAmazonOpenSearchServerlessDestinationUpdate' smart constructor.
data AmazonOpenSearchServerlessDestinationUpdate = AmazonOpenSearchServerlessDestinationUpdate'
  { -- | The buffering options. If no value is specified,
    -- AmazonopensearchBufferingHints object default values are used.
    bufferingHints :: Prelude.Maybe AmazonOpenSearchServerlessBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the collection in the
    -- Serverless offering for Amazon OpenSearch Service.
    collectionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Serverless offering for Amazon OpenSearch Service index name.
    indexName :: Prelude.Maybe Prelude.Text,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to the Serverless offering for Amazon OpenSearch Service. The
    -- default value is 300 (5 minutes).
    retryOptions :: Prelude.Maybe AmazonOpenSearchServerlessRetryOptions,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Serverless offering for Amazon OpenSearch
    -- Service Configuration API and for indexing documents.
    roleARN :: Prelude.Maybe Prelude.Text,
    s3Update :: Prelude.Maybe S3DestinationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchServerlessDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'amazonOpenSearchServerlessDestinationUpdate_bufferingHints' - The buffering options. If no value is specified,
-- AmazonopensearchBufferingHints object default values are used.
--
-- 'cloudWatchLoggingOptions', 'amazonOpenSearchServerlessDestinationUpdate_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'collectionEndpoint', 'amazonOpenSearchServerlessDestinationUpdate_collectionEndpoint' - The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
--
-- 'indexName', 'amazonOpenSearchServerlessDestinationUpdate_indexName' - The Serverless offering for Amazon OpenSearch Service index name.
--
-- 'processingConfiguration', 'amazonOpenSearchServerlessDestinationUpdate_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonOpenSearchServerlessDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to the Serverless offering for Amazon OpenSearch Service. The
-- default value is 300 (5 minutes).
--
-- 'roleARN', 'amazonOpenSearchServerlessDestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Serverless offering for Amazon OpenSearch
-- Service Configuration API and for indexing documents.
--
-- 's3Update', 'amazonOpenSearchServerlessDestinationUpdate_s3Update' - Undocumented member.
newAmazonOpenSearchServerlessDestinationUpdate ::
  AmazonOpenSearchServerlessDestinationUpdate
newAmazonOpenSearchServerlessDestinationUpdate =
  AmazonOpenSearchServerlessDestinationUpdate'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      collectionEndpoint =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3Update = Prelude.Nothing
    }

-- | The buffering options. If no value is specified,
-- AmazonopensearchBufferingHints object default values are used.
amazonOpenSearchServerlessDestinationUpdate_bufferingHints :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe AmazonOpenSearchServerlessBufferingHints)
amazonOpenSearchServerlessDestinationUpdate_bufferingHints = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {bufferingHints = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
amazonOpenSearchServerlessDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
amazonOpenSearchServerlessDestinationUpdate_collectionEndpoint :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationUpdate_collectionEndpoint = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {collectionEndpoint} -> collectionEndpoint) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {collectionEndpoint = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | The Serverless offering for Amazon OpenSearch Service index name.
amazonOpenSearchServerlessDestinationUpdate_indexName :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationUpdate_indexName = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {indexName} -> indexName) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {indexName = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationUpdate_processingConfiguration :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
amazonOpenSearchServerlessDestinationUpdate_processingConfiguration = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {processingConfiguration = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to the Serverless offering for Amazon OpenSearch Service. The
-- default value is 300 (5 minutes).
amazonOpenSearchServerlessDestinationUpdate_retryOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe AmazonOpenSearchServerlessRetryOptions)
amazonOpenSearchServerlessDestinationUpdate_retryOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {retryOptions} -> retryOptions) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {retryOptions = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Serverless offering for Amazon OpenSearch
-- Service Configuration API and for indexing documents.
amazonOpenSearchServerlessDestinationUpdate_roleARN :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationUpdate_roleARN = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {roleARN} -> roleARN) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {roleARN = a} :: AmazonOpenSearchServerlessDestinationUpdate)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationUpdate_s3Update :: Lens.Lens' AmazonOpenSearchServerlessDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
amazonOpenSearchServerlessDestinationUpdate_s3Update = Lens.lens (\AmazonOpenSearchServerlessDestinationUpdate' {s3Update} -> s3Update) (\s@AmazonOpenSearchServerlessDestinationUpdate' {} a -> s {s3Update = a} :: AmazonOpenSearchServerlessDestinationUpdate)

instance
  Prelude.Hashable
    AmazonOpenSearchServerlessDestinationUpdate
  where
  hashWithSalt
    _salt
    AmazonOpenSearchServerlessDestinationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` collectionEndpoint
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` s3Update

instance
  Prelude.NFData
    AmazonOpenSearchServerlessDestinationUpdate
  where
  rnf AmazonOpenSearchServerlessDestinationUpdate' {..} =
    Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf collectionEndpoint
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf s3Update

instance
  Data.ToJSON
    AmazonOpenSearchServerlessDestinationUpdate
  where
  toJSON
    AmazonOpenSearchServerlessDestinationUpdate' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BufferingHints" Data..=)
                Prelude.<$> bufferingHints,
              ("CloudWatchLoggingOptions" Data..=)
                Prelude.<$> cloudWatchLoggingOptions,
              ("CollectionEndpoint" Data..=)
                Prelude.<$> collectionEndpoint,
              ("IndexName" Data..=) Prelude.<$> indexName,
              ("ProcessingConfiguration" Data..=)
                Prelude.<$> processingConfiguration,
              ("RetryOptions" Data..=) Prelude.<$> retryOptions,
              ("RoleARN" Data..=) Prelude.<$> roleARN,
              ("S3Update" Data..=) Prelude.<$> s3Update
            ]
        )
