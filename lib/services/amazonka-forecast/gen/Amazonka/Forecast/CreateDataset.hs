{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.CreateDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Forecast dataset. The information about the dataset
-- that you provide helps Forecast understand how to consume the data for
-- model training. This includes the following:
--
-- -   /@DataFrequency@/ - How frequently your historical time-series data
--     is collected.
--
-- -   /@Domain@/ and /@DatasetType@/ - Each dataset has an associated
--     dataset domain and a type within the domain. Amazon Forecast
--     provides a list of predefined domains and types within each domain.
--     For each unique dataset domain and type within the domain, Amazon
--     Forecast requires your data to include a minimum set of predefined
--     fields.
--
-- -   /@Schema@/ - A schema specifies the fields in the dataset, including
--     the field name and data type.
--
-- After creating a dataset, you import your training data into it and add
-- the dataset to a dataset group. You use the dataset group to create a
-- predictor. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Importing datasets>.
--
-- To get a list of all your datasets, use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_ListDatasets.html ListDatasets>
-- operation.
--
-- For example Forecast datasets, see the
-- <https://github.com/aws-samples/amazon-forecast-samples Amazon Forecast Sample GitHub repository>.
--
-- The @Status@ of a dataset must be @ACTIVE@ before you can import
-- training data. Use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDataset.html DescribeDataset>
-- operation to get the status.
module Amazonka.Forecast.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_dataFrequency,
    createDataset_encryptionConfig,
    createDataset_tags,
    createDataset_datasetName,
    createDataset_domain,
    createDataset_datasetType,
    createDataset_schema,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | The frequency of data collection. This parameter is required for
    -- RELATED_TIME_SERIES datasets.
    --
    -- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
    -- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
    -- minutes), and 1min (1 minute). For example, \"D\" indicates every day
    -- and \"15min\" indicates every 15 minutes.
    dataFrequency :: Prelude.Maybe Prelude.Text,
    -- | An AWS Key Management Service (KMS) key and the AWS Identity and Access
    -- Management (IAM) role that Amazon Forecast can assume to access the key.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | The optional metadata that you apply to the dataset to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50.
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8.
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for keys as it is reserved for AWS use. You cannot
    --     edit or delete tag keys with this prefix. Values can have this
    --     prefix. If a tag value has @aws@ as its prefix but the key does not,
    --     then Forecast considers it to be a user tag and will count against
    --     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
    --     count against your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the dataset.
    datasetName :: Prelude.Text,
    -- | The domain associated with the dataset. When you add a dataset to a
    -- dataset group, this value and the value specified for the @Domain@
    -- parameter of the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>
    -- operation must match.
    --
    -- The @Domain@ and @DatasetType@ that you choose determine the fields that
    -- must be present in the training data that you import to the dataset. For
    -- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
    -- the @DatasetType@, Amazon Forecast requires @item_id@, @timestamp@, and
    -- @demand@ fields to be present in your data. For more information, see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Importing datasets>.
    domain :: Domain,
    -- | The dataset type. Valid values depend on the chosen @Domain@.
    datasetType :: DatasetType,
    -- | The schema for the dataset. The schema attributes and their order must
    -- match the fields in your data. The dataset @Domain@ and @DatasetType@
    -- that you choose determine the minimum required fields in your training
    -- data. For information about the required fields for a specific dataset
    -- domain and type, see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-domains-ds-types.html Dataset Domains and Dataset Types>.
    schema :: Schema
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataFrequency', 'createDataset_dataFrequency' - The frequency of data collection. This parameter is required for
-- RELATED_TIME_SERIES datasets.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"D\" indicates every day
-- and \"15min\" indicates every 15 minutes.
--
-- 'encryptionConfig', 'createDataset_encryptionConfig' - An AWS Key Management Service (KMS) key and the AWS Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
--
-- 'tags', 'createDataset_tags' - The optional metadata that you apply to the dataset to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for AWS use. You cannot
--     edit or delete tag keys with this prefix. Values can have this
--     prefix. If a tag value has @aws@ as its prefix but the key does not,
--     then Forecast considers it to be a user tag and will count against
--     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
--     count against your tags per resource limit.
--
-- 'datasetName', 'createDataset_datasetName' - A name for the dataset.
--
-- 'domain', 'createDataset_domain' - The domain associated with the dataset. When you add a dataset to a
-- dataset group, this value and the value specified for the @Domain@
-- parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>
-- operation must match.
--
-- The @Domain@ and @DatasetType@ that you choose determine the fields that
-- must be present in the training data that you import to the dataset. For
-- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
-- the @DatasetType@, Amazon Forecast requires @item_id@, @timestamp@, and
-- @demand@ fields to be present in your data. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Importing datasets>.
--
-- 'datasetType', 'createDataset_datasetType' - The dataset type. Valid values depend on the chosen @Domain@.
--
-- 'schema', 'createDataset_schema' - The schema for the dataset. The schema attributes and their order must
-- match the fields in your data. The dataset @Domain@ and @DatasetType@
-- that you choose determine the minimum required fields in your training
-- data. For information about the required fields for a specific dataset
-- domain and type, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-domains-ds-types.html Dataset Domains and Dataset Types>.
newCreateDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'domain'
  Domain ->
  -- | 'datasetType'
  DatasetType ->
  -- | 'schema'
  Schema ->
  CreateDataset
newCreateDataset
  pDatasetName_
  pDomain_
  pDatasetType_
  pSchema_ =
    CreateDataset'
      { dataFrequency = Prelude.Nothing,
        encryptionConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        datasetName = pDatasetName_,
        domain = pDomain_,
        datasetType = pDatasetType_,
        schema = pSchema_
      }

-- | The frequency of data collection. This parameter is required for
-- RELATED_TIME_SERIES datasets.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"D\" indicates every day
-- and \"15min\" indicates every 15 minutes.
createDataset_dataFrequency :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_dataFrequency = Lens.lens (\CreateDataset' {dataFrequency} -> dataFrequency) (\s@CreateDataset' {} a -> s {dataFrequency = a} :: CreateDataset)

-- | An AWS Key Management Service (KMS) key and the AWS Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
createDataset_encryptionConfig :: Lens.Lens' CreateDataset (Prelude.Maybe EncryptionConfig)
createDataset_encryptionConfig = Lens.lens (\CreateDataset' {encryptionConfig} -> encryptionConfig) (\s@CreateDataset' {} a -> s {encryptionConfig = a} :: CreateDataset)

-- | The optional metadata that you apply to the dataset to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for AWS use. You cannot
--     edit or delete tag keys with this prefix. Values can have this
--     prefix. If a tag value has @aws@ as its prefix but the key does not,
--     then Forecast considers it to be a user tag and will count against
--     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
--     count against your tags per resource limit.
createDataset_tags :: Lens.Lens' CreateDataset (Prelude.Maybe [Tag])
createDataset_tags = Lens.lens (\CreateDataset' {tags} -> tags) (\s@CreateDataset' {} a -> s {tags = a} :: CreateDataset) Prelude.. Lens.mapping Lens.coerced

-- | A name for the dataset.
createDataset_datasetName :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetName = Lens.lens (\CreateDataset' {datasetName} -> datasetName) (\s@CreateDataset' {} a -> s {datasetName = a} :: CreateDataset)

-- | The domain associated with the dataset. When you add a dataset to a
-- dataset group, this value and the value specified for the @Domain@
-- parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>
-- operation must match.
--
-- The @Domain@ and @DatasetType@ that you choose determine the fields that
-- must be present in the training data that you import to the dataset. For
-- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
-- the @DatasetType@, Amazon Forecast requires @item_id@, @timestamp@, and
-- @demand@ fields to be present in your data. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Importing datasets>.
createDataset_domain :: Lens.Lens' CreateDataset Domain
createDataset_domain = Lens.lens (\CreateDataset' {domain} -> domain) (\s@CreateDataset' {} a -> s {domain = a} :: CreateDataset)

-- | The dataset type. Valid values depend on the chosen @Domain@.
createDataset_datasetType :: Lens.Lens' CreateDataset DatasetType
createDataset_datasetType = Lens.lens (\CreateDataset' {datasetType} -> datasetType) (\s@CreateDataset' {} a -> s {datasetType = a} :: CreateDataset)

-- | The schema for the dataset. The schema attributes and their order must
-- match the fields in your data. The dataset @Domain@ and @DatasetType@
-- that you choose determine the minimum required fields in your training
-- data. For information about the required fields for a specific dataset
-- domain and type, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-domains-ds-types.html Dataset Domains and Dataset Types>.
createDataset_schema :: Lens.Lens' CreateDataset Schema
createDataset_schema = Lens.lens (\CreateDataset' {schema} -> schema) (\s@CreateDataset' {} a -> s {schema = a} :: CreateDataset)

instance Core.AWSRequest CreateDataset where
  type
    AWSResponse CreateDataset =
      CreateDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset where
  hashWithSalt _salt CreateDataset' {..} =
    _salt
      `Prelude.hashWithSalt` dataFrequency
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` schema

instance Prelude.NFData CreateDataset where
  rnf CreateDataset' {..} =
    Prelude.rnf dataFrequency
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf schema

instance Data.ToHeaders CreateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataFrequency" Data..=) Prelude.<$> dataFrequency,
            ("EncryptionConfig" Data..=)
              Prelude.<$> encryptionConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("DatasetName" Data..= datasetName),
            Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("DatasetType" Data..= datasetType),
            Prelude.Just ("Schema" Data..= schema)
          ]
      )

instance Data.ToPath CreateDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'createDatasetResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
createDatasetResponse_datasetArn :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetArn = Lens.lens (\CreateDatasetResponse' {datasetArn} -> datasetArn) (\s@CreateDatasetResponse' {} a -> s {datasetArn = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse where
  rnf CreateDatasetResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf httpStatus
