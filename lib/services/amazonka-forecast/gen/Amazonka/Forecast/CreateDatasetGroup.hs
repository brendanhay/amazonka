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
-- Module      : Amazonka.Forecast.CreateDatasetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset group, which holds a collection of related datasets.
-- You can add datasets to the dataset group when you create the dataset
-- group, or later by using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation.
--
-- After creating a dataset group and adding datasets, you use the dataset
-- group when you create a predictor. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Dataset groups>.
--
-- To get a list of all your datasets groups, use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_ListDatasetGroups.html ListDatasetGroups>
-- operation.
--
-- The @Status@ of a dataset group must be @ACTIVE@ before you can use the
-- dataset group to create a predictor. To get the status, use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
-- operation.
module Amazonka.Forecast.CreateDatasetGroup
  ( -- * Creating a Request
    CreateDatasetGroup (..),
    newCreateDatasetGroup,

    -- * Request Lenses
    createDatasetGroup_tags,
    createDatasetGroup_datasetArns,
    createDatasetGroup_datasetGroupName,
    createDatasetGroup_domain,

    -- * Destructuring the Response
    CreateDatasetGroupResponse (..),
    newCreateDatasetGroupResponse,

    -- * Response Lenses
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetGroup' smart constructor.
data CreateDatasetGroup = CreateDatasetGroup'
  { -- | The optional metadata that you apply to the dataset group to help you
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
    -- | An array of Amazon Resource Names (ARNs) of the datasets that you want
    -- to include in the dataset group.
    datasetArns :: Prelude.Maybe [Prelude.Text],
    -- | A name for the dataset group.
    datasetGroupName :: Prelude.Text,
    -- | The domain associated with the dataset group. When you add a dataset to
    -- a dataset group, this value and the value specified for the @Domain@
    -- parameter of the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
    -- operation must match.
    --
    -- The @Domain@ and @DatasetType@ that you choose determine the fields that
    -- must be present in training data that you import to a dataset. For
    -- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
    -- the @DatasetType@, Amazon Forecast requires that @item_id@, @timestamp@,
    -- and @demand@ fields are present in your data. For more information, see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Dataset groups>.
    domain :: Domain
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDatasetGroup_tags' - The optional metadata that you apply to the dataset group to help you
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
-- 'datasetArns', 'createDatasetGroup_datasetArns' - An array of Amazon Resource Names (ARNs) of the datasets that you want
-- to include in the dataset group.
--
-- 'datasetGroupName', 'createDatasetGroup_datasetGroupName' - A name for the dataset group.
--
-- 'domain', 'createDatasetGroup_domain' - The domain associated with the dataset group. When you add a dataset to
-- a dataset group, this value and the value specified for the @Domain@
-- parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation must match.
--
-- The @Domain@ and @DatasetType@ that you choose determine the fields that
-- must be present in training data that you import to a dataset. For
-- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
-- the @DatasetType@, Amazon Forecast requires that @item_id@, @timestamp@,
-- and @demand@ fields are present in your data. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Dataset groups>.
newCreateDatasetGroup ::
  -- | 'datasetGroupName'
  Prelude.Text ->
  -- | 'domain'
  Domain ->
  CreateDatasetGroup
newCreateDatasetGroup pDatasetGroupName_ pDomain_ =
  CreateDatasetGroup'
    { tags = Prelude.Nothing,
      datasetArns = Prelude.Nothing,
      datasetGroupName = pDatasetGroupName_,
      domain = pDomain_
    }

-- | The optional metadata that you apply to the dataset group to help you
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
createDatasetGroup_tags :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe [Tag])
createDatasetGroup_tags = Lens.lens (\CreateDatasetGroup' {tags} -> tags) (\s@CreateDatasetGroup' {} a -> s {tags = a} :: CreateDatasetGroup) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Resource Names (ARNs) of the datasets that you want
-- to include in the dataset group.
createDatasetGroup_datasetArns :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe [Prelude.Text])
createDatasetGroup_datasetArns = Lens.lens (\CreateDatasetGroup' {datasetArns} -> datasetArns) (\s@CreateDatasetGroup' {} a -> s {datasetArns = a} :: CreateDatasetGroup) Prelude.. Lens.mapping Lens.coerced

-- | A name for the dataset group.
createDatasetGroup_datasetGroupName :: Lens.Lens' CreateDatasetGroup Prelude.Text
createDatasetGroup_datasetGroupName = Lens.lens (\CreateDatasetGroup' {datasetGroupName} -> datasetGroupName) (\s@CreateDatasetGroup' {} a -> s {datasetGroupName = a} :: CreateDatasetGroup)

-- | The domain associated with the dataset group. When you add a dataset to
-- a dataset group, this value and the value specified for the @Domain@
-- parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation must match.
--
-- The @Domain@ and @DatasetType@ that you choose determine the fields that
-- must be present in training data that you import to a dataset. For
-- example, if you choose the @RETAIL@ domain and @TARGET_TIME_SERIES@ as
-- the @DatasetType@, Amazon Forecast requires that @item_id@, @timestamp@,
-- and @demand@ fields are present in your data. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/howitworks-datasets-groups.html Dataset groups>.
createDatasetGroup_domain :: Lens.Lens' CreateDatasetGroup Domain
createDatasetGroup_domain = Lens.lens (\CreateDatasetGroup' {domain} -> domain) (\s@CreateDatasetGroup' {} a -> s {domain = a} :: CreateDatasetGroup)

instance Core.AWSRequest CreateDatasetGroup where
  type
    AWSResponse CreateDatasetGroup =
      CreateDatasetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetGroupResponse'
            Prelude.<$> (x Core..?> "DatasetGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetGroup where
  hashWithSalt _salt CreateDatasetGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` datasetArns
      `Prelude.hashWithSalt` datasetGroupName
      `Prelude.hashWithSalt` domain

instance Prelude.NFData CreateDatasetGroup where
  rnf CreateDatasetGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf datasetArns
      `Prelude.seq` Prelude.rnf datasetGroupName
      `Prelude.seq` Prelude.rnf domain

instance Core.ToHeaders CreateDatasetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.CreateDatasetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDatasetGroup where
  toJSON CreateDatasetGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("DatasetArns" Core..=) Prelude.<$> datasetArns,
            Prelude.Just
              ("DatasetGroupName" Core..= datasetGroupName),
            Prelude.Just ("Domain" Core..= domain)
          ]
      )

instance Core.ToPath CreateDatasetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDatasetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetGroupResponse' smart constructor.
data CreateDatasetGroupResponse = CreateDatasetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'createDatasetGroupResponse_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'httpStatus', 'createDatasetGroupResponse_httpStatus' - The response's http status code.
newCreateDatasetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetGroupResponse
newCreateDatasetGroupResponse pHttpStatus_ =
  CreateDatasetGroupResponse'
    { datasetGroupArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset group.
createDatasetGroupResponse_datasetGroupArn :: Lens.Lens' CreateDatasetGroupResponse (Prelude.Maybe Prelude.Text)
createDatasetGroupResponse_datasetGroupArn = Lens.lens (\CreateDatasetGroupResponse' {datasetGroupArn} -> datasetGroupArn) (\s@CreateDatasetGroupResponse' {} a -> s {datasetGroupArn = a} :: CreateDatasetGroupResponse)

-- | The response's http status code.
createDatasetGroupResponse_httpStatus :: Lens.Lens' CreateDatasetGroupResponse Prelude.Int
createDatasetGroupResponse_httpStatus = Lens.lens (\CreateDatasetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetGroupResponse' {} a -> s {httpStatus = a} :: CreateDatasetGroupResponse)

instance Prelude.NFData CreateDatasetGroupResponse where
  rnf CreateDatasetGroupResponse' {..} =
    Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
