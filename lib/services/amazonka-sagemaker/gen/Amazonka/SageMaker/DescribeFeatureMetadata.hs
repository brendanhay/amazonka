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
-- Module      : Amazonka.SageMaker.DescribeFeatureMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows the metadata for a feature within a feature group.
module Amazonka.SageMaker.DescribeFeatureMetadata
  ( -- * Creating a Request
    DescribeFeatureMetadata (..),
    newDescribeFeatureMetadata,

    -- * Request Lenses
    describeFeatureMetadata_featureGroupName,
    describeFeatureMetadata_featureName,

    -- * Destructuring the Response
    DescribeFeatureMetadataResponse (..),
    newDescribeFeatureMetadataResponse,

    -- * Response Lenses
    describeFeatureMetadataResponse_description,
    describeFeatureMetadataResponse_parameters,
    describeFeatureMetadataResponse_httpStatus,
    describeFeatureMetadataResponse_featureGroupArn,
    describeFeatureMetadataResponse_featureGroupName,
    describeFeatureMetadataResponse_featureName,
    describeFeatureMetadataResponse_featureType,
    describeFeatureMetadataResponse_creationTime,
    describeFeatureMetadataResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeFeatureMetadata' smart constructor.
data DescribeFeatureMetadata = DescribeFeatureMetadata'
  { -- | The name of the feature group containing the feature.
    featureGroupName :: Prelude.Text,
    -- | The name of the feature.
    featureName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupName', 'describeFeatureMetadata_featureGroupName' - The name of the feature group containing the feature.
--
-- 'featureName', 'describeFeatureMetadata_featureName' - The name of the feature.
newDescribeFeatureMetadata ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
  DescribeFeatureMetadata
newDescribeFeatureMetadata
  pFeatureGroupName_
  pFeatureName_ =
    DescribeFeatureMetadata'
      { featureGroupName =
          pFeatureGroupName_,
        featureName = pFeatureName_
      }

-- | The name of the feature group containing the feature.
describeFeatureMetadata_featureGroupName :: Lens.Lens' DescribeFeatureMetadata Prelude.Text
describeFeatureMetadata_featureGroupName = Lens.lens (\DescribeFeatureMetadata' {featureGroupName} -> featureGroupName) (\s@DescribeFeatureMetadata' {} a -> s {featureGroupName = a} :: DescribeFeatureMetadata)

-- | The name of the feature.
describeFeatureMetadata_featureName :: Lens.Lens' DescribeFeatureMetadata Prelude.Text
describeFeatureMetadata_featureName = Lens.lens (\DescribeFeatureMetadata' {featureName} -> featureName) (\s@DescribeFeatureMetadata' {} a -> s {featureName = a} :: DescribeFeatureMetadata)

instance Core.AWSRequest DescribeFeatureMetadata where
  type
    AWSResponse DescribeFeatureMetadata =
      DescribeFeatureMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeatureMetadataResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FeatureGroupArn")
            Prelude.<*> (x Data..:> "FeatureGroupName")
            Prelude.<*> (x Data..:> "FeatureName")
            Prelude.<*> (x Data..:> "FeatureType")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
      )

instance Prelude.Hashable DescribeFeatureMetadata where
  hashWithSalt _salt DescribeFeatureMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` featureName

instance Prelude.NFData DescribeFeatureMetadata where
  rnf DescribeFeatureMetadata' {..} =
    Prelude.rnf featureGroupName `Prelude.seq`
      Prelude.rnf featureName

instance Data.ToHeaders DescribeFeatureMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeFeatureMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFeatureMetadata where
  toJSON DescribeFeatureMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FeatureGroupName" Data..= featureGroupName),
            Prelude.Just ("FeatureName" Data..= featureName)
          ]
      )

instance Data.ToPath DescribeFeatureMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFeatureMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeatureMetadataResponse' smart constructor.
data DescribeFeatureMetadataResponse = DescribeFeatureMetadataResponse'
  { -- | The description you added to describe the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key-value pairs that you added to describe the feature.
    parameters :: Prelude.Maybe [FeatureParameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the feature group that contains the
    -- feature.
    featureGroupArn :: Prelude.Text,
    -- | The name of the feature group that you\'ve specified.
    featureGroupName :: Prelude.Text,
    -- | The name of the feature that you\'ve specified.
    featureName :: Prelude.Text,
    -- | The data type of the feature.
    featureType :: FeatureType,
    -- | A timestamp indicating when the feature was created.
    creationTime :: Data.POSIX,
    -- | A timestamp indicating when the metadata for the feature group was
    -- modified. For example, if you add a parameter describing the feature,
    -- the timestamp changes to reflect the last time you
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeFeatureMetadataResponse_description' - The description you added to describe the feature.
--
-- 'parameters', 'describeFeatureMetadataResponse_parameters' - The key-value pairs that you added to describe the feature.
--
-- 'httpStatus', 'describeFeatureMetadataResponse_httpStatus' - The response's http status code.
--
-- 'featureGroupArn', 'describeFeatureMetadataResponse_featureGroupArn' - The Amazon Resource Number (ARN) of the feature group that contains the
-- feature.
--
-- 'featureGroupName', 'describeFeatureMetadataResponse_featureGroupName' - The name of the feature group that you\'ve specified.
--
-- 'featureName', 'describeFeatureMetadataResponse_featureName' - The name of the feature that you\'ve specified.
--
-- 'featureType', 'describeFeatureMetadataResponse_featureType' - The data type of the feature.
--
-- 'creationTime', 'describeFeatureMetadataResponse_creationTime' - A timestamp indicating when the feature was created.
--
-- 'lastModifiedTime', 'describeFeatureMetadataResponse_lastModifiedTime' - A timestamp indicating when the metadata for the feature group was
-- modified. For example, if you add a parameter describing the feature,
-- the timestamp changes to reflect the last time you
newDescribeFeatureMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'featureGroupArn'
  Prelude.Text ->
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
  -- | 'featureType'
  FeatureType ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  DescribeFeatureMetadataResponse
newDescribeFeatureMetadataResponse
  pHttpStatus_
  pFeatureGroupArn_
  pFeatureGroupName_
  pFeatureName_
  pFeatureType_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeFeatureMetadataResponse'
      { description =
          Prelude.Nothing,
        parameters = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        featureGroupArn = pFeatureGroupArn_,
        featureGroupName = pFeatureGroupName_,
        featureName = pFeatureName_,
        featureType = pFeatureType_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The description you added to describe the feature.
describeFeatureMetadataResponse_description :: Lens.Lens' DescribeFeatureMetadataResponse (Prelude.Maybe Prelude.Text)
describeFeatureMetadataResponse_description = Lens.lens (\DescribeFeatureMetadataResponse' {description} -> description) (\s@DescribeFeatureMetadataResponse' {} a -> s {description = a} :: DescribeFeatureMetadataResponse)

-- | The key-value pairs that you added to describe the feature.
describeFeatureMetadataResponse_parameters :: Lens.Lens' DescribeFeatureMetadataResponse (Prelude.Maybe [FeatureParameter])
describeFeatureMetadataResponse_parameters = Lens.lens (\DescribeFeatureMetadataResponse' {parameters} -> parameters) (\s@DescribeFeatureMetadataResponse' {} a -> s {parameters = a} :: DescribeFeatureMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFeatureMetadataResponse_httpStatus :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.Int
describeFeatureMetadataResponse_httpStatus = Lens.lens (\DescribeFeatureMetadataResponse' {httpStatus} -> httpStatus) (\s@DescribeFeatureMetadataResponse' {} a -> s {httpStatus = a} :: DescribeFeatureMetadataResponse)

-- | The Amazon Resource Number (ARN) of the feature group that contains the
-- feature.
describeFeatureMetadataResponse_featureGroupArn :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.Text
describeFeatureMetadataResponse_featureGroupArn = Lens.lens (\DescribeFeatureMetadataResponse' {featureGroupArn} -> featureGroupArn) (\s@DescribeFeatureMetadataResponse' {} a -> s {featureGroupArn = a} :: DescribeFeatureMetadataResponse)

-- | The name of the feature group that you\'ve specified.
describeFeatureMetadataResponse_featureGroupName :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.Text
describeFeatureMetadataResponse_featureGroupName = Lens.lens (\DescribeFeatureMetadataResponse' {featureGroupName} -> featureGroupName) (\s@DescribeFeatureMetadataResponse' {} a -> s {featureGroupName = a} :: DescribeFeatureMetadataResponse)

-- | The name of the feature that you\'ve specified.
describeFeatureMetadataResponse_featureName :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.Text
describeFeatureMetadataResponse_featureName = Lens.lens (\DescribeFeatureMetadataResponse' {featureName} -> featureName) (\s@DescribeFeatureMetadataResponse' {} a -> s {featureName = a} :: DescribeFeatureMetadataResponse)

-- | The data type of the feature.
describeFeatureMetadataResponse_featureType :: Lens.Lens' DescribeFeatureMetadataResponse FeatureType
describeFeatureMetadataResponse_featureType = Lens.lens (\DescribeFeatureMetadataResponse' {featureType} -> featureType) (\s@DescribeFeatureMetadataResponse' {} a -> s {featureType = a} :: DescribeFeatureMetadataResponse)

-- | A timestamp indicating when the feature was created.
describeFeatureMetadataResponse_creationTime :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.UTCTime
describeFeatureMetadataResponse_creationTime = Lens.lens (\DescribeFeatureMetadataResponse' {creationTime} -> creationTime) (\s@DescribeFeatureMetadataResponse' {} a -> s {creationTime = a} :: DescribeFeatureMetadataResponse) Prelude.. Data._Time

-- | A timestamp indicating when the metadata for the feature group was
-- modified. For example, if you add a parameter describing the feature,
-- the timestamp changes to reflect the last time you
describeFeatureMetadataResponse_lastModifiedTime :: Lens.Lens' DescribeFeatureMetadataResponse Prelude.UTCTime
describeFeatureMetadataResponse_lastModifiedTime = Lens.lens (\DescribeFeatureMetadataResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeFeatureMetadataResponse' {} a -> s {lastModifiedTime = a} :: DescribeFeatureMetadataResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    DescribeFeatureMetadataResponse
  where
  rnf DescribeFeatureMetadataResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf parameters `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf featureGroupArn `Prelude.seq`
            Prelude.rnf featureGroupName `Prelude.seq`
              Prelude.rnf featureName `Prelude.seq`
                Prelude.rnf featureType `Prelude.seq`
                  Prelude.rnf creationTime `Prelude.seq`
                    Prelude.rnf lastModifiedTime
