{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a data set by applying a @queryAction@ (a SQL
-- query) or a @containerAction@ (executing a containerized application).
module Network.AWS.IoTAnalytics.CreateDatasetContent
  ( -- * Creating a Request
    CreateDatasetContent (..),
    newCreateDatasetContent,

    -- * Request Lenses
    createDatasetContent_versionId,
    createDatasetContent_datasetName,

    -- * Destructuring the Response
    CreateDatasetContentResponse (..),
    newCreateDatasetContentResponse,

    -- * Response Lenses
    createDatasetContentResponse_versionId,
    createDatasetContentResponse_httpStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDatasetContent' smart constructor.
data CreateDatasetContent = CreateDatasetContent'
  { -- | The version ID of the dataset content. To specify @versionId@ for a
    -- dataset content, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'createDatasetContent_versionId' - The version ID of the dataset content. To specify @versionId@ for a
-- dataset content, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'datasetName', 'createDatasetContent_datasetName' - The name of the dataset.
newCreateDatasetContent ::
  -- | 'datasetName'
  Prelude.Text ->
  CreateDatasetContent
newCreateDatasetContent pDatasetName_ =
  CreateDatasetContent'
    { versionId = Prelude.Nothing,
      datasetName = pDatasetName_
    }

-- | The version ID of the dataset content. To specify @versionId@ for a
-- dataset content, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
createDatasetContent_versionId :: Lens.Lens' CreateDatasetContent (Prelude.Maybe Prelude.Text)
createDatasetContent_versionId = Lens.lens (\CreateDatasetContent' {versionId} -> versionId) (\s@CreateDatasetContent' {} a -> s {versionId = a} :: CreateDatasetContent)

-- | The name of the dataset.
createDatasetContent_datasetName :: Lens.Lens' CreateDatasetContent Prelude.Text
createDatasetContent_datasetName = Lens.lens (\CreateDatasetContent' {datasetName} -> datasetName) (\s@CreateDatasetContent' {} a -> s {datasetName = a} :: CreateDatasetContent)

instance Prelude.AWSRequest CreateDatasetContent where
  type
    Rs CreateDatasetContent =
      CreateDatasetContentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetContentResponse'
            Prelude.<$> (x Prelude..?> "versionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetContent

instance Prelude.NFData CreateDatasetContent

instance Prelude.ToHeaders CreateDatasetContent where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateDatasetContent where
  toJSON CreateDatasetContent' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("versionId" Prelude..=) Prelude.<$> versionId]
      )

instance Prelude.ToPath CreateDatasetContent where
  toPath CreateDatasetContent' {..} =
    Prelude.mconcat
      ["/datasets/", Prelude.toBS datasetName, "/content"]

instance Prelude.ToQuery CreateDatasetContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse = CreateDatasetContentResponse'
  { -- | The version ID of the dataset contents that are being created.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'createDatasetContentResponse_versionId' - The version ID of the dataset contents that are being created.
--
-- 'httpStatus', 'createDatasetContentResponse_httpStatus' - The response's http status code.
newCreateDatasetContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetContentResponse
newCreateDatasetContentResponse pHttpStatus_ =
  CreateDatasetContentResponse'
    { versionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version ID of the dataset contents that are being created.
createDatasetContentResponse_versionId :: Lens.Lens' CreateDatasetContentResponse (Prelude.Maybe Prelude.Text)
createDatasetContentResponse_versionId = Lens.lens (\CreateDatasetContentResponse' {versionId} -> versionId) (\s@CreateDatasetContentResponse' {} a -> s {versionId = a} :: CreateDatasetContentResponse)

-- | The response's http status code.
createDatasetContentResponse_httpStatus :: Lens.Lens' CreateDatasetContentResponse Prelude.Int
createDatasetContentResponse_httpStatus = Lens.lens (\CreateDatasetContentResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetContentResponse' {} a -> s {httpStatus = a} :: CreateDatasetContentResponse)

instance Prelude.NFData CreateDatasetContentResponse
