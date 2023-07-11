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
-- Module      : Amazonka.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a dataset by applying a @queryAction@ (a SQL
-- query) or a @containerAction@ (executing a containerized application).
module Amazonka.IoTAnalytics.CreateDatasetContent
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CreateDatasetContent where
  type
    AWSResponse CreateDatasetContent =
      CreateDatasetContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetContentResponse'
            Prelude.<$> (x Data..?> "versionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetContent where
  hashWithSalt _salt CreateDatasetContent' {..} =
    _salt
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` datasetName

instance Prelude.NFData CreateDatasetContent where
  rnf CreateDatasetContent' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf datasetName

instance Data.ToHeaders CreateDatasetContent where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateDatasetContent where
  toJSON CreateDatasetContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [("versionId" Data..=) Prelude.<$> versionId]
      )

instance Data.ToPath CreateDatasetContent where
  toPath CreateDatasetContent' {..} =
    Prelude.mconcat
      ["/datasets/", Data.toBS datasetName, "/content"]

instance Data.ToQuery CreateDatasetContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse = CreateDatasetContentResponse'
  { -- | The version ID of the dataset contents that are being created.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateDatasetContentResponse where
  rnf CreateDatasetContentResponse' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
