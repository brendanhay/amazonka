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
-- Module      : Network.AWS.IoTAnalytics.GetDatasetContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of a data set as presigned URIs.
module Network.AWS.IoTAnalytics.GetDatasetContent
  ( -- * Creating a Request
    GetDatasetContent (..),
    newGetDatasetContent,

    -- * Request Lenses
    getDatasetContent_versionId,
    getDatasetContent_datasetName,

    -- * Destructuring the Response
    GetDatasetContentResponse (..),
    newGetDatasetContentResponse,

    -- * Response Lenses
    getDatasetContentResponse_status,
    getDatasetContentResponse_timestamp,
    getDatasetContentResponse_entries,
    getDatasetContentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDatasetContent' smart constructor.
data GetDatasetContent = GetDatasetContent'
  { -- | The version of the data set whose contents are retrieved. You can also
    -- use the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to retrieve the
    -- contents of the latest or latest successfully completed data set. If not
    -- specified, \"$LATEST_SUCCEEDED\" is the default.
    versionId :: Core.Maybe Core.Text,
    -- | The name of the data set whose contents are retrieved.
    datasetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDatasetContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'getDatasetContent_versionId' - The version of the data set whose contents are retrieved. You can also
-- use the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to retrieve the
-- contents of the latest or latest successfully completed data set. If not
-- specified, \"$LATEST_SUCCEEDED\" is the default.
--
-- 'datasetName', 'getDatasetContent_datasetName' - The name of the data set whose contents are retrieved.
newGetDatasetContent ::
  -- | 'datasetName'
  Core.Text ->
  GetDatasetContent
newGetDatasetContent pDatasetName_ =
  GetDatasetContent'
    { versionId = Core.Nothing,
      datasetName = pDatasetName_
    }

-- | The version of the data set whose contents are retrieved. You can also
-- use the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to retrieve the
-- contents of the latest or latest successfully completed data set. If not
-- specified, \"$LATEST_SUCCEEDED\" is the default.
getDatasetContent_versionId :: Lens.Lens' GetDatasetContent (Core.Maybe Core.Text)
getDatasetContent_versionId = Lens.lens (\GetDatasetContent' {versionId} -> versionId) (\s@GetDatasetContent' {} a -> s {versionId = a} :: GetDatasetContent)

-- | The name of the data set whose contents are retrieved.
getDatasetContent_datasetName :: Lens.Lens' GetDatasetContent Core.Text
getDatasetContent_datasetName = Lens.lens (\GetDatasetContent' {datasetName} -> datasetName) (\s@GetDatasetContent' {} a -> s {datasetName = a} :: GetDatasetContent)

instance Core.AWSRequest GetDatasetContent where
  type
    AWSResponse GetDatasetContent =
      GetDatasetContentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatasetContentResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "timestamp")
            Core.<*> (x Core..?> "entries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDatasetContent

instance Core.NFData GetDatasetContent

instance Core.ToHeaders GetDatasetContent where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetDatasetContent where
  toPath GetDatasetContent' {..} =
    Core.mconcat
      ["/datasets/", Core.toBS datasetName, "/content"]

instance Core.ToQuery GetDatasetContent where
  toQuery GetDatasetContent' {..} =
    Core.mconcat ["versionId" Core.=: versionId]

-- | /See:/ 'newGetDatasetContentResponse' smart constructor.
data GetDatasetContentResponse = GetDatasetContentResponse'
  { -- | The status of the data set content.
    status :: Core.Maybe DatasetContentStatus,
    -- | The time when the request was made.
    timestamp :: Core.Maybe Core.POSIX,
    -- | A list of @DatasetEntry@ objects.
    entries :: Core.Maybe [DatasetEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDatasetContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getDatasetContentResponse_status' - The status of the data set content.
--
-- 'timestamp', 'getDatasetContentResponse_timestamp' - The time when the request was made.
--
-- 'entries', 'getDatasetContentResponse_entries' - A list of @DatasetEntry@ objects.
--
-- 'httpStatus', 'getDatasetContentResponse_httpStatus' - The response's http status code.
newGetDatasetContentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDatasetContentResponse
newGetDatasetContentResponse pHttpStatus_ =
  GetDatasetContentResponse'
    { status = Core.Nothing,
      timestamp = Core.Nothing,
      entries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the data set content.
getDatasetContentResponse_status :: Lens.Lens' GetDatasetContentResponse (Core.Maybe DatasetContentStatus)
getDatasetContentResponse_status = Lens.lens (\GetDatasetContentResponse' {status} -> status) (\s@GetDatasetContentResponse' {} a -> s {status = a} :: GetDatasetContentResponse)

-- | The time when the request was made.
getDatasetContentResponse_timestamp :: Lens.Lens' GetDatasetContentResponse (Core.Maybe Core.UTCTime)
getDatasetContentResponse_timestamp = Lens.lens (\GetDatasetContentResponse' {timestamp} -> timestamp) (\s@GetDatasetContentResponse' {} a -> s {timestamp = a} :: GetDatasetContentResponse) Core.. Lens.mapping Core._Time

-- | A list of @DatasetEntry@ objects.
getDatasetContentResponse_entries :: Lens.Lens' GetDatasetContentResponse (Core.Maybe [DatasetEntry])
getDatasetContentResponse_entries = Lens.lens (\GetDatasetContentResponse' {entries} -> entries) (\s@GetDatasetContentResponse' {} a -> s {entries = a} :: GetDatasetContentResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDatasetContentResponse_httpStatus :: Lens.Lens' GetDatasetContentResponse Core.Int
getDatasetContentResponse_httpStatus = Lens.lens (\GetDatasetContentResponse' {httpStatus} -> httpStatus) (\s@GetDatasetContentResponse' {} a -> s {httpStatus = a} :: GetDatasetContentResponse)

instance Core.NFData GetDatasetContentResponse
