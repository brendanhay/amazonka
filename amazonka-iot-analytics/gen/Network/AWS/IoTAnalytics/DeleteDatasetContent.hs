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
-- Module      : Network.AWS.IoTAnalytics.DeleteDatasetContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of the specified dataset.
module Network.AWS.IoTAnalytics.DeleteDatasetContent
  ( -- * Creating a Request
    DeleteDatasetContent (..),
    newDeleteDatasetContent,

    -- * Request Lenses
    deleteDatasetContent_versionId,
    deleteDatasetContent_datasetName,

    -- * Destructuring the Response
    DeleteDatasetContentResponse (..),
    newDeleteDatasetContentResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDatasetContent' smart constructor.
data DeleteDatasetContent = DeleteDatasetContent'
  { -- | The version of the dataset whose content is deleted. You can also use
    -- the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to delete the latest or
    -- latest successfully completed data set. If not specified,
    -- \"$LATEST_SUCCEEDED\" is the default.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset whose content is deleted.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'deleteDatasetContent_versionId' - The version of the dataset whose content is deleted. You can also use
-- the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to delete the latest or
-- latest successfully completed data set. If not specified,
-- \"$LATEST_SUCCEEDED\" is the default.
--
-- 'datasetName', 'deleteDatasetContent_datasetName' - The name of the dataset whose content is deleted.
newDeleteDatasetContent ::
  -- | 'datasetName'
  Prelude.Text ->
  DeleteDatasetContent
newDeleteDatasetContent pDatasetName_ =
  DeleteDatasetContent'
    { versionId = Prelude.Nothing,
      datasetName = pDatasetName_
    }

-- | The version of the dataset whose content is deleted. You can also use
-- the strings \"$LATEST\" or \"$LATEST_SUCCEEDED\" to delete the latest or
-- latest successfully completed data set. If not specified,
-- \"$LATEST_SUCCEEDED\" is the default.
deleteDatasetContent_versionId :: Lens.Lens' DeleteDatasetContent (Prelude.Maybe Prelude.Text)
deleteDatasetContent_versionId = Lens.lens (\DeleteDatasetContent' {versionId} -> versionId) (\s@DeleteDatasetContent' {} a -> s {versionId = a} :: DeleteDatasetContent)

-- | The name of the dataset whose content is deleted.
deleteDatasetContent_datasetName :: Lens.Lens' DeleteDatasetContent Prelude.Text
deleteDatasetContent_datasetName = Lens.lens (\DeleteDatasetContent' {datasetName} -> datasetName) (\s@DeleteDatasetContent' {} a -> s {datasetName = a} :: DeleteDatasetContent)

instance Prelude.AWSRequest DeleteDatasetContent where
  type
    Rs DeleteDatasetContent =
      DeleteDatasetContentResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteDatasetContentResponse'

instance Prelude.Hashable DeleteDatasetContent

instance Prelude.NFData DeleteDatasetContent

instance Prelude.ToHeaders DeleteDatasetContent where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDatasetContent where
  toPath DeleteDatasetContent' {..} =
    Prelude.mconcat
      ["/datasets/", Prelude.toBS datasetName, "/content"]

instance Prelude.ToQuery DeleteDatasetContent where
  toQuery DeleteDatasetContent' {..} =
    Prelude.mconcat ["versionId" Prelude.=: versionId]

-- | /See:/ 'newDeleteDatasetContentResponse' smart constructor.
data DeleteDatasetContentResponse = DeleteDatasetContentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatasetContentResponse ::
  DeleteDatasetContentResponse
newDeleteDatasetContentResponse =
  DeleteDatasetContentResponse'

instance Prelude.NFData DeleteDatasetContentResponse
