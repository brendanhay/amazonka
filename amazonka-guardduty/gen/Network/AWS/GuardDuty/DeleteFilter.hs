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
-- Module      : Network.AWS.GuardDuty.DeleteFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the filter specified by the filter name.
module Network.AWS.GuardDuty.DeleteFilter
  ( -- * Creating a Request
    DeleteFilter (..),
    newDeleteFilter,

    -- * Request Lenses
    deleteFilter_detectorId,
    deleteFilter_filterName,

    -- * Destructuring the Response
    DeleteFilterResponse (..),
    newDeleteFilterResponse,

    -- * Response Lenses
    deleteFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Core.Text,
    -- | The name of the filter that you want to delete.
    filterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteFilter_detectorId' - The unique ID of the detector that the filter is associated with.
--
-- 'filterName', 'deleteFilter_filterName' - The name of the filter that you want to delete.
newDeleteFilter ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'filterName'
  Core.Text ->
  DeleteFilter
newDeleteFilter pDetectorId_ pFilterName_ =
  DeleteFilter'
    { detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | The unique ID of the detector that the filter is associated with.
deleteFilter_detectorId :: Lens.Lens' DeleteFilter Core.Text
deleteFilter_detectorId = Lens.lens (\DeleteFilter' {detectorId} -> detectorId) (\s@DeleteFilter' {} a -> s {detectorId = a} :: DeleteFilter)

-- | The name of the filter that you want to delete.
deleteFilter_filterName :: Lens.Lens' DeleteFilter Core.Text
deleteFilter_filterName = Lens.lens (\DeleteFilter' {filterName} -> filterName) (\s@DeleteFilter' {} a -> s {filterName = a} :: DeleteFilter)

instance Core.AWSRequest DeleteFilter where
  type AWSResponse DeleteFilter = DeleteFilterResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFilterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteFilter

instance Core.NFData DeleteFilter

instance Core.ToHeaders DeleteFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteFilter where
  toPath DeleteFilter' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/filter/",
        Core.toBS filterName
      ]

instance Core.ToQuery DeleteFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFilterResponse' smart constructor.
data DeleteFilterResponse = DeleteFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFilterResponse_httpStatus' - The response's http status code.
newDeleteFilterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteFilterResponse
newDeleteFilterResponse pHttpStatus_ =
  DeleteFilterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFilterResponse_httpStatus :: Lens.Lens' DeleteFilterResponse Core.Int
deleteFilterResponse_httpStatus = Lens.lens (\DeleteFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteFilterResponse' {} a -> s {httpStatus = a} :: DeleteFilterResponse)

instance Core.NFData DeleteFilterResponse
