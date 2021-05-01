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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Prelude.Text,
    -- | The name of the filter that you want to delete.
    filterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  DeleteFilter
newDeleteFilter pDetectorId_ pFilterName_ =
  DeleteFilter'
    { detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | The unique ID of the detector that the filter is associated with.
deleteFilter_detectorId :: Lens.Lens' DeleteFilter Prelude.Text
deleteFilter_detectorId = Lens.lens (\DeleteFilter' {detectorId} -> detectorId) (\s@DeleteFilter' {} a -> s {detectorId = a} :: DeleteFilter)

-- | The name of the filter that you want to delete.
deleteFilter_filterName :: Lens.Lens' DeleteFilter Prelude.Text
deleteFilter_filterName = Lens.lens (\DeleteFilter' {filterName} -> filterName) (\s@DeleteFilter' {} a -> s {filterName = a} :: DeleteFilter)

instance Prelude.AWSRequest DeleteFilter where
  type Rs DeleteFilter = DeleteFilterResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFilter

instance Prelude.NFData DeleteFilter

instance Prelude.ToHeaders DeleteFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteFilter where
  toPath DeleteFilter' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/filter/",
        Prelude.toBS filterName
      ]

instance Prelude.ToQuery DeleteFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFilterResponse' smart constructor.
data DeleteFilterResponse = DeleteFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteFilterResponse
newDeleteFilterResponse pHttpStatus_ =
  DeleteFilterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFilterResponse_httpStatus :: Lens.Lens' DeleteFilterResponse Prelude.Int
deleteFilterResponse_httpStatus = Lens.lens (\DeleteFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteFilterResponse' {} a -> s {httpStatus = a} :: DeleteFilterResponse)

instance Prelude.NFData DeleteFilterResponse
