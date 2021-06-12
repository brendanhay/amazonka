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
-- Module      : Network.AWS.GuardDuty.DeleteThreatIntelSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.DeleteThreatIntelSet
  ( -- * Creating a Request
    DeleteThreatIntelSet (..),
    newDeleteThreatIntelSet,

    -- * Request Lenses
    deleteThreatIntelSet_detectorId,
    deleteThreatIntelSet_threatIntelSetId,

    -- * Destructuring the Response
    DeleteThreatIntelSetResponse (..),
    newDeleteThreatIntelSetResponse,

    -- * Response Lenses
    deleteThreatIntelSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteThreatIntelSet' smart constructor.
data DeleteThreatIntelSet = DeleteThreatIntelSet'
  { -- | The unique ID of the detector that the threatIntelSet is associated
    -- with.
    detectorId :: Core.Text,
    -- | The unique ID of the threatIntelSet that you want to delete.
    threatIntelSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThreatIntelSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteThreatIntelSet_detectorId' - The unique ID of the detector that the threatIntelSet is associated
-- with.
--
-- 'threatIntelSetId', 'deleteThreatIntelSet_threatIntelSetId' - The unique ID of the threatIntelSet that you want to delete.
newDeleteThreatIntelSet ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'threatIntelSetId'
  Core.Text ->
  DeleteThreatIntelSet
newDeleteThreatIntelSet
  pDetectorId_
  pThreatIntelSetId_ =
    DeleteThreatIntelSet'
      { detectorId = pDetectorId_,
        threatIntelSetId = pThreatIntelSetId_
      }

-- | The unique ID of the detector that the threatIntelSet is associated
-- with.
deleteThreatIntelSet_detectorId :: Lens.Lens' DeleteThreatIntelSet Core.Text
deleteThreatIntelSet_detectorId = Lens.lens (\DeleteThreatIntelSet' {detectorId} -> detectorId) (\s@DeleteThreatIntelSet' {} a -> s {detectorId = a} :: DeleteThreatIntelSet)

-- | The unique ID of the threatIntelSet that you want to delete.
deleteThreatIntelSet_threatIntelSetId :: Lens.Lens' DeleteThreatIntelSet Core.Text
deleteThreatIntelSet_threatIntelSetId = Lens.lens (\DeleteThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@DeleteThreatIntelSet' {} a -> s {threatIntelSetId = a} :: DeleteThreatIntelSet)

instance Core.AWSRequest DeleteThreatIntelSet where
  type
    AWSResponse DeleteThreatIntelSet =
      DeleteThreatIntelSetResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThreatIntelSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteThreatIntelSet

instance Core.NFData DeleteThreatIntelSet

instance Core.ToHeaders DeleteThreatIntelSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteThreatIntelSet where
  toPath DeleteThreatIntelSet' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/threatintelset/",
        Core.toBS threatIntelSetId
      ]

instance Core.ToQuery DeleteThreatIntelSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteThreatIntelSetResponse' smart constructor.
data DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThreatIntelSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteThreatIntelSetResponse_httpStatus' - The response's http status code.
newDeleteThreatIntelSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteThreatIntelSetResponse
newDeleteThreatIntelSetResponse pHttpStatus_ =
  DeleteThreatIntelSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteThreatIntelSetResponse_httpStatus :: Lens.Lens' DeleteThreatIntelSetResponse Core.Int
deleteThreatIntelSetResponse_httpStatus = Lens.lens (\DeleteThreatIntelSetResponse' {httpStatus} -> httpStatus) (\s@DeleteThreatIntelSetResponse' {} a -> s {httpStatus = a} :: DeleteThreatIntelSetResponse)

instance Core.NFData DeleteThreatIntelSetResponse
