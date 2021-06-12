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
-- Module      : Network.AWS.GuardDuty.UnarchiveFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchives GuardDuty findings specified by the @findingIds@.
module Network.AWS.GuardDuty.UnarchiveFindings
  ( -- * Creating a Request
    UnarchiveFindings (..),
    newUnarchiveFindings,

    -- * Request Lenses
    unarchiveFindings_detectorId,
    unarchiveFindings_findingIds,

    -- * Destructuring the Response
    UnarchiveFindingsResponse (..),
    newUnarchiveFindingsResponse,

    -- * Response Lenses
    unarchiveFindingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { -- | The ID of the detector associated with the findings to unarchive.
    detectorId :: Core.Text,
    -- | The IDs of the findings to unarchive.
    findingIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnarchiveFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'unarchiveFindings_detectorId' - The ID of the detector associated with the findings to unarchive.
--
-- 'findingIds', 'unarchiveFindings_findingIds' - The IDs of the findings to unarchive.
newUnarchiveFindings ::
  -- | 'detectorId'
  Core.Text ->
  UnarchiveFindings
newUnarchiveFindings pDetectorId_ =
  UnarchiveFindings'
    { detectorId = pDetectorId_,
      findingIds = Core.mempty
    }

-- | The ID of the detector associated with the findings to unarchive.
unarchiveFindings_detectorId :: Lens.Lens' UnarchiveFindings Core.Text
unarchiveFindings_detectorId = Lens.lens (\UnarchiveFindings' {detectorId} -> detectorId) (\s@UnarchiveFindings' {} a -> s {detectorId = a} :: UnarchiveFindings)

-- | The IDs of the findings to unarchive.
unarchiveFindings_findingIds :: Lens.Lens' UnarchiveFindings [Core.Text]
unarchiveFindings_findingIds = Lens.lens (\UnarchiveFindings' {findingIds} -> findingIds) (\s@UnarchiveFindings' {} a -> s {findingIds = a} :: UnarchiveFindings) Core.. Lens._Coerce

instance Core.AWSRequest UnarchiveFindings where
  type
    AWSResponse UnarchiveFindings =
      UnarchiveFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnarchiveFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UnarchiveFindings

instance Core.NFData UnarchiveFindings

instance Core.ToHeaders UnarchiveFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UnarchiveFindings where
  toJSON UnarchiveFindings' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("findingIds" Core..= findingIds)]
      )

instance Core.ToPath UnarchiveFindings where
  toPath UnarchiveFindings' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/findings/unarchive"
      ]

instance Core.ToQuery UnarchiveFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUnarchiveFindingsResponse' smart constructor.
data UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnarchiveFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'unarchiveFindingsResponse_httpStatus' - The response's http status code.
newUnarchiveFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UnarchiveFindingsResponse
newUnarchiveFindingsResponse pHttpStatus_ =
  UnarchiveFindingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
unarchiveFindingsResponse_httpStatus :: Lens.Lens' UnarchiveFindingsResponse Core.Int
unarchiveFindingsResponse_httpStatus = Lens.lens (\UnarchiveFindingsResponse' {httpStatus} -> httpStatus) (\s@UnarchiveFindingsResponse' {} a -> s {httpStatus = a} :: UnarchiveFindingsResponse)

instance Core.NFData UnarchiveFindingsResponse
