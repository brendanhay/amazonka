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
-- Module      : Network.AWS.GuardDuty.UpdateThreatIntelSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.UpdateThreatIntelSet
  ( -- * Creating a Request
    UpdateThreatIntelSet (..),
    newUpdateThreatIntelSet,

    -- * Request Lenses
    updateThreatIntelSet_activate,
    updateThreatIntelSet_name,
    updateThreatIntelSet_location,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,

    -- * Destructuring the Response
    UpdateThreatIntelSetResponse (..),
    newUpdateThreatIntelSetResponse,

    -- * Response Lenses
    updateThreatIntelSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThreatIntelSet' smart constructor.
data UpdateThreatIntelSet = UpdateThreatIntelSet'
  { -- | The updated Boolean value that specifies whether the ThreateIntelSet is
    -- active or not.
    activate :: Core.Maybe Core.Bool,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    name :: Core.Maybe Core.Text,
    -- | The updated URI of the file that contains the ThreateIntelSet.
    location :: Core.Maybe Core.Text,
    -- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
    -- you want to update.
    detectorId :: Core.Text,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    threatIntelSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateThreatIntelSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activate', 'updateThreatIntelSet_activate' - The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
--
-- 'name', 'updateThreatIntelSet_name' - The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- 'location', 'updateThreatIntelSet_location' - The updated URI of the file that contains the ThreateIntelSet.
--
-- 'detectorId', 'updateThreatIntelSet_detectorId' - The detectorID that specifies the GuardDuty service whose ThreatIntelSet
-- you want to update.
--
-- 'threatIntelSetId', 'updateThreatIntelSet_threatIntelSetId' - The unique ID that specifies the ThreatIntelSet that you want to update.
newUpdateThreatIntelSet ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'threatIntelSetId'
  Core.Text ->
  UpdateThreatIntelSet
newUpdateThreatIntelSet
  pDetectorId_
  pThreatIntelSetId_ =
    UpdateThreatIntelSet'
      { activate = Core.Nothing,
        name = Core.Nothing,
        location = Core.Nothing,
        detectorId = pDetectorId_,
        threatIntelSetId = pThreatIntelSetId_
      }

-- | The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
updateThreatIntelSet_activate :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Core.Bool)
updateThreatIntelSet_activate = Lens.lens (\UpdateThreatIntelSet' {activate} -> activate) (\s@UpdateThreatIntelSet' {} a -> s {activate = a} :: UpdateThreatIntelSet)

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_name :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Core.Text)
updateThreatIntelSet_name = Lens.lens (\UpdateThreatIntelSet' {name} -> name) (\s@UpdateThreatIntelSet' {} a -> s {name = a} :: UpdateThreatIntelSet)

-- | The updated URI of the file that contains the ThreateIntelSet.
updateThreatIntelSet_location :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Core.Text)
updateThreatIntelSet_location = Lens.lens (\UpdateThreatIntelSet' {location} -> location) (\s@UpdateThreatIntelSet' {} a -> s {location = a} :: UpdateThreatIntelSet)

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
-- you want to update.
updateThreatIntelSet_detectorId :: Lens.Lens' UpdateThreatIntelSet Core.Text
updateThreatIntelSet_detectorId = Lens.lens (\UpdateThreatIntelSet' {detectorId} -> detectorId) (\s@UpdateThreatIntelSet' {} a -> s {detectorId = a} :: UpdateThreatIntelSet)

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_threatIntelSetId :: Lens.Lens' UpdateThreatIntelSet Core.Text
updateThreatIntelSet_threatIntelSetId = Lens.lens (\UpdateThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@UpdateThreatIntelSet' {} a -> s {threatIntelSetId = a} :: UpdateThreatIntelSet)

instance Core.AWSRequest UpdateThreatIntelSet where
  type
    AWSResponse UpdateThreatIntelSet =
      UpdateThreatIntelSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThreatIntelSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateThreatIntelSet

instance Core.NFData UpdateThreatIntelSet

instance Core.ToHeaders UpdateThreatIntelSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateThreatIntelSet where
  toJSON UpdateThreatIntelSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("activate" Core..=) Core.<$> activate,
            ("name" Core..=) Core.<$> name,
            ("location" Core..=) Core.<$> location
          ]
      )

instance Core.ToPath UpdateThreatIntelSet where
  toPath UpdateThreatIntelSet' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/threatintelset/",
        Core.toBS threatIntelSetId
      ]

instance Core.ToQuery UpdateThreatIntelSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateThreatIntelSetResponse' smart constructor.
data UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateThreatIntelSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateThreatIntelSetResponse_httpStatus' - The response's http status code.
newUpdateThreatIntelSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateThreatIntelSetResponse
newUpdateThreatIntelSetResponse pHttpStatus_ =
  UpdateThreatIntelSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateThreatIntelSetResponse_httpStatus :: Lens.Lens' UpdateThreatIntelSetResponse Core.Int
updateThreatIntelSetResponse_httpStatus = Lens.lens (\UpdateThreatIntelSetResponse' {httpStatus} -> httpStatus) (\s@UpdateThreatIntelSetResponse' {} a -> s {httpStatus = a} :: UpdateThreatIntelSetResponse)

instance Core.NFData UpdateThreatIntelSetResponse
