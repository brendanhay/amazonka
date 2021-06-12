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
-- Module      : Network.AWS.GuardDuty.UpdateDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.UpdateDetector
  ( -- * Creating a Request
    UpdateDetector (..),
    newUpdateDetector,

    -- * Request Lenses
    updateDetector_enable,
    updateDetector_dataSources,
    updateDetector_findingPublishingFrequency,
    updateDetector_detectorId,

    -- * Destructuring the Response
    UpdateDetectorResponse (..),
    newUpdateDetectorResponse,

    -- * Response Lenses
    updateDetectorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDetector' smart constructor.
data UpdateDetector = UpdateDetector'
  { -- | Specifies whether the detector is enabled or not enabled.
    enable :: Core.Maybe Core.Bool,
    -- | Describes which data sources will be updated.
    dataSources :: Core.Maybe DataSourceConfigurations,
    -- | An enum value that specifies how frequently findings are exported, such
    -- as to CloudWatch Events.
    findingPublishingFrequency :: Core.Maybe FindingPublishingFrequency,
    -- | The unique ID of the detector to update.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'updateDetector_enable' - Specifies whether the detector is enabled or not enabled.
--
-- 'dataSources', 'updateDetector_dataSources' - Describes which data sources will be updated.
--
-- 'findingPublishingFrequency', 'updateDetector_findingPublishingFrequency' - An enum value that specifies how frequently findings are exported, such
-- as to CloudWatch Events.
--
-- 'detectorId', 'updateDetector_detectorId' - The unique ID of the detector to update.
newUpdateDetector ::
  -- | 'detectorId'
  Core.Text ->
  UpdateDetector
newUpdateDetector pDetectorId_ =
  UpdateDetector'
    { enable = Core.Nothing,
      dataSources = Core.Nothing,
      findingPublishingFrequency = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | Specifies whether the detector is enabled or not enabled.
updateDetector_enable :: Lens.Lens' UpdateDetector (Core.Maybe Core.Bool)
updateDetector_enable = Lens.lens (\UpdateDetector' {enable} -> enable) (\s@UpdateDetector' {} a -> s {enable = a} :: UpdateDetector)

-- | Describes which data sources will be updated.
updateDetector_dataSources :: Lens.Lens' UpdateDetector (Core.Maybe DataSourceConfigurations)
updateDetector_dataSources = Lens.lens (\UpdateDetector' {dataSources} -> dataSources) (\s@UpdateDetector' {} a -> s {dataSources = a} :: UpdateDetector)

-- | An enum value that specifies how frequently findings are exported, such
-- as to CloudWatch Events.
updateDetector_findingPublishingFrequency :: Lens.Lens' UpdateDetector (Core.Maybe FindingPublishingFrequency)
updateDetector_findingPublishingFrequency = Lens.lens (\UpdateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@UpdateDetector' {} a -> s {findingPublishingFrequency = a} :: UpdateDetector)

-- | The unique ID of the detector to update.
updateDetector_detectorId :: Lens.Lens' UpdateDetector Core.Text
updateDetector_detectorId = Lens.lens (\UpdateDetector' {detectorId} -> detectorId) (\s@UpdateDetector' {} a -> s {detectorId = a} :: UpdateDetector)

instance Core.AWSRequest UpdateDetector where
  type
    AWSResponse UpdateDetector =
      UpdateDetectorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDetector

instance Core.NFData UpdateDetector

instance Core.ToHeaders UpdateDetector where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDetector where
  toJSON UpdateDetector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("enable" Core..=) Core.<$> enable,
            ("dataSources" Core..=) Core.<$> dataSources,
            ("findingPublishingFrequency" Core..=)
              Core.<$> findingPublishingFrequency
          ]
      )

instance Core.ToPath UpdateDetector where
  toPath UpdateDetector' {..} =
    Core.mconcat ["/detector/", Core.toBS detectorId]

instance Core.ToQuery UpdateDetector where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDetectorResponse' smart constructor.
data UpdateDetectorResponse = UpdateDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDetectorResponse_httpStatus' - The response's http status code.
newUpdateDetectorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDetectorResponse
newUpdateDetectorResponse pHttpStatus_ =
  UpdateDetectorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDetectorResponse_httpStatus :: Lens.Lens' UpdateDetectorResponse Core.Int
updateDetectorResponse_httpStatus = Lens.lens (\UpdateDetectorResponse' {httpStatus} -> httpStatus) (\s@UpdateDetectorResponse' {} a -> s {httpStatus = a} :: UpdateDetectorResponse)

instance Core.NFData UpdateDetectorResponse
