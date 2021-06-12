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
-- Module      : Network.AWS.DirectoryService.UpdateRadius
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Remote Authentication Dial In User Service (RADIUS) server
-- information for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.UpdateRadius
  ( -- * Creating a Request
    UpdateRadius (..),
    newUpdateRadius,

    -- * Request Lenses
    updateRadius_directoryId,
    updateRadius_radiusSettings,

    -- * Destructuring the Response
    UpdateRadiusResponse (..),
    newUpdateRadiusResponse,

    -- * Response Lenses
    updateRadiusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the UpdateRadius operation.
--
-- /See:/ 'newUpdateRadius' smart constructor.
data UpdateRadius = UpdateRadius'
  { -- | The identifier of the directory for which to update the RADIUS server
    -- information.
    directoryId :: Core.Text,
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server.
    radiusSettings :: RadiusSettings
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRadius' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'updateRadius_directoryId' - The identifier of the directory for which to update the RADIUS server
-- information.
--
-- 'radiusSettings', 'updateRadius_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server.
newUpdateRadius ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'radiusSettings'
  RadiusSettings ->
  UpdateRadius
newUpdateRadius pDirectoryId_ pRadiusSettings_ =
  UpdateRadius'
    { directoryId = pDirectoryId_,
      radiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory for which to update the RADIUS server
-- information.
updateRadius_directoryId :: Lens.Lens' UpdateRadius Core.Text
updateRadius_directoryId = Lens.lens (\UpdateRadius' {directoryId} -> directoryId) (\s@UpdateRadius' {} a -> s {directoryId = a} :: UpdateRadius)

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
updateRadius_radiusSettings :: Lens.Lens' UpdateRadius RadiusSettings
updateRadius_radiusSettings = Lens.lens (\UpdateRadius' {radiusSettings} -> radiusSettings) (\s@UpdateRadius' {} a -> s {radiusSettings = a} :: UpdateRadius)

instance Core.AWSRequest UpdateRadius where
  type AWSResponse UpdateRadius = UpdateRadiusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRadiusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRadius

instance Core.NFData UpdateRadius

instance Core.ToHeaders UpdateRadius where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.UpdateRadius" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRadius where
  toJSON UpdateRadius' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("RadiusSettings" Core..= radiusSettings)
          ]
      )

instance Core.ToPath UpdateRadius where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRadius where
  toQuery = Core.const Core.mempty

-- | Contains the results of the UpdateRadius operation.
--
-- /See:/ 'newUpdateRadiusResponse' smart constructor.
data UpdateRadiusResponse = UpdateRadiusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRadiusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRadiusResponse_httpStatus' - The response's http status code.
newUpdateRadiusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRadiusResponse
newUpdateRadiusResponse pHttpStatus_ =
  UpdateRadiusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRadiusResponse_httpStatus :: Lens.Lens' UpdateRadiusResponse Core.Int
updateRadiusResponse_httpStatus = Lens.lens (\UpdateRadiusResponse' {httpStatus} -> httpStatus) (\s@UpdateRadiusResponse' {} a -> s {httpStatus = a} :: UpdateRadiusResponse)

instance Core.NFData UpdateRadiusResponse
