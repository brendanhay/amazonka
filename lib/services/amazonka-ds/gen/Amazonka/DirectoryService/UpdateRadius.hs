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
-- Module      : Amazonka.DirectoryService.UpdateRadius
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Remote Authentication Dial In User Service (RADIUS) server
-- information for an AD Connector or Microsoft AD directory.
module Amazonka.DirectoryService.UpdateRadius
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the UpdateRadius operation.
--
-- /See:/ 'newUpdateRadius' smart constructor.
data UpdateRadius = UpdateRadius'
  { -- | The identifier of the directory for which to update the RADIUS server
    -- information.
    directoryId :: Prelude.Text,
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server.
    radiusSettings :: RadiusSettings
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
updateRadius_directoryId :: Lens.Lens' UpdateRadius Prelude.Text
updateRadius_directoryId = Lens.lens (\UpdateRadius' {directoryId} -> directoryId) (\s@UpdateRadius' {} a -> s {directoryId = a} :: UpdateRadius)

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
updateRadius_radiusSettings :: Lens.Lens' UpdateRadius RadiusSettings
updateRadius_radiusSettings = Lens.lens (\UpdateRadius' {radiusSettings} -> radiusSettings) (\s@UpdateRadius' {} a -> s {radiusSettings = a} :: UpdateRadius)

instance Core.AWSRequest UpdateRadius where
  type AWSResponse UpdateRadius = UpdateRadiusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRadiusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRadius where
  hashWithSalt _salt UpdateRadius' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` radiusSettings

instance Prelude.NFData UpdateRadius where
  rnf UpdateRadius' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf radiusSettings

instance Data.ToHeaders UpdateRadius where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.UpdateRadius" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRadius where
  toJSON UpdateRadius' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("RadiusSettings" Data..= radiusSettings)
          ]
      )

instance Data.ToPath UpdateRadius where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRadius where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the UpdateRadius operation.
--
-- /See:/ 'newUpdateRadiusResponse' smart constructor.
data UpdateRadiusResponse = UpdateRadiusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateRadiusResponse
newUpdateRadiusResponse pHttpStatus_ =
  UpdateRadiusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRadiusResponse_httpStatus :: Lens.Lens' UpdateRadiusResponse Prelude.Int
updateRadiusResponse_httpStatus = Lens.lens (\UpdateRadiusResponse' {httpStatus} -> httpStatus) (\s@UpdateRadiusResponse' {} a -> s {httpStatus = a} :: UpdateRadiusResponse)

instance Prelude.NFData UpdateRadiusResponse where
  rnf UpdateRadiusResponse' {..} =
    Prelude.rnf httpStatus
