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
-- Module      : Network.AWS.Glue.GetDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configuration for a specified catalog.
module Network.AWS.Glue.GetDataCatalogEncryptionSettings
  ( -- * Creating a Request
    GetDataCatalogEncryptionSettings (..),
    newGetDataCatalogEncryptionSettings,

    -- * Request Lenses
    getDataCatalogEncryptionSettings_catalogId,

    -- * Destructuring the Response
    GetDataCatalogEncryptionSettingsResponse (..),
    newGetDataCatalogEncryptionSettingsResponse,

    -- * Response Lenses
    getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings,
    getDataCatalogEncryptionSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataCatalogEncryptionSettings' smart constructor.
data GetDataCatalogEncryptionSettings = GetDataCatalogEncryptionSettings'
  { -- | The ID of the Data Catalog to retrieve the security configuration for.
    -- If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getDataCatalogEncryptionSettings_catalogId' - The ID of the Data Catalog to retrieve the security configuration for.
-- If none is provided, the AWS account ID is used by default.
newGetDataCatalogEncryptionSettings ::
  GetDataCatalogEncryptionSettings
newGetDataCatalogEncryptionSettings =
  GetDataCatalogEncryptionSettings'
    { catalogId =
        Core.Nothing
    }

-- | The ID of the Data Catalog to retrieve the security configuration for.
-- If none is provided, the AWS account ID is used by default.
getDataCatalogEncryptionSettings_catalogId :: Lens.Lens' GetDataCatalogEncryptionSettings (Core.Maybe Core.Text)
getDataCatalogEncryptionSettings_catalogId = Lens.lens (\GetDataCatalogEncryptionSettings' {catalogId} -> catalogId) (\s@GetDataCatalogEncryptionSettings' {} a -> s {catalogId = a} :: GetDataCatalogEncryptionSettings)

instance
  Core.AWSRequest
    GetDataCatalogEncryptionSettings
  where
  type
    AWSResponse GetDataCatalogEncryptionSettings =
      GetDataCatalogEncryptionSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataCatalogEncryptionSettingsResponse'
            Core.<$> (x Core..?> "DataCatalogEncryptionSettings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetDataCatalogEncryptionSettings

instance Core.NFData GetDataCatalogEncryptionSettings

instance
  Core.ToHeaders
    GetDataCatalogEncryptionSettings
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetDataCatalogEncryptionSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDataCatalogEncryptionSettings where
  toJSON GetDataCatalogEncryptionSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("CatalogId" Core..=) Core.<$> catalogId]
      )

instance Core.ToPath GetDataCatalogEncryptionSettings where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetDataCatalogEncryptionSettings
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDataCatalogEncryptionSettingsResponse' smart constructor.
data GetDataCatalogEncryptionSettingsResponse = GetDataCatalogEncryptionSettingsResponse'
  { -- | The requested security configuration.
    dataCatalogEncryptionSettings :: Core.Maybe DataCatalogEncryptionSettings,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDataCatalogEncryptionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCatalogEncryptionSettings', 'getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings' - The requested security configuration.
--
-- 'httpStatus', 'getDataCatalogEncryptionSettingsResponse_httpStatus' - The response's http status code.
newGetDataCatalogEncryptionSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDataCatalogEncryptionSettingsResponse
newGetDataCatalogEncryptionSettingsResponse
  pHttpStatus_ =
    GetDataCatalogEncryptionSettingsResponse'
      { dataCatalogEncryptionSettings =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The requested security configuration.
getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse (Core.Maybe DataCatalogEncryptionSettings)
getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings = Lens.lens (\GetDataCatalogEncryptionSettingsResponse' {dataCatalogEncryptionSettings} -> dataCatalogEncryptionSettings) (\s@GetDataCatalogEncryptionSettingsResponse' {} a -> s {dataCatalogEncryptionSettings = a} :: GetDataCatalogEncryptionSettingsResponse)

-- | The response's http status code.
getDataCatalogEncryptionSettingsResponse_httpStatus :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse Core.Int
getDataCatalogEncryptionSettingsResponse_httpStatus = Lens.lens (\GetDataCatalogEncryptionSettingsResponse' {httpStatus} -> httpStatus) (\s@GetDataCatalogEncryptionSettingsResponse' {} a -> s {httpStatus = a} :: GetDataCatalogEncryptionSettingsResponse)

instance
  Core.NFData
    GetDataCatalogEncryptionSettingsResponse
