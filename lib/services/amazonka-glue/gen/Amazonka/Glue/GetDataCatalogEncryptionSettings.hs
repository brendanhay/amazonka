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
-- Module      : Amazonka.Glue.GetDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configuration for a specified catalog.
module Amazonka.Glue.GetDataCatalogEncryptionSettings
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataCatalogEncryptionSettings' smart constructor.
data GetDataCatalogEncryptionSettings = GetDataCatalogEncryptionSettings'
  { -- | The ID of the Data Catalog to retrieve the security configuration for.
    -- If none is provided, the Amazon Web Services account ID is used by
    -- default.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getDataCatalogEncryptionSettings_catalogId' - The ID of the Data Catalog to retrieve the security configuration for.
-- If none is provided, the Amazon Web Services account ID is used by
-- default.
newGetDataCatalogEncryptionSettings ::
  GetDataCatalogEncryptionSettings
newGetDataCatalogEncryptionSettings =
  GetDataCatalogEncryptionSettings'
    { catalogId =
        Prelude.Nothing
    }

-- | The ID of the Data Catalog to retrieve the security configuration for.
-- If none is provided, the Amazon Web Services account ID is used by
-- default.
getDataCatalogEncryptionSettings_catalogId :: Lens.Lens' GetDataCatalogEncryptionSettings (Prelude.Maybe Prelude.Text)
getDataCatalogEncryptionSettings_catalogId = Lens.lens (\GetDataCatalogEncryptionSettings' {catalogId} -> catalogId) (\s@GetDataCatalogEncryptionSettings' {} a -> s {catalogId = a} :: GetDataCatalogEncryptionSettings)

instance
  Core.AWSRequest
    GetDataCatalogEncryptionSettings
  where
  type
    AWSResponse GetDataCatalogEncryptionSettings =
      GetDataCatalogEncryptionSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataCatalogEncryptionSettingsResponse'
            Prelude.<$> (x Data..?> "DataCatalogEncryptionSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDataCatalogEncryptionSettings
  where
  hashWithSalt
    _salt
    GetDataCatalogEncryptionSettings' {..} =
      _salt `Prelude.hashWithSalt` catalogId

instance
  Prelude.NFData
    GetDataCatalogEncryptionSettings
  where
  rnf GetDataCatalogEncryptionSettings' {..} =
    Prelude.rnf catalogId

instance
  Data.ToHeaders
    GetDataCatalogEncryptionSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetDataCatalogEncryptionSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataCatalogEncryptionSettings where
  toJSON GetDataCatalogEncryptionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CatalogId" Data..=) Prelude.<$> catalogId]
      )

instance Data.ToPath GetDataCatalogEncryptionSettings where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetDataCatalogEncryptionSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataCatalogEncryptionSettingsResponse' smart constructor.
data GetDataCatalogEncryptionSettingsResponse = GetDataCatalogEncryptionSettingsResponse'
  { -- | The requested security configuration.
    dataCatalogEncryptionSettings :: Prelude.Maybe DataCatalogEncryptionSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDataCatalogEncryptionSettingsResponse
newGetDataCatalogEncryptionSettingsResponse
  pHttpStatus_ =
    GetDataCatalogEncryptionSettingsResponse'
      { dataCatalogEncryptionSettings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The requested security configuration.
getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse (Prelude.Maybe DataCatalogEncryptionSettings)
getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings = Lens.lens (\GetDataCatalogEncryptionSettingsResponse' {dataCatalogEncryptionSettings} -> dataCatalogEncryptionSettings) (\s@GetDataCatalogEncryptionSettingsResponse' {} a -> s {dataCatalogEncryptionSettings = a} :: GetDataCatalogEncryptionSettingsResponse)

-- | The response's http status code.
getDataCatalogEncryptionSettingsResponse_httpStatus :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse Prelude.Int
getDataCatalogEncryptionSettingsResponse_httpStatus = Lens.lens (\GetDataCatalogEncryptionSettingsResponse' {httpStatus} -> httpStatus) (\s@GetDataCatalogEncryptionSettingsResponse' {} a -> s {httpStatus = a} :: GetDataCatalogEncryptionSettingsResponse)

instance
  Prelude.NFData
    GetDataCatalogEncryptionSettingsResponse
  where
  rnf GetDataCatalogEncryptionSettingsResponse' {..} =
    Prelude.rnf dataCatalogEncryptionSettings `Prelude.seq`
      Prelude.rnf httpStatus
