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
-- Module      : Amazonka.Glue.PutDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the security configuration for a specified catalog. After the
-- configuration has been set, the specified encryption is applied to every
-- catalog write thereafter.
module Amazonka.Glue.PutDataCatalogEncryptionSettings
  ( -- * Creating a Request
    PutDataCatalogEncryptionSettings (..),
    newPutDataCatalogEncryptionSettings,

    -- * Request Lenses
    putDataCatalogEncryptionSettings_catalogId,
    putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings,

    -- * Destructuring the Response
    PutDataCatalogEncryptionSettingsResponse (..),
    newPutDataCatalogEncryptionSettingsResponse,

    -- * Response Lenses
    putDataCatalogEncryptionSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDataCatalogEncryptionSettings' smart constructor.
data PutDataCatalogEncryptionSettings = PutDataCatalogEncryptionSettings'
  { -- | The ID of the Data Catalog to set the security configuration for. If
    -- none is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The security configuration to set.
    dataCatalogEncryptionSettings :: DataCatalogEncryptionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'putDataCatalogEncryptionSettings_catalogId' - The ID of the Data Catalog to set the security configuration for. If
-- none is provided, the Amazon Web Services account ID is used by default.
--
-- 'dataCatalogEncryptionSettings', 'putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings' - The security configuration to set.
newPutDataCatalogEncryptionSettings ::
  -- | 'dataCatalogEncryptionSettings'
  DataCatalogEncryptionSettings ->
  PutDataCatalogEncryptionSettings
newPutDataCatalogEncryptionSettings
  pDataCatalogEncryptionSettings_ =
    PutDataCatalogEncryptionSettings'
      { catalogId =
          Prelude.Nothing,
        dataCatalogEncryptionSettings =
          pDataCatalogEncryptionSettings_
      }

-- | The ID of the Data Catalog to set the security configuration for. If
-- none is provided, the Amazon Web Services account ID is used by default.
putDataCatalogEncryptionSettings_catalogId :: Lens.Lens' PutDataCatalogEncryptionSettings (Prelude.Maybe Prelude.Text)
putDataCatalogEncryptionSettings_catalogId = Lens.lens (\PutDataCatalogEncryptionSettings' {catalogId} -> catalogId) (\s@PutDataCatalogEncryptionSettings' {} a -> s {catalogId = a} :: PutDataCatalogEncryptionSettings)

-- | The security configuration to set.
putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings :: Lens.Lens' PutDataCatalogEncryptionSettings DataCatalogEncryptionSettings
putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings = Lens.lens (\PutDataCatalogEncryptionSettings' {dataCatalogEncryptionSettings} -> dataCatalogEncryptionSettings) (\s@PutDataCatalogEncryptionSettings' {} a -> s {dataCatalogEncryptionSettings = a} :: PutDataCatalogEncryptionSettings)

instance
  Core.AWSRequest
    PutDataCatalogEncryptionSettings
  where
  type
    AWSResponse PutDataCatalogEncryptionSettings =
      PutDataCatalogEncryptionSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDataCatalogEncryptionSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDataCatalogEncryptionSettings
  where
  hashWithSalt
    _salt
    PutDataCatalogEncryptionSettings' {..} =
      _salt `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` dataCatalogEncryptionSettings

instance
  Prelude.NFData
    PutDataCatalogEncryptionSettings
  where
  rnf PutDataCatalogEncryptionSettings' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf dataCatalogEncryptionSettings

instance
  Data.ToHeaders
    PutDataCatalogEncryptionSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.PutDataCatalogEncryptionSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDataCatalogEncryptionSettings where
  toJSON PutDataCatalogEncryptionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just
              ( "DataCatalogEncryptionSettings"
                  Data..= dataCatalogEncryptionSettings
              )
          ]
      )

instance Data.ToPath PutDataCatalogEncryptionSettings where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PutDataCatalogEncryptionSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDataCatalogEncryptionSettingsResponse' smart constructor.
data PutDataCatalogEncryptionSettingsResponse = PutDataCatalogEncryptionSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataCatalogEncryptionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDataCatalogEncryptionSettingsResponse_httpStatus' - The response's http status code.
newPutDataCatalogEncryptionSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDataCatalogEncryptionSettingsResponse
newPutDataCatalogEncryptionSettingsResponse
  pHttpStatus_ =
    PutDataCatalogEncryptionSettingsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putDataCatalogEncryptionSettingsResponse_httpStatus :: Lens.Lens' PutDataCatalogEncryptionSettingsResponse Prelude.Int
putDataCatalogEncryptionSettingsResponse_httpStatus = Lens.lens (\PutDataCatalogEncryptionSettingsResponse' {httpStatus} -> httpStatus) (\s@PutDataCatalogEncryptionSettingsResponse' {} a -> s {httpStatus = a} :: PutDataCatalogEncryptionSettingsResponse)

instance
  Prelude.NFData
    PutDataCatalogEncryptionSettingsResponse
  where
  rnf PutDataCatalogEncryptionSettingsResponse' {..} =
    Prelude.rnf httpStatus
