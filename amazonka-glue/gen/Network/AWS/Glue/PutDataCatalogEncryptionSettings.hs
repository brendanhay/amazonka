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
-- Module      : Network.AWS.Glue.PutDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the security configuration for a specified catalog. After the
-- configuration has been set, the specified encryption is applied to every
-- catalog write thereafter.
module Network.AWS.Glue.PutDataCatalogEncryptionSettings
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutDataCatalogEncryptionSettings' smart constructor.
data PutDataCatalogEncryptionSettings = PutDataCatalogEncryptionSettings'
  { -- | The ID of the Data Catalog to set the security configuration for. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The security configuration to set.
    dataCatalogEncryptionSettings :: DataCatalogEncryptionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutDataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'putDataCatalogEncryptionSettings_catalogId' - The ID of the Data Catalog to set the security configuration for. If
-- none is provided, the AWS account ID is used by default.
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
-- none is provided, the AWS account ID is used by default.
putDataCatalogEncryptionSettings_catalogId :: Lens.Lens' PutDataCatalogEncryptionSettings (Prelude.Maybe Prelude.Text)
putDataCatalogEncryptionSettings_catalogId = Lens.lens (\PutDataCatalogEncryptionSettings' {catalogId} -> catalogId) (\s@PutDataCatalogEncryptionSettings' {} a -> s {catalogId = a} :: PutDataCatalogEncryptionSettings)

-- | The security configuration to set.
putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings :: Lens.Lens' PutDataCatalogEncryptionSettings DataCatalogEncryptionSettings
putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings = Lens.lens (\PutDataCatalogEncryptionSettings' {dataCatalogEncryptionSettings} -> dataCatalogEncryptionSettings) (\s@PutDataCatalogEncryptionSettings' {} a -> s {dataCatalogEncryptionSettings = a} :: PutDataCatalogEncryptionSettings)

instance
  Prelude.AWSRequest
    PutDataCatalogEncryptionSettings
  where
  type
    Rs PutDataCatalogEncryptionSettings =
      PutDataCatalogEncryptionSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDataCatalogEncryptionSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDataCatalogEncryptionSettings

instance
  Prelude.NFData
    PutDataCatalogEncryptionSettings

instance
  Prelude.ToHeaders
    PutDataCatalogEncryptionSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.PutDataCatalogEncryptionSettings" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    PutDataCatalogEncryptionSettings
  where
  toJSON PutDataCatalogEncryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ( "DataCatalogEncryptionSettings"
                  Prelude..= dataCatalogEncryptionSettings
              )
          ]
      )

instance
  Prelude.ToPath
    PutDataCatalogEncryptionSettings
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    PutDataCatalogEncryptionSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDataCatalogEncryptionSettingsResponse' smart constructor.
data PutDataCatalogEncryptionSettingsResponse = PutDataCatalogEncryptionSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
