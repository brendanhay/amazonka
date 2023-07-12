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
-- Module      : Amazonka.LakeFormation.GetDataLakeSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of the data lake administrators of a Lake
-- Formation-managed data lake.
module Amazonka.LakeFormation.GetDataLakeSettings
  ( -- * Creating a Request
    GetDataLakeSettings (..),
    newGetDataLakeSettings,

    -- * Request Lenses
    getDataLakeSettings_catalogId,

    -- * Destructuring the Response
    GetDataLakeSettingsResponse (..),
    newGetDataLakeSettingsResponse,

    -- * Response Lenses
    getDataLakeSettingsResponse_dataLakeSettings,
    getDataLakeSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataLakeSettings' smart constructor.
data GetDataLakeSettings = GetDataLakeSettings'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getDataLakeSettings_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
newGetDataLakeSettings ::
  GetDataLakeSettings
newGetDataLakeSettings =
  GetDataLakeSettings' {catalogId = Prelude.Nothing}

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
getDataLakeSettings_catalogId :: Lens.Lens' GetDataLakeSettings (Prelude.Maybe Prelude.Text)
getDataLakeSettings_catalogId = Lens.lens (\GetDataLakeSettings' {catalogId} -> catalogId) (\s@GetDataLakeSettings' {} a -> s {catalogId = a} :: GetDataLakeSettings)

instance Core.AWSRequest GetDataLakeSettings where
  type
    AWSResponse GetDataLakeSettings =
      GetDataLakeSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataLakeSettingsResponse'
            Prelude.<$> (x Data..?> "DataLakeSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataLakeSettings where
  hashWithSalt _salt GetDataLakeSettings' {..} =
    _salt `Prelude.hashWithSalt` catalogId

instance Prelude.NFData GetDataLakeSettings where
  rnf GetDataLakeSettings' {..} = Prelude.rnf catalogId

instance Data.ToHeaders GetDataLakeSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataLakeSettings where
  toJSON GetDataLakeSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CatalogId" Data..=) Prelude.<$> catalogId]
      )

instance Data.ToPath GetDataLakeSettings where
  toPath = Prelude.const "/GetDataLakeSettings"

instance Data.ToQuery GetDataLakeSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataLakeSettingsResponse' smart constructor.
data GetDataLakeSettingsResponse = GetDataLakeSettingsResponse'
  { -- | A structure representing a list of Lake Formation principals designated
    -- as data lake administrators.
    dataLakeSettings :: Prelude.Maybe DataLakeSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakeSettings', 'getDataLakeSettingsResponse_dataLakeSettings' - A structure representing a list of Lake Formation principals designated
-- as data lake administrators.
--
-- 'httpStatus', 'getDataLakeSettingsResponse_httpStatus' - The response's http status code.
newGetDataLakeSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataLakeSettingsResponse
newGetDataLakeSettingsResponse pHttpStatus_ =
  GetDataLakeSettingsResponse'
    { dataLakeSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure representing a list of Lake Formation principals designated
-- as data lake administrators.
getDataLakeSettingsResponse_dataLakeSettings :: Lens.Lens' GetDataLakeSettingsResponse (Prelude.Maybe DataLakeSettings)
getDataLakeSettingsResponse_dataLakeSettings = Lens.lens (\GetDataLakeSettingsResponse' {dataLakeSettings} -> dataLakeSettings) (\s@GetDataLakeSettingsResponse' {} a -> s {dataLakeSettings = a} :: GetDataLakeSettingsResponse)

-- | The response's http status code.
getDataLakeSettingsResponse_httpStatus :: Lens.Lens' GetDataLakeSettingsResponse Prelude.Int
getDataLakeSettingsResponse_httpStatus = Lens.lens (\GetDataLakeSettingsResponse' {httpStatus} -> httpStatus) (\s@GetDataLakeSettingsResponse' {} a -> s {httpStatus = a} :: GetDataLakeSettingsResponse)

instance Prelude.NFData GetDataLakeSettingsResponse where
  rnf GetDataLakeSettingsResponse' {..} =
    Prelude.rnf dataLakeSettings
      `Prelude.seq` Prelude.rnf httpStatus
