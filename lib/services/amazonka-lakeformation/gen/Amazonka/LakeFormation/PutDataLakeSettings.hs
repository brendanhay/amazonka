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
-- Module      : Amazonka.LakeFormation.PutDataLakeSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the list of data lake administrators who have admin privileges on
-- all resources managed by Lake Formation. For more information on admin
-- privileges, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/lake-formation-permissions.html Granting Lake Formation Permissions>.
--
-- This API replaces the current list of data lake admins with the new list
-- being passed. To add an admin, fetch the current list and add the new
-- admin to that list and pass that list in this API.
module Amazonka.LakeFormation.PutDataLakeSettings
  ( -- * Creating a Request
    PutDataLakeSettings (..),
    newPutDataLakeSettings,

    -- * Request Lenses
    putDataLakeSettings_catalogId,
    putDataLakeSettings_dataLakeSettings,

    -- * Destructuring the Response
    PutDataLakeSettingsResponse (..),
    newPutDataLakeSettingsResponse,

    -- * Response Lenses
    putDataLakeSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDataLakeSettings' smart constructor.
data PutDataLakeSettings = PutDataLakeSettings'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A structure representing a list of Lake Formation principals designated
    -- as data lake administrators.
    dataLakeSettings :: DataLakeSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataLakeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'putDataLakeSettings_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'dataLakeSettings', 'putDataLakeSettings_dataLakeSettings' - A structure representing a list of Lake Formation principals designated
-- as data lake administrators.
newPutDataLakeSettings ::
  -- | 'dataLakeSettings'
  DataLakeSettings ->
  PutDataLakeSettings
newPutDataLakeSettings pDataLakeSettings_ =
  PutDataLakeSettings'
    { catalogId = Prelude.Nothing,
      dataLakeSettings = pDataLakeSettings_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
putDataLakeSettings_catalogId :: Lens.Lens' PutDataLakeSettings (Prelude.Maybe Prelude.Text)
putDataLakeSettings_catalogId = Lens.lens (\PutDataLakeSettings' {catalogId} -> catalogId) (\s@PutDataLakeSettings' {} a -> s {catalogId = a} :: PutDataLakeSettings)

-- | A structure representing a list of Lake Formation principals designated
-- as data lake administrators.
putDataLakeSettings_dataLakeSettings :: Lens.Lens' PutDataLakeSettings DataLakeSettings
putDataLakeSettings_dataLakeSettings = Lens.lens (\PutDataLakeSettings' {dataLakeSettings} -> dataLakeSettings) (\s@PutDataLakeSettings' {} a -> s {dataLakeSettings = a} :: PutDataLakeSettings)

instance Core.AWSRequest PutDataLakeSettings where
  type
    AWSResponse PutDataLakeSettings =
      PutDataLakeSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDataLakeSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDataLakeSettings where
  hashWithSalt _salt PutDataLakeSettings' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` dataLakeSettings

instance Prelude.NFData PutDataLakeSettings where
  rnf PutDataLakeSettings' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf dataLakeSettings

instance Core.ToHeaders PutDataLakeSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutDataLakeSettings where
  toJSON PutDataLakeSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DataLakeSettings" Core..= dataLakeSettings)
          ]
      )

instance Core.ToPath PutDataLakeSettings where
  toPath = Prelude.const "/PutDataLakeSettings"

instance Core.ToQuery PutDataLakeSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDataLakeSettingsResponse' smart constructor.
data PutDataLakeSettingsResponse = PutDataLakeSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataLakeSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDataLakeSettingsResponse_httpStatus' - The response's http status code.
newPutDataLakeSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDataLakeSettingsResponse
newPutDataLakeSettingsResponse pHttpStatus_ =
  PutDataLakeSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putDataLakeSettingsResponse_httpStatus :: Lens.Lens' PutDataLakeSettingsResponse Prelude.Int
putDataLakeSettingsResponse_httpStatus = Lens.lens (\PutDataLakeSettingsResponse' {httpStatus} -> httpStatus) (\s@PutDataLakeSettingsResponse' {} a -> s {httpStatus = a} :: PutDataLakeSettingsResponse)

instance Prelude.NFData PutDataLakeSettingsResponse where
  rnf PutDataLakeSettingsResponse' {..} =
    Prelude.rnf httpStatus
