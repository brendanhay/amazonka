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
-- Module      : Amazonka.CleanRooms.UpdateConfiguredTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a configured table.
module Amazonka.CleanRooms.UpdateConfiguredTable
  ( -- * Creating a Request
    UpdateConfiguredTable (..),
    newUpdateConfiguredTable,

    -- * Request Lenses
    updateConfiguredTable_description,
    updateConfiguredTable_name,
    updateConfiguredTable_configuredTableIdentifier,

    -- * Destructuring the Response
    UpdateConfiguredTableResponse (..),
    newUpdateConfiguredTableResponse,

    -- * Response Lenses
    updateConfiguredTableResponse_httpStatus,
    updateConfiguredTableResponse_configuredTable,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfiguredTable' smart constructor.
data UpdateConfiguredTable = UpdateConfiguredTable'
  { -- | A new description for the configured table.
    description :: Prelude.Maybe Prelude.Text,
    -- | A new name for the configured table.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the configured table to update. Currently accepts the
    -- configured table ID.
    configuredTableIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateConfiguredTable_description' - A new description for the configured table.
--
-- 'name', 'updateConfiguredTable_name' - A new name for the configured table.
--
-- 'configuredTableIdentifier', 'updateConfiguredTable_configuredTableIdentifier' - The identifier for the configured table to update. Currently accepts the
-- configured table ID.
newUpdateConfiguredTable ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  UpdateConfiguredTable
newUpdateConfiguredTable pConfiguredTableIdentifier_ =
  UpdateConfiguredTable'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      configuredTableIdentifier =
        pConfiguredTableIdentifier_
    }

-- | A new description for the configured table.
updateConfiguredTable_description :: Lens.Lens' UpdateConfiguredTable (Prelude.Maybe Prelude.Text)
updateConfiguredTable_description = Lens.lens (\UpdateConfiguredTable' {description} -> description) (\s@UpdateConfiguredTable' {} a -> s {description = a} :: UpdateConfiguredTable)

-- | A new name for the configured table.
updateConfiguredTable_name :: Lens.Lens' UpdateConfiguredTable (Prelude.Maybe Prelude.Text)
updateConfiguredTable_name = Lens.lens (\UpdateConfiguredTable' {name} -> name) (\s@UpdateConfiguredTable' {} a -> s {name = a} :: UpdateConfiguredTable)

-- | The identifier for the configured table to update. Currently accepts the
-- configured table ID.
updateConfiguredTable_configuredTableIdentifier :: Lens.Lens' UpdateConfiguredTable Prelude.Text
updateConfiguredTable_configuredTableIdentifier = Lens.lens (\UpdateConfiguredTable' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@UpdateConfiguredTable' {} a -> s {configuredTableIdentifier = a} :: UpdateConfiguredTable)

instance Core.AWSRequest UpdateConfiguredTable where
  type
    AWSResponse UpdateConfiguredTable =
      UpdateConfiguredTableResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConfiguredTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTable")
      )

instance Prelude.Hashable UpdateConfiguredTable where
  hashWithSalt _salt UpdateConfiguredTable' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` configuredTableIdentifier

instance Prelude.NFData UpdateConfiguredTable where
  rnf UpdateConfiguredTable' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf configuredTableIdentifier

instance Data.ToHeaders UpdateConfiguredTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConfiguredTable where
  toJSON UpdateConfiguredTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateConfiguredTable where
  toPath UpdateConfiguredTable' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier
      ]

instance Data.ToQuery UpdateConfiguredTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConfiguredTableResponse' smart constructor.
data UpdateConfiguredTableResponse = UpdateConfiguredTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated configured table.
    configuredTable :: ConfiguredTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfiguredTableResponse_httpStatus' - The response's http status code.
--
-- 'configuredTable', 'updateConfiguredTableResponse_configuredTable' - The updated configured table.
newUpdateConfiguredTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTable'
  ConfiguredTable ->
  UpdateConfiguredTableResponse
newUpdateConfiguredTableResponse
  pHttpStatus_
  pConfiguredTable_ =
    UpdateConfiguredTableResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTable = pConfiguredTable_
      }

-- | The response's http status code.
updateConfiguredTableResponse_httpStatus :: Lens.Lens' UpdateConfiguredTableResponse Prelude.Int
updateConfiguredTableResponse_httpStatus = Lens.lens (\UpdateConfiguredTableResponse' {httpStatus} -> httpStatus) (\s@UpdateConfiguredTableResponse' {} a -> s {httpStatus = a} :: UpdateConfiguredTableResponse)

-- | The updated configured table.
updateConfiguredTableResponse_configuredTable :: Lens.Lens' UpdateConfiguredTableResponse ConfiguredTable
updateConfiguredTableResponse_configuredTable = Lens.lens (\UpdateConfiguredTableResponse' {configuredTable} -> configuredTable) (\s@UpdateConfiguredTableResponse' {} a -> s {configuredTable = a} :: UpdateConfiguredTableResponse)

instance Prelude.NFData UpdateConfiguredTableResponse where
  rnf UpdateConfiguredTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTable
