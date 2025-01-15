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
-- Module      : Amazonka.IoTWireless.UpdateWirelessGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties of a wireless gateway.
module Amazonka.IoTWireless.UpdateWirelessGateway
  ( -- * Creating a Request
    UpdateWirelessGateway (..),
    newUpdateWirelessGateway,

    -- * Request Lenses
    updateWirelessGateway_description,
    updateWirelessGateway_joinEuiFilters,
    updateWirelessGateway_name,
    updateWirelessGateway_netIdFilters,
    updateWirelessGateway_id,

    -- * Destructuring the Response
    UpdateWirelessGatewayResponse (..),
    newUpdateWirelessGatewayResponse,

    -- * Response Lenses
    updateWirelessGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWirelessGateway' smart constructor.
data UpdateWirelessGateway = UpdateWirelessGateway'
  { -- | A new description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    joinEuiFilters :: Prelude.Maybe [Prelude.NonEmpty Prelude.Text],
    -- | The new name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    netIdFilters :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the resource to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWirelessGateway_description' - A new description of the resource.
--
-- 'joinEuiFilters', 'updateWirelessGateway_joinEuiFilters' - Undocumented member.
--
-- 'name', 'updateWirelessGateway_name' - The new name of the resource.
--
-- 'netIdFilters', 'updateWirelessGateway_netIdFilters' - Undocumented member.
--
-- 'id', 'updateWirelessGateway_id' - The ID of the resource to update.
newUpdateWirelessGateway ::
  -- | 'id'
  Prelude.Text ->
  UpdateWirelessGateway
newUpdateWirelessGateway pId_ =
  UpdateWirelessGateway'
    { description =
        Prelude.Nothing,
      joinEuiFilters = Prelude.Nothing,
      name = Prelude.Nothing,
      netIdFilters = Prelude.Nothing,
      id = pId_
    }

-- | A new description of the resource.
updateWirelessGateway_description :: Lens.Lens' UpdateWirelessGateway (Prelude.Maybe Prelude.Text)
updateWirelessGateway_description = Lens.lens (\UpdateWirelessGateway' {description} -> description) (\s@UpdateWirelessGateway' {} a -> s {description = a} :: UpdateWirelessGateway)

-- | Undocumented member.
updateWirelessGateway_joinEuiFilters :: Lens.Lens' UpdateWirelessGateway (Prelude.Maybe [Prelude.NonEmpty Prelude.Text])
updateWirelessGateway_joinEuiFilters = Lens.lens (\UpdateWirelessGateway' {joinEuiFilters} -> joinEuiFilters) (\s@UpdateWirelessGateway' {} a -> s {joinEuiFilters = a} :: UpdateWirelessGateway) Prelude.. Lens.mapping Lens.coerced

-- | The new name of the resource.
updateWirelessGateway_name :: Lens.Lens' UpdateWirelessGateway (Prelude.Maybe Prelude.Text)
updateWirelessGateway_name = Lens.lens (\UpdateWirelessGateway' {name} -> name) (\s@UpdateWirelessGateway' {} a -> s {name = a} :: UpdateWirelessGateway)

-- | Undocumented member.
updateWirelessGateway_netIdFilters :: Lens.Lens' UpdateWirelessGateway (Prelude.Maybe [Prelude.Text])
updateWirelessGateway_netIdFilters = Lens.lens (\UpdateWirelessGateway' {netIdFilters} -> netIdFilters) (\s@UpdateWirelessGateway' {} a -> s {netIdFilters = a} :: UpdateWirelessGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the resource to update.
updateWirelessGateway_id :: Lens.Lens' UpdateWirelessGateway Prelude.Text
updateWirelessGateway_id = Lens.lens (\UpdateWirelessGateway' {id} -> id) (\s@UpdateWirelessGateway' {} a -> s {id = a} :: UpdateWirelessGateway)

instance Core.AWSRequest UpdateWirelessGateway where
  type
    AWSResponse UpdateWirelessGateway =
      UpdateWirelessGatewayResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWirelessGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWirelessGateway where
  hashWithSalt _salt UpdateWirelessGateway' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` joinEuiFilters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` netIdFilters
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWirelessGateway where
  rnf UpdateWirelessGateway' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf joinEuiFilters `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf netIdFilters `Prelude.seq`
            Prelude.rnf id

instance Data.ToHeaders UpdateWirelessGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateWirelessGateway where
  toJSON UpdateWirelessGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("JoinEuiFilters" Data..=)
              Prelude.<$> joinEuiFilters,
            ("Name" Data..=) Prelude.<$> name,
            ("NetIdFilters" Data..=) Prelude.<$> netIdFilters
          ]
      )

instance Data.ToPath UpdateWirelessGateway where
  toPath UpdateWirelessGateway' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Data.toBS id]

instance Data.ToQuery UpdateWirelessGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWirelessGatewayResponse' smart constructor.
data UpdateWirelessGatewayResponse = UpdateWirelessGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWirelessGatewayResponse_httpStatus' - The response's http status code.
newUpdateWirelessGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWirelessGatewayResponse
newUpdateWirelessGatewayResponse pHttpStatus_ =
  UpdateWirelessGatewayResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateWirelessGatewayResponse_httpStatus :: Lens.Lens' UpdateWirelessGatewayResponse Prelude.Int
updateWirelessGatewayResponse_httpStatus = Lens.lens (\UpdateWirelessGatewayResponse' {httpStatus} -> httpStatus) (\s@UpdateWirelessGatewayResponse' {} a -> s {httpStatus = a} :: UpdateWirelessGatewayResponse)

instance Prelude.NFData UpdateWirelessGatewayResponse where
  rnf UpdateWirelessGatewayResponse' {..} =
    Prelude.rnf httpStatus
