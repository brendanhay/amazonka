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
-- Module      : Amazonka.IoTWireless.UpdateMulticastGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties of a multicast group session.
module Amazonka.IoTWireless.UpdateMulticastGroup
  ( -- * Creating a Request
    UpdateMulticastGroup (..),
    newUpdateMulticastGroup,

    -- * Request Lenses
    updateMulticastGroup_description,
    updateMulticastGroup_loRaWAN,
    updateMulticastGroup_name,
    updateMulticastGroup_id,

    -- * Destructuring the Response
    UpdateMulticastGroupResponse (..),
    newUpdateMulticastGroupResponse,

    -- * Response Lenses
    updateMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMulticastGroup' smart constructor.
data UpdateMulticastGroup = UpdateMulticastGroup'
  { description :: Prelude.Maybe Prelude.Text,
    loRaWAN :: Prelude.Maybe LoRaWANMulticast,
    name :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateMulticastGroup_description' - Undocumented member.
--
-- 'loRaWAN', 'updateMulticastGroup_loRaWAN' - Undocumented member.
--
-- 'name', 'updateMulticastGroup_name' - Undocumented member.
--
-- 'id', 'updateMulticastGroup_id' - Undocumented member.
newUpdateMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  UpdateMulticastGroup
newUpdateMulticastGroup pId_ =
  UpdateMulticastGroup'
    { description =
        Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateMulticastGroup_description :: Lens.Lens' UpdateMulticastGroup (Prelude.Maybe Prelude.Text)
updateMulticastGroup_description = Lens.lens (\UpdateMulticastGroup' {description} -> description) (\s@UpdateMulticastGroup' {} a -> s {description = a} :: UpdateMulticastGroup)

-- | Undocumented member.
updateMulticastGroup_loRaWAN :: Lens.Lens' UpdateMulticastGroup (Prelude.Maybe LoRaWANMulticast)
updateMulticastGroup_loRaWAN = Lens.lens (\UpdateMulticastGroup' {loRaWAN} -> loRaWAN) (\s@UpdateMulticastGroup' {} a -> s {loRaWAN = a} :: UpdateMulticastGroup)

-- | Undocumented member.
updateMulticastGroup_name :: Lens.Lens' UpdateMulticastGroup (Prelude.Maybe Prelude.Text)
updateMulticastGroup_name = Lens.lens (\UpdateMulticastGroup' {name} -> name) (\s@UpdateMulticastGroup' {} a -> s {name = a} :: UpdateMulticastGroup)

-- | Undocumented member.
updateMulticastGroup_id :: Lens.Lens' UpdateMulticastGroup Prelude.Text
updateMulticastGroup_id = Lens.lens (\UpdateMulticastGroup' {id} -> id) (\s@UpdateMulticastGroup' {} a -> s {id = a} :: UpdateMulticastGroup)

instance Core.AWSRequest UpdateMulticastGroup where
  type
    AWSResponse UpdateMulticastGroup =
      UpdateMulticastGroupResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMulticastGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMulticastGroup where
  hashWithSalt _salt UpdateMulticastGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateMulticastGroup where
  rnf UpdateMulticastGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateMulticastGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateMulticastGroup where
  toJSON UpdateMulticastGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateMulticastGroup where
  toPath UpdateMulticastGroup' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id]

instance Data.ToQuery UpdateMulticastGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMulticastGroupResponse' smart constructor.
data UpdateMulticastGroupResponse = UpdateMulticastGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMulticastGroupResponse_httpStatus' - The response's http status code.
newUpdateMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMulticastGroupResponse
newUpdateMulticastGroupResponse pHttpStatus_ =
  UpdateMulticastGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateMulticastGroupResponse_httpStatus :: Lens.Lens' UpdateMulticastGroupResponse Prelude.Int
updateMulticastGroupResponse_httpStatus = Lens.lens (\UpdateMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateMulticastGroupResponse' {} a -> s {httpStatus = a} :: UpdateMulticastGroupResponse)

instance Prelude.NFData UpdateMulticastGroupResponse where
  rnf UpdateMulticastGroupResponse' {..} =
    Prelude.rnf httpStatus
