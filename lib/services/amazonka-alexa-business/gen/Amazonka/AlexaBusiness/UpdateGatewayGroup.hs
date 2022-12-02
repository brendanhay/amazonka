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
-- Module      : Amazonka.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not
-- provided, the existing corresponding value is left unmodified.
module Amazonka.AlexaBusiness.UpdateGatewayGroup
  ( -- * Creating a Request
    UpdateGatewayGroup (..),
    newUpdateGatewayGroup,

    -- * Request Lenses
    updateGatewayGroup_name,
    updateGatewayGroup_description,
    updateGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    UpdateGatewayGroupResponse (..),
    newUpdateGatewayGroupResponse,

    -- * Response Lenses
    updateGatewayGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { -- | The updated name of the gateway group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the gateway group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway group to update.
    gatewayGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateGatewayGroup_name' - The updated name of the gateway group.
--
-- 'description', 'updateGatewayGroup_description' - The updated description of the gateway group.
--
-- 'gatewayGroupArn', 'updateGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to update.
newUpdateGatewayGroup ::
  -- | 'gatewayGroupArn'
  Prelude.Text ->
  UpdateGatewayGroup
newUpdateGatewayGroup pGatewayGroupArn_ =
  UpdateGatewayGroup'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      gatewayGroupArn = pGatewayGroupArn_
    }

-- | The updated name of the gateway group.
updateGatewayGroup_name :: Lens.Lens' UpdateGatewayGroup (Prelude.Maybe Prelude.Text)
updateGatewayGroup_name = Lens.lens (\UpdateGatewayGroup' {name} -> name) (\s@UpdateGatewayGroup' {} a -> s {name = a} :: UpdateGatewayGroup)

-- | The updated description of the gateway group.
updateGatewayGroup_description :: Lens.Lens' UpdateGatewayGroup (Prelude.Maybe Prelude.Text)
updateGatewayGroup_description = Lens.lens (\UpdateGatewayGroup' {description} -> description) (\s@UpdateGatewayGroup' {} a -> s {description = a} :: UpdateGatewayGroup)

-- | The ARN of the gateway group to update.
updateGatewayGroup_gatewayGroupArn :: Lens.Lens' UpdateGatewayGroup Prelude.Text
updateGatewayGroup_gatewayGroupArn = Lens.lens (\UpdateGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@UpdateGatewayGroup' {} a -> s {gatewayGroupArn = a} :: UpdateGatewayGroup)

instance Core.AWSRequest UpdateGatewayGroup where
  type
    AWSResponse UpdateGatewayGroup =
      UpdateGatewayGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewayGroup where
  hashWithSalt _salt UpdateGatewayGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` gatewayGroupArn

instance Prelude.NFData UpdateGatewayGroup where
  rnf UpdateGatewayGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf gatewayGroupArn

instance Data.ToHeaders UpdateGatewayGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateGatewayGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGatewayGroup where
  toJSON UpdateGatewayGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("GatewayGroupArn" Data..= gatewayGroupArn)
          ]
      )

instance Data.ToPath UpdateGatewayGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGatewayGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayGroupResponse' smart constructor.
data UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGatewayGroupResponse_httpStatus' - The response's http status code.
newUpdateGatewayGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewayGroupResponse
newUpdateGatewayGroupResponse pHttpStatus_ =
  UpdateGatewayGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateGatewayGroupResponse_httpStatus :: Lens.Lens' UpdateGatewayGroupResponse Prelude.Int
updateGatewayGroupResponse_httpStatus = Lens.lens (\UpdateGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayGroupResponse' {} a -> s {httpStatus = a} :: UpdateGatewayGroupResponse)

instance Prelude.NFData UpdateGatewayGroupResponse where
  rnf UpdateGatewayGroupResponse' {..} =
    Prelude.rnf httpStatus
