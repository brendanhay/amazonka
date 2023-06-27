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
-- Module      : Amazonka.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit
-- gateway, the modified options are applied to new transit gateway
-- attachments only. Your existing transit gateway attachments are not
-- modified.
module Amazonka.EC2.ModifyTransitGateway
  ( -- * Creating a Request
    ModifyTransitGateway (..),
    newModifyTransitGateway,

    -- * Request Lenses
    modifyTransitGateway_description,
    modifyTransitGateway_dryRun,
    modifyTransitGateway_options,
    modifyTransitGateway_transitGatewayId,

    -- * Destructuring the Response
    ModifyTransitGatewayResponse (..),
    newModifyTransitGatewayResponse,

    -- * Response Lenses
    modifyTransitGatewayResponse_transitGateway,
    modifyTransitGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { -- | The description for the transit gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The options to modify.
    options :: Prelude.Maybe ModifyTransitGatewayOptions,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyTransitGateway_description' - The description for the transit gateway.
--
-- 'dryRun', 'modifyTransitGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'modifyTransitGateway_options' - The options to modify.
--
-- 'transitGatewayId', 'modifyTransitGateway_transitGatewayId' - The ID of the transit gateway.
newModifyTransitGateway ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  ModifyTransitGateway
newModifyTransitGateway pTransitGatewayId_ =
  ModifyTransitGateway'
    { description =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      options = Prelude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | The description for the transit gateway.
modifyTransitGateway_description :: Lens.Lens' ModifyTransitGateway (Prelude.Maybe Prelude.Text)
modifyTransitGateway_description = Lens.lens (\ModifyTransitGateway' {description} -> description) (\s@ModifyTransitGateway' {} a -> s {description = a} :: ModifyTransitGateway)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGateway_dryRun :: Lens.Lens' ModifyTransitGateway (Prelude.Maybe Prelude.Bool)
modifyTransitGateway_dryRun = Lens.lens (\ModifyTransitGateway' {dryRun} -> dryRun) (\s@ModifyTransitGateway' {} a -> s {dryRun = a} :: ModifyTransitGateway)

-- | The options to modify.
modifyTransitGateway_options :: Lens.Lens' ModifyTransitGateway (Prelude.Maybe ModifyTransitGatewayOptions)
modifyTransitGateway_options = Lens.lens (\ModifyTransitGateway' {options} -> options) (\s@ModifyTransitGateway' {} a -> s {options = a} :: ModifyTransitGateway)

-- | The ID of the transit gateway.
modifyTransitGateway_transitGatewayId :: Lens.Lens' ModifyTransitGateway Prelude.Text
modifyTransitGateway_transitGatewayId = Lens.lens (\ModifyTransitGateway' {transitGatewayId} -> transitGatewayId) (\s@ModifyTransitGateway' {} a -> s {transitGatewayId = a} :: ModifyTransitGateway)

instance Core.AWSRequest ModifyTransitGateway where
  type
    AWSResponse ModifyTransitGateway =
      ModifyTransitGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayResponse'
            Prelude.<$> (x Data..@? "transitGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyTransitGateway where
  hashWithSalt _salt ModifyTransitGateway' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` transitGatewayId

instance Prelude.NFData ModifyTransitGateway where
  rnf ModifyTransitGateway' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf transitGatewayId

instance Data.ToHeaders ModifyTransitGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyTransitGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyTransitGateway where
  toQuery ModifyTransitGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyTransitGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "Options" Data.=: options,
        "TransitGatewayId" Data.=: transitGatewayId
      ]

-- | /See:/ 'newModifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { -- | Information about the transit gateway.
    transitGateway :: Prelude.Maybe TransitGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGateway', 'modifyTransitGatewayResponse_transitGateway' - Information about the transit gateway.
--
-- 'httpStatus', 'modifyTransitGatewayResponse_httpStatus' - The response's http status code.
newModifyTransitGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyTransitGatewayResponse
newModifyTransitGatewayResponse pHttpStatus_ =
  ModifyTransitGatewayResponse'
    { transitGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the transit gateway.
modifyTransitGatewayResponse_transitGateway :: Lens.Lens' ModifyTransitGatewayResponse (Prelude.Maybe TransitGateway)
modifyTransitGatewayResponse_transitGateway = Lens.lens (\ModifyTransitGatewayResponse' {transitGateway} -> transitGateway) (\s@ModifyTransitGatewayResponse' {} a -> s {transitGateway = a} :: ModifyTransitGatewayResponse)

-- | The response's http status code.
modifyTransitGatewayResponse_httpStatus :: Lens.Lens' ModifyTransitGatewayResponse Prelude.Int
modifyTransitGatewayResponse_httpStatus = Lens.lens (\ModifyTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@ModifyTransitGatewayResponse' {} a -> s {httpStatus = a} :: ModifyTransitGatewayResponse)

instance Prelude.NFData ModifyTransitGatewayResponse where
  rnf ModifyTransitGatewayResponse' {..} =
    Prelude.rnf transitGateway
      `Prelude.seq` Prelude.rnf httpStatus
