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
-- Module      : Amazonka.RDS.ModifyDBProxyEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings for an existing DB proxy endpoint.
module Amazonka.RDS.ModifyDBProxyEndpoint
  ( -- * Creating a Request
    ModifyDBProxyEndpoint (..),
    newModifyDBProxyEndpoint,

    -- * Request Lenses
    modifyDBProxyEndpoint_vpcSecurityGroupIds,
    modifyDBProxyEndpoint_newDBProxyEndpointName,
    modifyDBProxyEndpoint_dbProxyEndpointName,

    -- * Destructuring the Response
    ModifyDBProxyEndpointResponse (..),
    newModifyDBProxyEndpointResponse,

    -- * Response Lenses
    modifyDBProxyEndpointResponse_dbProxyEndpoint,
    modifyDBProxyEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBProxyEndpoint' smart constructor.
data ModifyDBProxyEndpoint = ModifyDBProxyEndpoint'
  { -- | The VPC security group IDs for the DB proxy endpoint. When the DB proxy
    -- endpoint uses a different VPC than the original proxy, you also specify
    -- a different set of security group IDs than for the original proxy.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The new identifier for the @DBProxyEndpoint@. An identifier must begin
    -- with a letter and must contain only ASCII letters, digits, and hyphens;
    -- it can\'t end with a hyphen or contain two consecutive hyphens.
    newDBProxyEndpointName' :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB proxy sociated with the DB proxy endpoint that you
    -- want to modify.
    dbProxyEndpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBProxyEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcSecurityGroupIds', 'modifyDBProxyEndpoint_vpcSecurityGroupIds' - The VPC security group IDs for the DB proxy endpoint. When the DB proxy
-- endpoint uses a different VPC than the original proxy, you also specify
-- a different set of security group IDs than for the original proxy.
--
-- 'newDBProxyEndpointName'', 'modifyDBProxyEndpoint_newDBProxyEndpointName' - The new identifier for the @DBProxyEndpoint@. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'dbProxyEndpointName', 'modifyDBProxyEndpoint_dbProxyEndpointName' - The name of the DB proxy sociated with the DB proxy endpoint that you
-- want to modify.
newModifyDBProxyEndpoint ::
  -- | 'dbProxyEndpointName'
  Prelude.Text ->
  ModifyDBProxyEndpoint
newModifyDBProxyEndpoint pDBProxyEndpointName_ =
  ModifyDBProxyEndpoint'
    { vpcSecurityGroupIds =
        Prelude.Nothing,
      newDBProxyEndpointName' = Prelude.Nothing,
      dbProxyEndpointName = pDBProxyEndpointName_
    }

-- | The VPC security group IDs for the DB proxy endpoint. When the DB proxy
-- endpoint uses a different VPC than the original proxy, you also specify
-- a different set of security group IDs than for the original proxy.
modifyDBProxyEndpoint_vpcSecurityGroupIds :: Lens.Lens' ModifyDBProxyEndpoint (Prelude.Maybe [Prelude.Text])
modifyDBProxyEndpoint_vpcSecurityGroupIds = Lens.lens (\ModifyDBProxyEndpoint' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBProxyEndpoint' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBProxyEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The new identifier for the @DBProxyEndpoint@. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it can\'t end with a hyphen or contain two consecutive hyphens.
modifyDBProxyEndpoint_newDBProxyEndpointName :: Lens.Lens' ModifyDBProxyEndpoint (Prelude.Maybe Prelude.Text)
modifyDBProxyEndpoint_newDBProxyEndpointName = Lens.lens (\ModifyDBProxyEndpoint' {newDBProxyEndpointName'} -> newDBProxyEndpointName') (\s@ModifyDBProxyEndpoint' {} a -> s {newDBProxyEndpointName' = a} :: ModifyDBProxyEndpoint)

-- | The name of the DB proxy sociated with the DB proxy endpoint that you
-- want to modify.
modifyDBProxyEndpoint_dbProxyEndpointName :: Lens.Lens' ModifyDBProxyEndpoint Prelude.Text
modifyDBProxyEndpoint_dbProxyEndpointName = Lens.lens (\ModifyDBProxyEndpoint' {dbProxyEndpointName} -> dbProxyEndpointName) (\s@ModifyDBProxyEndpoint' {} a -> s {dbProxyEndpointName = a} :: ModifyDBProxyEndpoint)

instance Core.AWSRequest ModifyDBProxyEndpoint where
  type
    AWSResponse ModifyDBProxyEndpoint =
      ModifyDBProxyEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBProxyEndpointResult"
      ( \s h x ->
          ModifyDBProxyEndpointResponse'
            Prelude.<$> (x Core..@? "DBProxyEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBProxyEndpoint where
  hashWithSalt _salt ModifyDBProxyEndpoint' {..} =
    _salt `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` newDBProxyEndpointName'
      `Prelude.hashWithSalt` dbProxyEndpointName

instance Prelude.NFData ModifyDBProxyEndpoint where
  rnf ModifyDBProxyEndpoint' {..} =
    Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf newDBProxyEndpointName'
      `Prelude.seq` Prelude.rnf dbProxyEndpointName

instance Core.ToHeaders ModifyDBProxyEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBProxyEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBProxyEndpoint where
  toQuery ModifyDBProxyEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBProxyEndpoint" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "NewDBProxyEndpointName"
          Core.=: newDBProxyEndpointName',
        "DBProxyEndpointName" Core.=: dbProxyEndpointName
      ]

-- | /See:/ 'newModifyDBProxyEndpointResponse' smart constructor.
data ModifyDBProxyEndpointResponse = ModifyDBProxyEndpointResponse'
  { -- | The @DBProxyEndpoint@ object representing the new settings for the DB
    -- proxy endpoint.
    dbProxyEndpoint :: Prelude.Maybe DBProxyEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBProxyEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpoint', 'modifyDBProxyEndpointResponse_dbProxyEndpoint' - The @DBProxyEndpoint@ object representing the new settings for the DB
-- proxy endpoint.
--
-- 'httpStatus', 'modifyDBProxyEndpointResponse_httpStatus' - The response's http status code.
newModifyDBProxyEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBProxyEndpointResponse
newModifyDBProxyEndpointResponse pHttpStatus_ =
  ModifyDBProxyEndpointResponse'
    { dbProxyEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxyEndpoint@ object representing the new settings for the DB
-- proxy endpoint.
modifyDBProxyEndpointResponse_dbProxyEndpoint :: Lens.Lens' ModifyDBProxyEndpointResponse (Prelude.Maybe DBProxyEndpoint)
modifyDBProxyEndpointResponse_dbProxyEndpoint = Lens.lens (\ModifyDBProxyEndpointResponse' {dbProxyEndpoint} -> dbProxyEndpoint) (\s@ModifyDBProxyEndpointResponse' {} a -> s {dbProxyEndpoint = a} :: ModifyDBProxyEndpointResponse)

-- | The response's http status code.
modifyDBProxyEndpointResponse_httpStatus :: Lens.Lens' ModifyDBProxyEndpointResponse Prelude.Int
modifyDBProxyEndpointResponse_httpStatus = Lens.lens (\ModifyDBProxyEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyDBProxyEndpointResponse' {} a -> s {httpStatus = a} :: ModifyDBProxyEndpointResponse)

instance Prelude.NFData ModifyDBProxyEndpointResponse where
  rnf ModifyDBProxyEndpointResponse' {..} =
    Prelude.rnf dbProxyEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
