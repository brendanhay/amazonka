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
-- Module      : Amazonka.EC2.CreateLocalGatewayRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a local gateway route table.
module Amazonka.EC2.CreateLocalGatewayRouteTable
  ( -- * Creating a Request
    CreateLocalGatewayRouteTable (..),
    newCreateLocalGatewayRouteTable,

    -- * Request Lenses
    createLocalGatewayRouteTable_dryRun,
    createLocalGatewayRouteTable_mode,
    createLocalGatewayRouteTable_tagSpecifications,
    createLocalGatewayRouteTable_localGatewayId,

    -- * Destructuring the Response
    CreateLocalGatewayRouteTableResponse (..),
    newCreateLocalGatewayRouteTableResponse,

    -- * Response Lenses
    createLocalGatewayRouteTableResponse_localGatewayRouteTable,
    createLocalGatewayRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocalGatewayRouteTable' smart constructor.
data CreateLocalGatewayRouteTable = CreateLocalGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The mode of the local gateway route table.
    mode :: Prelude.Maybe LocalGatewayRouteTableMode,
    -- | The tags assigned to the local gateway route table.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createLocalGatewayRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'mode', 'createLocalGatewayRouteTable_mode' - The mode of the local gateway route table.
--
-- 'tagSpecifications', 'createLocalGatewayRouteTable_tagSpecifications' - The tags assigned to the local gateway route table.
--
-- 'localGatewayId', 'createLocalGatewayRouteTable_localGatewayId' - The ID of the local gateway.
newCreateLocalGatewayRouteTable ::
  -- | 'localGatewayId'
  Prelude.Text ->
  CreateLocalGatewayRouteTable
newCreateLocalGatewayRouteTable pLocalGatewayId_ =
  CreateLocalGatewayRouteTable'
    { dryRun =
        Prelude.Nothing,
      mode = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      localGatewayId = pLocalGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLocalGatewayRouteTable_dryRun :: Lens.Lens' CreateLocalGatewayRouteTable (Prelude.Maybe Prelude.Bool)
createLocalGatewayRouteTable_dryRun = Lens.lens (\CreateLocalGatewayRouteTable' {dryRun} -> dryRun) (\s@CreateLocalGatewayRouteTable' {} a -> s {dryRun = a} :: CreateLocalGatewayRouteTable)

-- | The mode of the local gateway route table.
createLocalGatewayRouteTable_mode :: Lens.Lens' CreateLocalGatewayRouteTable (Prelude.Maybe LocalGatewayRouteTableMode)
createLocalGatewayRouteTable_mode = Lens.lens (\CreateLocalGatewayRouteTable' {mode} -> mode) (\s@CreateLocalGatewayRouteTable' {} a -> s {mode = a} :: CreateLocalGatewayRouteTable)

-- | The tags assigned to the local gateway route table.
createLocalGatewayRouteTable_tagSpecifications :: Lens.Lens' CreateLocalGatewayRouteTable (Prelude.Maybe [TagSpecification])
createLocalGatewayRouteTable_tagSpecifications = Lens.lens (\CreateLocalGatewayRouteTable' {tagSpecifications} -> tagSpecifications) (\s@CreateLocalGatewayRouteTable' {} a -> s {tagSpecifications = a} :: CreateLocalGatewayRouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the local gateway.
createLocalGatewayRouteTable_localGatewayId :: Lens.Lens' CreateLocalGatewayRouteTable Prelude.Text
createLocalGatewayRouteTable_localGatewayId = Lens.lens (\CreateLocalGatewayRouteTable' {localGatewayId} -> localGatewayId) (\s@CreateLocalGatewayRouteTable' {} a -> s {localGatewayId = a} :: CreateLocalGatewayRouteTable)

instance Core.AWSRequest CreateLocalGatewayRouteTable where
  type
    AWSResponse CreateLocalGatewayRouteTable =
      CreateLocalGatewayRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteTableResponse'
            Prelude.<$> (x Data..@? "localGatewayRouteTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLocalGatewayRouteTable
  where
  hashWithSalt _salt CreateLocalGatewayRouteTable' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` localGatewayId

instance Prelude.NFData CreateLocalGatewayRouteTable where
  rnf CreateLocalGatewayRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf localGatewayId

instance Data.ToHeaders CreateLocalGatewayRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLocalGatewayRouteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocalGatewayRouteTable where
  toQuery CreateLocalGatewayRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateLocalGatewayRouteTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Mode" Data.=: mode,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "LocalGatewayId" Data.=: localGatewayId
      ]

-- | /See:/ 'newCreateLocalGatewayRouteTableResponse' smart constructor.
data CreateLocalGatewayRouteTableResponse = CreateLocalGatewayRouteTableResponse'
  { -- | Information about the local gateway route table.
    localGatewayRouteTable :: Prelude.Maybe LocalGatewayRouteTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTable', 'createLocalGatewayRouteTableResponse_localGatewayRouteTable' - Information about the local gateway route table.
--
-- 'httpStatus', 'createLocalGatewayRouteTableResponse_httpStatus' - The response's http status code.
newCreateLocalGatewayRouteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocalGatewayRouteTableResponse
newCreateLocalGatewayRouteTableResponse pHttpStatus_ =
  CreateLocalGatewayRouteTableResponse'
    { localGatewayRouteTable =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the local gateway route table.
createLocalGatewayRouteTableResponse_localGatewayRouteTable :: Lens.Lens' CreateLocalGatewayRouteTableResponse (Prelude.Maybe LocalGatewayRouteTable)
createLocalGatewayRouteTableResponse_localGatewayRouteTable = Lens.lens (\CreateLocalGatewayRouteTableResponse' {localGatewayRouteTable} -> localGatewayRouteTable) (\s@CreateLocalGatewayRouteTableResponse' {} a -> s {localGatewayRouteTable = a} :: CreateLocalGatewayRouteTableResponse)

-- | The response's http status code.
createLocalGatewayRouteTableResponse_httpStatus :: Lens.Lens' CreateLocalGatewayRouteTableResponse Prelude.Int
createLocalGatewayRouteTableResponse_httpStatus = Lens.lens (\CreateLocalGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@CreateLocalGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: CreateLocalGatewayRouteTableResponse)

instance
  Prelude.NFData
    CreateLocalGatewayRouteTableResponse
  where
  rnf CreateLocalGatewayRouteTableResponse' {..} =
    Prelude.rnf localGatewayRouteTable
      `Prelude.seq` Prelude.rnf httpStatus
