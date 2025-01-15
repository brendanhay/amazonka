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
-- Module      : Amazonka.EC2.CreateTransitGatewayPolicyTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway policy table.
module Amazonka.EC2.CreateTransitGatewayPolicyTable
  ( -- * Creating a Request
    CreateTransitGatewayPolicyTable (..),
    newCreateTransitGatewayPolicyTable,

    -- * Request Lenses
    createTransitGatewayPolicyTable_dryRun,
    createTransitGatewayPolicyTable_tagSpecifications,
    createTransitGatewayPolicyTable_transitGatewayId,

    -- * Destructuring the Response
    CreateTransitGatewayPolicyTableResponse (..),
    newCreateTransitGatewayPolicyTableResponse,

    -- * Response Lenses
    createTransitGatewayPolicyTableResponse_transitGatewayPolicyTable,
    createTransitGatewayPolicyTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayPolicyTable' smart constructor.
data CreateTransitGatewayPolicyTable = CreateTransitGatewayPolicyTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags specification for the transit gateway policy table created
    -- during the request.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway used for the policy table.
    transitGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPolicyTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayPolicyTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTransitGatewayPolicyTable_tagSpecifications' - The tags specification for the transit gateway policy table created
-- during the request.
--
-- 'transitGatewayId', 'createTransitGatewayPolicyTable_transitGatewayId' - The ID of the transit gateway used for the policy table.
newCreateTransitGatewayPolicyTable ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  CreateTransitGatewayPolicyTable
newCreateTransitGatewayPolicyTable pTransitGatewayId_ =
  CreateTransitGatewayPolicyTable'
    { dryRun =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayPolicyTable_dryRun :: Lens.Lens' CreateTransitGatewayPolicyTable (Prelude.Maybe Prelude.Bool)
createTransitGatewayPolicyTable_dryRun = Lens.lens (\CreateTransitGatewayPolicyTable' {dryRun} -> dryRun) (\s@CreateTransitGatewayPolicyTable' {} a -> s {dryRun = a} :: CreateTransitGatewayPolicyTable)

-- | The tags specification for the transit gateway policy table created
-- during the request.
createTransitGatewayPolicyTable_tagSpecifications :: Lens.Lens' CreateTransitGatewayPolicyTable (Prelude.Maybe [TagSpecification])
createTransitGatewayPolicyTable_tagSpecifications = Lens.lens (\CreateTransitGatewayPolicyTable' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayPolicyTable' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayPolicyTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway used for the policy table.
createTransitGatewayPolicyTable_transitGatewayId :: Lens.Lens' CreateTransitGatewayPolicyTable Prelude.Text
createTransitGatewayPolicyTable_transitGatewayId = Lens.lens (\CreateTransitGatewayPolicyTable' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayPolicyTable' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayPolicyTable)

instance
  Core.AWSRequest
    CreateTransitGatewayPolicyTable
  where
  type
    AWSResponse CreateTransitGatewayPolicyTable =
      CreateTransitGatewayPolicyTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPolicyTableResponse'
            Prelude.<$> (x Data..@? "transitGatewayPolicyTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayPolicyTable
  where
  hashWithSalt
    _salt
    CreateTransitGatewayPolicyTable' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayId

instance
  Prelude.NFData
    CreateTransitGatewayPolicyTable
  where
  rnf CreateTransitGatewayPolicyTable' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf tagSpecifications `Prelude.seq`
        Prelude.rnf transitGatewayId

instance
  Data.ToHeaders
    CreateTransitGatewayPolicyTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTransitGatewayPolicyTable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTransitGatewayPolicyTable where
  toQuery CreateTransitGatewayPolicyTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayPolicyTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecifications"
              Prelude.<$> tagSpecifications
          ),
        "TransitGatewayId" Data.=: transitGatewayId
      ]

-- | /See:/ 'newCreateTransitGatewayPolicyTableResponse' smart constructor.
data CreateTransitGatewayPolicyTableResponse = CreateTransitGatewayPolicyTableResponse'
  { -- | Describes the created transit gateway policy table.
    transitGatewayPolicyTable :: Prelude.Maybe TransitGatewayPolicyTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPolicyTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPolicyTable', 'createTransitGatewayPolicyTableResponse_transitGatewayPolicyTable' - Describes the created transit gateway policy table.
--
-- 'httpStatus', 'createTransitGatewayPolicyTableResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayPolicyTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayPolicyTableResponse
newCreateTransitGatewayPolicyTableResponse
  pHttpStatus_ =
    CreateTransitGatewayPolicyTableResponse'
      { transitGatewayPolicyTable =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Describes the created transit gateway policy table.
createTransitGatewayPolicyTableResponse_transitGatewayPolicyTable :: Lens.Lens' CreateTransitGatewayPolicyTableResponse (Prelude.Maybe TransitGatewayPolicyTable)
createTransitGatewayPolicyTableResponse_transitGatewayPolicyTable = Lens.lens (\CreateTransitGatewayPolicyTableResponse' {transitGatewayPolicyTable} -> transitGatewayPolicyTable) (\s@CreateTransitGatewayPolicyTableResponse' {} a -> s {transitGatewayPolicyTable = a} :: CreateTransitGatewayPolicyTableResponse)

-- | The response's http status code.
createTransitGatewayPolicyTableResponse_httpStatus :: Lens.Lens' CreateTransitGatewayPolicyTableResponse Prelude.Int
createTransitGatewayPolicyTableResponse_httpStatus = Lens.lens (\CreateTransitGatewayPolicyTableResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayPolicyTableResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayPolicyTableResponse)

instance
  Prelude.NFData
    CreateTransitGatewayPolicyTableResponse
  where
  rnf CreateTransitGatewayPolicyTableResponse' {..} =
    Prelude.rnf transitGatewayPolicyTable `Prelude.seq`
      Prelude.rnf httpStatus
