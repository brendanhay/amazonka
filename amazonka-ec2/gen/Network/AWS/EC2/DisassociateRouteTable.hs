{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet or gateway from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in
-- the route table. Instead, it uses the routes in the VPC\'s main route
-- table. For more information about route tables, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.DisassociateRouteTable
  ( -- * Creating a Request
    DisassociateRouteTable (..),
    newDisassociateRouteTable,

    -- * Request Lenses
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- * Destructuring the Response
    DisassociateRouteTableResponse (..),
    newDisassociateRouteTableResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRouteTable' smart constructor.
data DisassociateRouteTable = DisassociateRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The association ID representing the current association between the
    -- route table and subnet or gateway.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'associationId', 'disassociateRouteTable_associationId' - The association ID representing the current association between the
-- route table and subnet or gateway.
newDisassociateRouteTable ::
  -- | 'associationId'
  Prelude.Text ->
  DisassociateRouteTable
newDisassociateRouteTable pAssociationId_ =
  DisassociateRouteTable'
    { dryRun = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateRouteTable_dryRun :: Lens.Lens' DisassociateRouteTable (Prelude.Maybe Prelude.Bool)
disassociateRouteTable_dryRun = Lens.lens (\DisassociateRouteTable' {dryRun} -> dryRun) (\s@DisassociateRouteTable' {} a -> s {dryRun = a} :: DisassociateRouteTable)

-- | The association ID representing the current association between the
-- route table and subnet or gateway.
disassociateRouteTable_associationId :: Lens.Lens' DisassociateRouteTable Prelude.Text
disassociateRouteTable_associationId = Lens.lens (\DisassociateRouteTable' {associationId} -> associationId) (\s@DisassociateRouteTable' {} a -> s {associationId = a} :: DisassociateRouteTable)

instance Prelude.AWSRequest DisassociateRouteTable where
  type
    Rs DisassociateRouteTable =
      DisassociateRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisassociateRouteTableResponse'

instance Prelude.Hashable DisassociateRouteTable

instance Prelude.NFData DisassociateRouteTable

instance Prelude.ToHeaders DisassociateRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisassociateRouteTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateRouteTable where
  toQuery DisassociateRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DisassociateRouteTable" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "AssociationId" Prelude.=: associationId
      ]

-- | /See:/ 'newDisassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse = DisassociateRouteTableResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRouteTableResponse ::
  DisassociateRouteTableResponse
newDisassociateRouteTableResponse =
  DisassociateRouteTableResponse'

instance
  Prelude.NFData
    DisassociateRouteTableResponse
