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
-- Module      : Amazonka.EC2.AssociateTrunkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API action is currently in __limited preview only__. If you are
-- interested in using this feature, contact your account manager.
--
-- Associates a branch network interface with a trunk network interface.
--
-- Before you create the association, run the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html create-network-interface>
-- command and set @--interface-type@ to @trunk@. You must also create a
-- network interface for each branch network interface that you want to
-- associate with the trunk network interface.
module Amazonka.EC2.AssociateTrunkInterface
  ( -- * Creating a Request
    AssociateTrunkInterface (..),
    newAssociateTrunkInterface,

    -- * Request Lenses
    associateTrunkInterface_clientToken,
    associateTrunkInterface_dryRun,
    associateTrunkInterface_greKey,
    associateTrunkInterface_vlanId,
    associateTrunkInterface_branchInterfaceId,
    associateTrunkInterface_trunkInterfaceId,

    -- * Destructuring the Response
    AssociateTrunkInterfaceResponse (..),
    newAssociateTrunkInterfaceResponse,

    -- * Response Lenses
    associateTrunkInterfaceResponse_clientToken,
    associateTrunkInterfaceResponse_interfaceAssociation,
    associateTrunkInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTrunkInterface' smart constructor.
data AssociateTrunkInterface = AssociateTrunkInterface'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The application key. This applies to the GRE protocol.
    greKey :: Prelude.Maybe Prelude.Int,
    -- | The ID of the VLAN. This applies to the VLAN protocol.
    vlanId :: Prelude.Maybe Prelude.Int,
    -- | The ID of the branch network interface.
    branchInterfaceId :: Prelude.Text,
    -- | The ID of the trunk network interface.
    trunkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrunkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateTrunkInterface_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'dryRun', 'associateTrunkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'greKey', 'associateTrunkInterface_greKey' - The application key. This applies to the GRE protocol.
--
-- 'vlanId', 'associateTrunkInterface_vlanId' - The ID of the VLAN. This applies to the VLAN protocol.
--
-- 'branchInterfaceId', 'associateTrunkInterface_branchInterfaceId' - The ID of the branch network interface.
--
-- 'trunkInterfaceId', 'associateTrunkInterface_trunkInterfaceId' - The ID of the trunk network interface.
newAssociateTrunkInterface ::
  -- | 'branchInterfaceId'
  Prelude.Text ->
  -- | 'trunkInterfaceId'
  Prelude.Text ->
  AssociateTrunkInterface
newAssociateTrunkInterface
  pBranchInterfaceId_
  pTrunkInterfaceId_ =
    AssociateTrunkInterface'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        greKey = Prelude.Nothing,
        vlanId = Prelude.Nothing,
        branchInterfaceId = pBranchInterfaceId_,
        trunkInterfaceId = pTrunkInterfaceId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
associateTrunkInterface_clientToken :: Lens.Lens' AssociateTrunkInterface (Prelude.Maybe Prelude.Text)
associateTrunkInterface_clientToken = Lens.lens (\AssociateTrunkInterface' {clientToken} -> clientToken) (\s@AssociateTrunkInterface' {} a -> s {clientToken = a} :: AssociateTrunkInterface)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateTrunkInterface_dryRun :: Lens.Lens' AssociateTrunkInterface (Prelude.Maybe Prelude.Bool)
associateTrunkInterface_dryRun = Lens.lens (\AssociateTrunkInterface' {dryRun} -> dryRun) (\s@AssociateTrunkInterface' {} a -> s {dryRun = a} :: AssociateTrunkInterface)

-- | The application key. This applies to the GRE protocol.
associateTrunkInterface_greKey :: Lens.Lens' AssociateTrunkInterface (Prelude.Maybe Prelude.Int)
associateTrunkInterface_greKey = Lens.lens (\AssociateTrunkInterface' {greKey} -> greKey) (\s@AssociateTrunkInterface' {} a -> s {greKey = a} :: AssociateTrunkInterface)

-- | The ID of the VLAN. This applies to the VLAN protocol.
associateTrunkInterface_vlanId :: Lens.Lens' AssociateTrunkInterface (Prelude.Maybe Prelude.Int)
associateTrunkInterface_vlanId = Lens.lens (\AssociateTrunkInterface' {vlanId} -> vlanId) (\s@AssociateTrunkInterface' {} a -> s {vlanId = a} :: AssociateTrunkInterface)

-- | The ID of the branch network interface.
associateTrunkInterface_branchInterfaceId :: Lens.Lens' AssociateTrunkInterface Prelude.Text
associateTrunkInterface_branchInterfaceId = Lens.lens (\AssociateTrunkInterface' {branchInterfaceId} -> branchInterfaceId) (\s@AssociateTrunkInterface' {} a -> s {branchInterfaceId = a} :: AssociateTrunkInterface)

-- | The ID of the trunk network interface.
associateTrunkInterface_trunkInterfaceId :: Lens.Lens' AssociateTrunkInterface Prelude.Text
associateTrunkInterface_trunkInterfaceId = Lens.lens (\AssociateTrunkInterface' {trunkInterfaceId} -> trunkInterfaceId) (\s@AssociateTrunkInterface' {} a -> s {trunkInterfaceId = a} :: AssociateTrunkInterface)

instance Core.AWSRequest AssociateTrunkInterface where
  type
    AWSResponse AssociateTrunkInterface =
      AssociateTrunkInterfaceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTrunkInterfaceResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "interfaceAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateTrunkInterface where
  hashWithSalt _salt AssociateTrunkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` greKey
      `Prelude.hashWithSalt` vlanId
      `Prelude.hashWithSalt` branchInterfaceId
      `Prelude.hashWithSalt` trunkInterfaceId

instance Prelude.NFData AssociateTrunkInterface where
  rnf AssociateTrunkInterface' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf greKey
      `Prelude.seq` Prelude.rnf vlanId
      `Prelude.seq` Prelude.rnf branchInterfaceId
      `Prelude.seq` Prelude.rnf trunkInterfaceId

instance Data.ToHeaders AssociateTrunkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateTrunkInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateTrunkInterface where
  toQuery AssociateTrunkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateTrunkInterface" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "GreKey" Data.=: greKey,
        "VlanId" Data.=: vlanId,
        "BranchInterfaceId" Data.=: branchInterfaceId,
        "TrunkInterfaceId" Data.=: trunkInterfaceId
      ]

-- | /See:/ 'newAssociateTrunkInterfaceResponse' smart constructor.
data AssociateTrunkInterfaceResponse = AssociateTrunkInterfaceResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the association between the trunk network interface
    -- and branch network interface.
    interfaceAssociation :: Prelude.Maybe TrunkInterfaceAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrunkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateTrunkInterfaceResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'interfaceAssociation', 'associateTrunkInterfaceResponse_interfaceAssociation' - Information about the association between the trunk network interface
-- and branch network interface.
--
-- 'httpStatus', 'associateTrunkInterfaceResponse_httpStatus' - The response's http status code.
newAssociateTrunkInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTrunkInterfaceResponse
newAssociateTrunkInterfaceResponse pHttpStatus_ =
  AssociateTrunkInterfaceResponse'
    { clientToken =
        Prelude.Nothing,
      interfaceAssociation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
associateTrunkInterfaceResponse_clientToken :: Lens.Lens' AssociateTrunkInterfaceResponse (Prelude.Maybe Prelude.Text)
associateTrunkInterfaceResponse_clientToken = Lens.lens (\AssociateTrunkInterfaceResponse' {clientToken} -> clientToken) (\s@AssociateTrunkInterfaceResponse' {} a -> s {clientToken = a} :: AssociateTrunkInterfaceResponse)

-- | Information about the association between the trunk network interface
-- and branch network interface.
associateTrunkInterfaceResponse_interfaceAssociation :: Lens.Lens' AssociateTrunkInterfaceResponse (Prelude.Maybe TrunkInterfaceAssociation)
associateTrunkInterfaceResponse_interfaceAssociation = Lens.lens (\AssociateTrunkInterfaceResponse' {interfaceAssociation} -> interfaceAssociation) (\s@AssociateTrunkInterfaceResponse' {} a -> s {interfaceAssociation = a} :: AssociateTrunkInterfaceResponse)

-- | The response's http status code.
associateTrunkInterfaceResponse_httpStatus :: Lens.Lens' AssociateTrunkInterfaceResponse Prelude.Int
associateTrunkInterfaceResponse_httpStatus = Lens.lens (\AssociateTrunkInterfaceResponse' {httpStatus} -> httpStatus) (\s@AssociateTrunkInterfaceResponse' {} a -> s {httpStatus = a} :: AssociateTrunkInterfaceResponse)

instance
  Prelude.NFData
    AssociateTrunkInterfaceResponse
  where
  rnf AssociateTrunkInterfaceResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf interfaceAssociation
      `Prelude.seq` Prelude.rnf httpStatus
