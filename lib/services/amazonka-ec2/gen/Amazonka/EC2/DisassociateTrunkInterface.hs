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
-- Module      : Amazonka.EC2.DisassociateTrunkInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API action is currently in __limited preview only__. If you are
-- interested in using this feature, contact your account manager.
--
-- Removes an association between a branch network interface with a trunk
-- network interface.
module Amazonka.EC2.DisassociateTrunkInterface
  ( -- * Creating a Request
    DisassociateTrunkInterface (..),
    newDisassociateTrunkInterface,

    -- * Request Lenses
    disassociateTrunkInterface_clientToken,
    disassociateTrunkInterface_dryRun,
    disassociateTrunkInterface_associationId,

    -- * Destructuring the Response
    DisassociateTrunkInterfaceResponse (..),
    newDisassociateTrunkInterfaceResponse,

    -- * Response Lenses
    disassociateTrunkInterfaceResponse_clientToken,
    disassociateTrunkInterfaceResponse_return,
    disassociateTrunkInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateTrunkInterface' smart constructor.
data DisassociateTrunkInterface = DisassociateTrunkInterface'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the association
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrunkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateTrunkInterface_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'dryRun', 'disassociateTrunkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'associationId', 'disassociateTrunkInterface_associationId' - The ID of the association
newDisassociateTrunkInterface ::
  -- | 'associationId'
  Prelude.Text ->
  DisassociateTrunkInterface
newDisassociateTrunkInterface pAssociationId_ =
  DisassociateTrunkInterface'
    { clientToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
disassociateTrunkInterface_clientToken :: Lens.Lens' DisassociateTrunkInterface (Prelude.Maybe Prelude.Text)
disassociateTrunkInterface_clientToken = Lens.lens (\DisassociateTrunkInterface' {clientToken} -> clientToken) (\s@DisassociateTrunkInterface' {} a -> s {clientToken = a} :: DisassociateTrunkInterface)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateTrunkInterface_dryRun :: Lens.Lens' DisassociateTrunkInterface (Prelude.Maybe Prelude.Bool)
disassociateTrunkInterface_dryRun = Lens.lens (\DisassociateTrunkInterface' {dryRun} -> dryRun) (\s@DisassociateTrunkInterface' {} a -> s {dryRun = a} :: DisassociateTrunkInterface)

-- | The ID of the association
disassociateTrunkInterface_associationId :: Lens.Lens' DisassociateTrunkInterface Prelude.Text
disassociateTrunkInterface_associationId = Lens.lens (\DisassociateTrunkInterface' {associationId} -> associationId) (\s@DisassociateTrunkInterface' {} a -> s {associationId = a} :: DisassociateTrunkInterface)

instance Core.AWSRequest DisassociateTrunkInterface where
  type
    AWSResponse DisassociateTrunkInterface =
      DisassociateTrunkInterfaceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTrunkInterfaceResponse'
            Prelude.<$> (x Core..@? "clientToken")
            Prelude.<*> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateTrunkInterface where
  hashWithSalt _salt DisassociateTrunkInterface' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData DisassociateTrunkInterface where
  rnf DisassociateTrunkInterface' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf associationId

instance Core.ToHeaders DisassociateTrunkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisassociateTrunkInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery DisassociateTrunkInterface where
  toQuery DisassociateTrunkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DisassociateTrunkInterface" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Core.=: clientToken,
        "DryRun" Core.=: dryRun,
        "AssociationId" Core.=: associationId
      ]

-- | /See:/ 'newDisassociateTrunkInterfaceResponse' smart constructor.
data DisassociateTrunkInterfaceResponse = DisassociateTrunkInterfaceResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrunkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateTrunkInterfaceResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'return'', 'disassociateTrunkInterfaceResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disassociateTrunkInterfaceResponse_httpStatus' - The response's http status code.
newDisassociateTrunkInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTrunkInterfaceResponse
newDisassociateTrunkInterfaceResponse pHttpStatus_ =
  DisassociateTrunkInterfaceResponse'
    { clientToken =
        Prelude.Nothing,
      return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
disassociateTrunkInterfaceResponse_clientToken :: Lens.Lens' DisassociateTrunkInterfaceResponse (Prelude.Maybe Prelude.Text)
disassociateTrunkInterfaceResponse_clientToken = Lens.lens (\DisassociateTrunkInterfaceResponse' {clientToken} -> clientToken) (\s@DisassociateTrunkInterfaceResponse' {} a -> s {clientToken = a} :: DisassociateTrunkInterfaceResponse)

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disassociateTrunkInterfaceResponse_return :: Lens.Lens' DisassociateTrunkInterfaceResponse (Prelude.Maybe Prelude.Bool)
disassociateTrunkInterfaceResponse_return = Lens.lens (\DisassociateTrunkInterfaceResponse' {return'} -> return') (\s@DisassociateTrunkInterfaceResponse' {} a -> s {return' = a} :: DisassociateTrunkInterfaceResponse)

-- | The response's http status code.
disassociateTrunkInterfaceResponse_httpStatus :: Lens.Lens' DisassociateTrunkInterfaceResponse Prelude.Int
disassociateTrunkInterfaceResponse_httpStatus = Lens.lens (\DisassociateTrunkInterfaceResponse' {httpStatus} -> httpStatus) (\s@DisassociateTrunkInterfaceResponse' {} a -> s {httpStatus = a} :: DisassociateTrunkInterfaceResponse)

instance
  Prelude.NFData
    DisassociateTrunkInterfaceResponse
  where
  rnf DisassociateTrunkInterfaceResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
