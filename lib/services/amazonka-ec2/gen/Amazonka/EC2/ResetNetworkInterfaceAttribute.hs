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
-- Module      : Amazonka.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a network interface attribute. You can specify only one attribute
-- at a time.
module Amazonka.EC2.ResetNetworkInterfaceAttribute
  ( -- * Creating a Request
    ResetNetworkInterfaceAttribute (..),
    newResetNetworkInterfaceAttribute,

    -- * Request Lenses
    resetNetworkInterfaceAttribute_sourceDestCheck,
    resetNetworkInterfaceAttribute_dryRun,
    resetNetworkInterfaceAttribute_networkInterfaceId,

    -- * Destructuring the Response
    ResetNetworkInterfaceAttributeResponse (..),
    newResetNetworkInterfaceAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ResetNetworkInterfaceAttribute.
--
-- /See:/ 'newResetNetworkInterfaceAttribute' smart constructor.
data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute'
  { -- | The source\/destination checking attribute. Resets the value to @true@.
    sourceDestCheck :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetNetworkInterfaceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceDestCheck', 'resetNetworkInterfaceAttribute_sourceDestCheck' - The source\/destination checking attribute. Resets the value to @true@.
--
-- 'dryRun', 'resetNetworkInterfaceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceId', 'resetNetworkInterfaceAttribute_networkInterfaceId' - The ID of the network interface.
newResetNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  ResetNetworkInterfaceAttribute
newResetNetworkInterfaceAttribute
  pNetworkInterfaceId_ =
    ResetNetworkInterfaceAttribute'
      { sourceDestCheck =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | The source\/destination checking attribute. Resets the value to @true@.
resetNetworkInterfaceAttribute_sourceDestCheck :: Lens.Lens' ResetNetworkInterfaceAttribute (Prelude.Maybe Prelude.Text)
resetNetworkInterfaceAttribute_sourceDestCheck = Lens.lens (\ResetNetworkInterfaceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ResetNetworkInterfaceAttribute' {} a -> s {sourceDestCheck = a} :: ResetNetworkInterfaceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetNetworkInterfaceAttribute_dryRun :: Lens.Lens' ResetNetworkInterfaceAttribute (Prelude.Maybe Prelude.Bool)
resetNetworkInterfaceAttribute_dryRun = Lens.lens (\ResetNetworkInterfaceAttribute' {dryRun} -> dryRun) (\s@ResetNetworkInterfaceAttribute' {} a -> s {dryRun = a} :: ResetNetworkInterfaceAttribute)

-- | The ID of the network interface.
resetNetworkInterfaceAttribute_networkInterfaceId :: Lens.Lens' ResetNetworkInterfaceAttribute Prelude.Text
resetNetworkInterfaceAttribute_networkInterfaceId = Lens.lens (\ResetNetworkInterfaceAttribute' {networkInterfaceId} -> networkInterfaceId) (\s@ResetNetworkInterfaceAttribute' {} a -> s {networkInterfaceId = a} :: ResetNetworkInterfaceAttribute)

instance
  Core.AWSRequest
    ResetNetworkInterfaceAttribute
  where
  type
    AWSResponse ResetNetworkInterfaceAttribute =
      ResetNetworkInterfaceAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ResetNetworkInterfaceAttributeResponse'

instance
  Prelude.Hashable
    ResetNetworkInterfaceAttribute
  where
  hashWithSalt
    _salt
    ResetNetworkInterfaceAttribute' {..} =
      _salt `Prelude.hashWithSalt` sourceDestCheck
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` networkInterfaceId

instance
  Prelude.NFData
    ResetNetworkInterfaceAttribute
  where
  rnf ResetNetworkInterfaceAttribute' {..} =
    Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance
  Core.ToHeaders
    ResetNetworkInterfaceAttribute
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResetNetworkInterfaceAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetNetworkInterfaceAttribute where
  toQuery ResetNetworkInterfaceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ResetNetworkInterfaceAttribute" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "SourceDestCheck" Core.=: sourceDestCheck,
        "DryRun" Core.=: dryRun,
        "NetworkInterfaceId" Core.=: networkInterfaceId
      ]

-- | /See:/ 'newResetNetworkInterfaceAttributeResponse' smart constructor.
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetNetworkInterfaceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetNetworkInterfaceAttributeResponse ::
  ResetNetworkInterfaceAttributeResponse
newResetNetworkInterfaceAttributeResponse =
  ResetNetworkInterfaceAttributeResponse'

instance
  Prelude.NFData
    ResetNetworkInterfaceAttributeResponse
  where
  rnf _ = ()
