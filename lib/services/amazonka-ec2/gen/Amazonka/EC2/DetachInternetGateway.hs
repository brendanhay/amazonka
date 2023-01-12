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
-- Module      : Amazonka.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an internet gateway from a VPC, disabling connectivity between
-- the internet and the VPC. The VPC must not contain any running instances
-- with Elastic IP addresses or public IPv4 addresses.
module Amazonka.EC2.DetachInternetGateway
  ( -- * Creating a Request
    DetachInternetGateway (..),
    newDetachInternetGateway,

    -- * Request Lenses
    detachInternetGateway_dryRun,
    detachInternetGateway_internetGatewayId,
    detachInternetGateway_vpcId,

    -- * Destructuring the Response
    DetachInternetGatewayResponse (..),
    newDetachInternetGatewayResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachInternetGateway' smart constructor.
data DetachInternetGateway = DetachInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the internet gateway.
    internetGatewayId :: Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'internetGatewayId', 'detachInternetGateway_internetGatewayId' - The ID of the internet gateway.
--
-- 'vpcId', 'detachInternetGateway_vpcId' - The ID of the VPC.
newDetachInternetGateway ::
  -- | 'internetGatewayId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  DetachInternetGateway
newDetachInternetGateway pInternetGatewayId_ pVpcId_ =
  DetachInternetGateway'
    { dryRun = Prelude.Nothing,
      internetGatewayId = pInternetGatewayId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachInternetGateway_dryRun :: Lens.Lens' DetachInternetGateway (Prelude.Maybe Prelude.Bool)
detachInternetGateway_dryRun = Lens.lens (\DetachInternetGateway' {dryRun} -> dryRun) (\s@DetachInternetGateway' {} a -> s {dryRun = a} :: DetachInternetGateway)

-- | The ID of the internet gateway.
detachInternetGateway_internetGatewayId :: Lens.Lens' DetachInternetGateway Prelude.Text
detachInternetGateway_internetGatewayId = Lens.lens (\DetachInternetGateway' {internetGatewayId} -> internetGatewayId) (\s@DetachInternetGateway' {} a -> s {internetGatewayId = a} :: DetachInternetGateway)

-- | The ID of the VPC.
detachInternetGateway_vpcId :: Lens.Lens' DetachInternetGateway Prelude.Text
detachInternetGateway_vpcId = Lens.lens (\DetachInternetGateway' {vpcId} -> vpcId) (\s@DetachInternetGateway' {} a -> s {vpcId = a} :: DetachInternetGateway)

instance Core.AWSRequest DetachInternetGateway where
  type
    AWSResponse DetachInternetGateway =
      DetachInternetGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DetachInternetGatewayResponse'

instance Prelude.Hashable DetachInternetGateway where
  hashWithSalt _salt DetachInternetGateway' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` internetGatewayId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DetachInternetGateway where
  rnf DetachInternetGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf internetGatewayId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders DetachInternetGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachInternetGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachInternetGateway where
  toQuery DetachInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetachInternetGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InternetGatewayId" Data.=: internetGatewayId,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDetachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse = DetachInternetGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachInternetGatewayResponse ::
  DetachInternetGatewayResponse
newDetachInternetGatewayResponse =
  DetachInternetGatewayResponse'

instance Prelude.NFData DetachInternetGatewayResponse where
  rnf _ = ()
