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
-- Module      : Amazonka.EC2.AssociateDhcpOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of DHCP options (that you\'ve previously created) with
-- the specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and
-- all new instances that you launch in that VPC use the options. You
-- don\'t need to restart or relaunch the instances. They automatically
-- pick up the changes within a few hours, depending on how frequently the
-- instance renews its DHCP lease. You can explicitly renew the lease using
-- the operating system on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP options sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssociateDhcpOptions
  ( -- * Creating a Request
    AssociateDhcpOptions (..),
    newAssociateDhcpOptions,

    -- * Request Lenses
    associateDhcpOptions_dryRun,
    associateDhcpOptions_dhcpOptionsId,
    associateDhcpOptions_vpcId,

    -- * Destructuring the Response
    AssociateDhcpOptionsResponse (..),
    newAssociateDhcpOptionsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDhcpOptions' smart constructor.
data AssociateDhcpOptions = AssociateDhcpOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the DHCP options set, or @default@ to associate no DHCP
    -- options with the VPC.
    dhcpOptionsId :: Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateDhcpOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'dhcpOptionsId', 'associateDhcpOptions_dhcpOptionsId' - The ID of the DHCP options set, or @default@ to associate no DHCP
-- options with the VPC.
--
-- 'vpcId', 'associateDhcpOptions_vpcId' - The ID of the VPC.
newAssociateDhcpOptions ::
  -- | 'dhcpOptionsId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  AssociateDhcpOptions
newAssociateDhcpOptions pDhcpOptionsId_ pVpcId_ =
  AssociateDhcpOptions'
    { dryRun = Prelude.Nothing,
      dhcpOptionsId = pDhcpOptionsId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateDhcpOptions_dryRun :: Lens.Lens' AssociateDhcpOptions (Prelude.Maybe Prelude.Bool)
associateDhcpOptions_dryRun = Lens.lens (\AssociateDhcpOptions' {dryRun} -> dryRun) (\s@AssociateDhcpOptions' {} a -> s {dryRun = a} :: AssociateDhcpOptions)

-- | The ID of the DHCP options set, or @default@ to associate no DHCP
-- options with the VPC.
associateDhcpOptions_dhcpOptionsId :: Lens.Lens' AssociateDhcpOptions Prelude.Text
associateDhcpOptions_dhcpOptionsId = Lens.lens (\AssociateDhcpOptions' {dhcpOptionsId} -> dhcpOptionsId) (\s@AssociateDhcpOptions' {} a -> s {dhcpOptionsId = a} :: AssociateDhcpOptions)

-- | The ID of the VPC.
associateDhcpOptions_vpcId :: Lens.Lens' AssociateDhcpOptions Prelude.Text
associateDhcpOptions_vpcId = Lens.lens (\AssociateDhcpOptions' {vpcId} -> vpcId) (\s@AssociateDhcpOptions' {} a -> s {vpcId = a} :: AssociateDhcpOptions)

instance Core.AWSRequest AssociateDhcpOptions where
  type
    AWSResponse AssociateDhcpOptions =
      AssociateDhcpOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AssociateDhcpOptionsResponse'

instance Prelude.Hashable AssociateDhcpOptions where
  hashWithSalt _salt AssociateDhcpOptions' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` dhcpOptionsId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AssociateDhcpOptions where
  rnf AssociateDhcpOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf dhcpOptionsId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders AssociateDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateDhcpOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateDhcpOptions where
  toQuery AssociateDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateDhcpOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "DhcpOptionsId" Data.=: dhcpOptionsId,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newAssociateDhcpOptionsResponse' smart constructor.
data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateDhcpOptionsResponse ::
  AssociateDhcpOptionsResponse
newAssociateDhcpOptionsResponse =
  AssociateDhcpOptionsResponse'

instance Prelude.NFData AssociateDhcpOptionsResponse where
  rnf _ = ()
