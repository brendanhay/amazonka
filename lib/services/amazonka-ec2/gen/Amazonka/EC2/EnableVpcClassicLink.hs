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
-- Module      : Amazonka.EC2.EnableVpcClassicLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Enables a VPC for ClassicLink. You can then link EC2-Classic instances
-- to your ClassicLink-enabled VPC to allow communication over private IP
-- addresses. You cannot enable your VPC for ClassicLink if any of your VPC
-- route tables have existing routes for address ranges within the
-- @10.0.0.0\/8@ IP address range, excluding local routes for VPCs in the
-- @10.0.0.0\/16@ and @10.1.0.0\/16@ IP address ranges. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.EnableVpcClassicLink
  ( -- * Creating a Request
    EnableVpcClassicLink (..),
    newEnableVpcClassicLink,

    -- * Request Lenses
    enableVpcClassicLink_dryRun,
    enableVpcClassicLink_vpcId,

    -- * Destructuring the Response
    EnableVpcClassicLinkResponse (..),
    newEnableVpcClassicLinkResponse,

    -- * Response Lenses
    enableVpcClassicLinkResponse_return,
    enableVpcClassicLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableVpcClassicLink' smart constructor.
data EnableVpcClassicLink = EnableVpcClassicLink'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVpcClassicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableVpcClassicLink_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'enableVpcClassicLink_vpcId' - The ID of the VPC.
newEnableVpcClassicLink ::
  -- | 'vpcId'
  Prelude.Text ->
  EnableVpcClassicLink
newEnableVpcClassicLink pVpcId_ =
  EnableVpcClassicLink'
    { dryRun = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableVpcClassicLink_dryRun :: Lens.Lens' EnableVpcClassicLink (Prelude.Maybe Prelude.Bool)
enableVpcClassicLink_dryRun = Lens.lens (\EnableVpcClassicLink' {dryRun} -> dryRun) (\s@EnableVpcClassicLink' {} a -> s {dryRun = a} :: EnableVpcClassicLink)

-- | The ID of the VPC.
enableVpcClassicLink_vpcId :: Lens.Lens' EnableVpcClassicLink Prelude.Text
enableVpcClassicLink_vpcId = Lens.lens (\EnableVpcClassicLink' {vpcId} -> vpcId) (\s@EnableVpcClassicLink' {} a -> s {vpcId = a} :: EnableVpcClassicLink)

instance Core.AWSRequest EnableVpcClassicLink where
  type
    AWSResponse EnableVpcClassicLink =
      EnableVpcClassicLinkResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableVpcClassicLinkResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableVpcClassicLink where
  hashWithSalt _salt EnableVpcClassicLink' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData EnableVpcClassicLink where
  rnf EnableVpcClassicLink' {..} =
    Prelude.rnf dryRun `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders EnableVpcClassicLink where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableVpcClassicLink where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableVpcClassicLink where
  toQuery EnableVpcClassicLink' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableVpcClassicLink" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newEnableVpcClassicLinkResponse' smart constructor.
data EnableVpcClassicLinkResponse = EnableVpcClassicLinkResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'enableVpcClassicLinkResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'enableVpcClassicLinkResponse_httpStatus' - The response's http status code.
newEnableVpcClassicLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableVpcClassicLinkResponse
newEnableVpcClassicLinkResponse pHttpStatus_ =
  EnableVpcClassicLinkResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
enableVpcClassicLinkResponse_return :: Lens.Lens' EnableVpcClassicLinkResponse (Prelude.Maybe Prelude.Bool)
enableVpcClassicLinkResponse_return = Lens.lens (\EnableVpcClassicLinkResponse' {return'} -> return') (\s@EnableVpcClassicLinkResponse' {} a -> s {return' = a} :: EnableVpcClassicLinkResponse)

-- | The response's http status code.
enableVpcClassicLinkResponse_httpStatus :: Lens.Lens' EnableVpcClassicLinkResponse Prelude.Int
enableVpcClassicLinkResponse_httpStatus = Lens.lens (\EnableVpcClassicLinkResponse' {httpStatus} -> httpStatus) (\s@EnableVpcClassicLinkResponse' {} a -> s {httpStatus = a} :: EnableVpcClassicLinkResponse)

instance Prelude.NFData EnableVpcClassicLinkResponse where
  rnf EnableVpcClassicLinkResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
