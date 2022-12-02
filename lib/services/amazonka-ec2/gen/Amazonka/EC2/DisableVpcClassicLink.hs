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
-- Module      : Amazonka.EC2.DisableVpcClassicLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC
-- that has EC2-Classic instances linked to it.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DisableVpcClassicLink
  ( -- * Creating a Request
    DisableVpcClassicLink (..),
    newDisableVpcClassicLink,

    -- * Request Lenses
    disableVpcClassicLink_dryRun,
    disableVpcClassicLink_vpcId,

    -- * Destructuring the Response
    DisableVpcClassicLinkResponse (..),
    newDisableVpcClassicLinkResponse,

    -- * Response Lenses
    disableVpcClassicLinkResponse_return,
    disableVpcClassicLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableVpcClassicLink' smart constructor.
data DisableVpcClassicLink = DisableVpcClassicLink'
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
-- Create a value of 'DisableVpcClassicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableVpcClassicLink_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'disableVpcClassicLink_vpcId' - The ID of the VPC.
newDisableVpcClassicLink ::
  -- | 'vpcId'
  Prelude.Text ->
  DisableVpcClassicLink
newDisableVpcClassicLink pVpcId_ =
  DisableVpcClassicLink'
    { dryRun = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableVpcClassicLink_dryRun :: Lens.Lens' DisableVpcClassicLink (Prelude.Maybe Prelude.Bool)
disableVpcClassicLink_dryRun = Lens.lens (\DisableVpcClassicLink' {dryRun} -> dryRun) (\s@DisableVpcClassicLink' {} a -> s {dryRun = a} :: DisableVpcClassicLink)

-- | The ID of the VPC.
disableVpcClassicLink_vpcId :: Lens.Lens' DisableVpcClassicLink Prelude.Text
disableVpcClassicLink_vpcId = Lens.lens (\DisableVpcClassicLink' {vpcId} -> vpcId) (\s@DisableVpcClassicLink' {} a -> s {vpcId = a} :: DisableVpcClassicLink)

instance Core.AWSRequest DisableVpcClassicLink where
  type
    AWSResponse DisableVpcClassicLink =
      DisableVpcClassicLinkResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableVpcClassicLinkResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableVpcClassicLink where
  hashWithSalt _salt DisableVpcClassicLink' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DisableVpcClassicLink where
  rnf DisableVpcClassicLink' {..} =
    Prelude.rnf dryRun `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders DisableVpcClassicLink where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableVpcClassicLink where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableVpcClassicLink where
  toQuery DisableVpcClassicLink' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableVpcClassicLink" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDisableVpcClassicLinkResponse' smart constructor.
data DisableVpcClassicLinkResponse = DisableVpcClassicLinkResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableVpcClassicLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'disableVpcClassicLinkResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disableVpcClassicLinkResponse_httpStatus' - The response's http status code.
newDisableVpcClassicLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableVpcClassicLinkResponse
newDisableVpcClassicLinkResponse pHttpStatus_ =
  DisableVpcClassicLinkResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disableVpcClassicLinkResponse_return :: Lens.Lens' DisableVpcClassicLinkResponse (Prelude.Maybe Prelude.Bool)
disableVpcClassicLinkResponse_return = Lens.lens (\DisableVpcClassicLinkResponse' {return'} -> return') (\s@DisableVpcClassicLinkResponse' {} a -> s {return' = a} :: DisableVpcClassicLinkResponse)

-- | The response's http status code.
disableVpcClassicLinkResponse_httpStatus :: Lens.Lens' DisableVpcClassicLinkResponse Prelude.Int
disableVpcClassicLinkResponse_httpStatus = Lens.lens (\DisableVpcClassicLinkResponse' {httpStatus} -> httpStatus) (\s@DisableVpcClassicLinkResponse' {} a -> s {httpStatus = a} :: DisableVpcClassicLinkResponse)

instance Prelude.NFData DisableVpcClassicLinkResponse where
  rnf DisableVpcClassicLinkResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
