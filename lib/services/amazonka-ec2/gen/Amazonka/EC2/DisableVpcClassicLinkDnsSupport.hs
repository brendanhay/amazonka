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
-- Module      : Amazonka.EC2.DisableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames
-- resolve to public IP addresses when addressed between a linked
-- EC2-Classic instance and instances in the VPC to which it\'s linked. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You must specify a VPC ID in the request.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DisableVpcClassicLinkDnsSupport
  ( -- * Creating a Request
    DisableVpcClassicLinkDnsSupport (..),
    newDisableVpcClassicLinkDnsSupport,

    -- * Request Lenses
    disableVpcClassicLinkDnsSupport_vpcId,

    -- * Destructuring the Response
    DisableVpcClassicLinkDnsSupportResponse (..),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- * Response Lenses
    disableVpcClassicLinkDnsSupportResponse_return,
    disableVpcClassicLinkDnsSupportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableVpcClassicLinkDnsSupport' smart constructor.
data DisableVpcClassicLinkDnsSupport = DisableVpcClassicLinkDnsSupport'
  { -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableVpcClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'disableVpcClassicLinkDnsSupport_vpcId' - The ID of the VPC.
newDisableVpcClassicLinkDnsSupport ::
  DisableVpcClassicLinkDnsSupport
newDisableVpcClassicLinkDnsSupport =
  DisableVpcClassicLinkDnsSupport'
    { vpcId =
        Prelude.Nothing
    }

-- | The ID of the VPC.
disableVpcClassicLinkDnsSupport_vpcId :: Lens.Lens' DisableVpcClassicLinkDnsSupport (Prelude.Maybe Prelude.Text)
disableVpcClassicLinkDnsSupport_vpcId = Lens.lens (\DisableVpcClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@DisableVpcClassicLinkDnsSupport' {} a -> s {vpcId = a} :: DisableVpcClassicLinkDnsSupport)

instance
  Core.AWSRequest
    DisableVpcClassicLinkDnsSupport
  where
  type
    AWSResponse DisableVpcClassicLinkDnsSupport =
      DisableVpcClassicLinkDnsSupportResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableVpcClassicLinkDnsSupportResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableVpcClassicLinkDnsSupport
  where
  hashWithSalt
    _salt
    DisableVpcClassicLinkDnsSupport' {..} =
      _salt `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    DisableVpcClassicLinkDnsSupport
  where
  rnf DisableVpcClassicLinkDnsSupport' {..} =
    Prelude.rnf vpcId

instance
  Data.ToHeaders
    DisableVpcClassicLinkDnsSupport
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableVpcClassicLinkDnsSupport where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableVpcClassicLinkDnsSupport where
  toQuery DisableVpcClassicLinkDnsSupport' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisableVpcClassicLinkDnsSupport" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDisableVpcClassicLinkDnsSupportResponse' smart constructor.
data DisableVpcClassicLinkDnsSupportResponse = DisableVpcClassicLinkDnsSupportResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableVpcClassicLinkDnsSupportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'disableVpcClassicLinkDnsSupportResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disableVpcClassicLinkDnsSupportResponse_httpStatus' - The response's http status code.
newDisableVpcClassicLinkDnsSupportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableVpcClassicLinkDnsSupportResponse
newDisableVpcClassicLinkDnsSupportResponse
  pHttpStatus_ =
    DisableVpcClassicLinkDnsSupportResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disableVpcClassicLinkDnsSupportResponse_return :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse (Prelude.Maybe Prelude.Bool)
disableVpcClassicLinkDnsSupportResponse_return = Lens.lens (\DisableVpcClassicLinkDnsSupportResponse' {return'} -> return') (\s@DisableVpcClassicLinkDnsSupportResponse' {} a -> s {return' = a} :: DisableVpcClassicLinkDnsSupportResponse)

-- | The response's http status code.
disableVpcClassicLinkDnsSupportResponse_httpStatus :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse Prelude.Int
disableVpcClassicLinkDnsSupportResponse_httpStatus = Lens.lens (\DisableVpcClassicLinkDnsSupportResponse' {httpStatus} -> httpStatus) (\s@DisableVpcClassicLinkDnsSupportResponse' {} a -> s {httpStatus = a} :: DisableVpcClassicLinkDnsSupportResponse)

instance
  Prelude.NFData
    DisableVpcClassicLinkDnsSupportResponse
  where
  rnf DisableVpcClassicLinkDnsSupportResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
