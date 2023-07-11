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
-- Module      : Amazonka.EC2.EnableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Enables a VPC to support DNS hostname resolution for ClassicLink. If
-- enabled, the DNS hostname of a linked EC2-Classic instance resolves to
-- its private IP address when addressed from an instance in the VPC to
-- which it\'s linked. Similarly, the DNS hostname of an instance in a VPC
-- resolves to its private IP address when addressed from a linked
-- EC2-Classic instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You must specify a VPC ID in the request.
module Amazonka.EC2.EnableVpcClassicLinkDnsSupport
  ( -- * Creating a Request
    EnableVpcClassicLinkDnsSupport (..),
    newEnableVpcClassicLinkDnsSupport,

    -- * Request Lenses
    enableVpcClassicLinkDnsSupport_vpcId,

    -- * Destructuring the Response
    EnableVpcClassicLinkDnsSupportResponse (..),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- * Response Lenses
    enableVpcClassicLinkDnsSupportResponse_return,
    enableVpcClassicLinkDnsSupportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableVpcClassicLinkDnsSupport' smart constructor.
data EnableVpcClassicLinkDnsSupport = EnableVpcClassicLinkDnsSupport'
  { -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'enableVpcClassicLinkDnsSupport_vpcId' - The ID of the VPC.
newEnableVpcClassicLinkDnsSupport ::
  EnableVpcClassicLinkDnsSupport
newEnableVpcClassicLinkDnsSupport =
  EnableVpcClassicLinkDnsSupport'
    { vpcId =
        Prelude.Nothing
    }

-- | The ID of the VPC.
enableVpcClassicLinkDnsSupport_vpcId :: Lens.Lens' EnableVpcClassicLinkDnsSupport (Prelude.Maybe Prelude.Text)
enableVpcClassicLinkDnsSupport_vpcId = Lens.lens (\EnableVpcClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@EnableVpcClassicLinkDnsSupport' {} a -> s {vpcId = a} :: EnableVpcClassicLinkDnsSupport)

instance
  Core.AWSRequest
    EnableVpcClassicLinkDnsSupport
  where
  type
    AWSResponse EnableVpcClassicLinkDnsSupport =
      EnableVpcClassicLinkDnsSupportResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableVpcClassicLinkDnsSupportResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableVpcClassicLinkDnsSupport
  where
  hashWithSalt
    _salt
    EnableVpcClassicLinkDnsSupport' {..} =
      _salt `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    EnableVpcClassicLinkDnsSupport
  where
  rnf EnableVpcClassicLinkDnsSupport' {..} =
    Prelude.rnf vpcId

instance
  Data.ToHeaders
    EnableVpcClassicLinkDnsSupport
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableVpcClassicLinkDnsSupport where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableVpcClassicLinkDnsSupport where
  toQuery EnableVpcClassicLinkDnsSupport' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "EnableVpcClassicLinkDnsSupport" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newEnableVpcClassicLinkDnsSupportResponse' smart constructor.
data EnableVpcClassicLinkDnsSupportResponse = EnableVpcClassicLinkDnsSupportResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkDnsSupportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'enableVpcClassicLinkDnsSupportResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'enableVpcClassicLinkDnsSupportResponse_httpStatus' - The response's http status code.
newEnableVpcClassicLinkDnsSupportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableVpcClassicLinkDnsSupportResponse
newEnableVpcClassicLinkDnsSupportResponse
  pHttpStatus_ =
    EnableVpcClassicLinkDnsSupportResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
enableVpcClassicLinkDnsSupportResponse_return :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse (Prelude.Maybe Prelude.Bool)
enableVpcClassicLinkDnsSupportResponse_return = Lens.lens (\EnableVpcClassicLinkDnsSupportResponse' {return'} -> return') (\s@EnableVpcClassicLinkDnsSupportResponse' {} a -> s {return' = a} :: EnableVpcClassicLinkDnsSupportResponse)

-- | The response's http status code.
enableVpcClassicLinkDnsSupportResponse_httpStatus :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse Prelude.Int
enableVpcClassicLinkDnsSupportResponse_httpStatus = Lens.lens (\EnableVpcClassicLinkDnsSupportResponse' {httpStatus} -> httpStatus) (\s@EnableVpcClassicLinkDnsSupportResponse' {} a -> s {httpStatus = a} :: EnableVpcClassicLinkDnsSupportResponse)

instance
  Prelude.NFData
    EnableVpcClassicLinkDnsSupportResponse
  where
  rnf EnableVpcClassicLinkDnsSupportResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
