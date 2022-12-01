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
-- Module      : Amazonka.Route53.DisassociateVPCFromHostedZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon Virtual Private Cloud (Amazon VPC) from an
-- Amazon Route 53 private hosted zone. Note the following:
--
-- -   You can\'t disassociate the last Amazon VPC from a private hosted
--     zone.
--
-- -   You can\'t convert a private hosted zone into a public hosted zone.
--
-- -   You can submit a @DisassociateVPCFromHostedZone@ request using
--     either the account that created the hosted zone or the account that
--     created the Amazon VPC.
--
-- -   Some services, such as Cloud Map and Amazon Elastic File System
--     (Amazon EFS) automatically create hosted zones and associate VPCs
--     with the hosted zones. A service can create a hosted zone using your
--     account or using its own account. You can disassociate a VPC from a
--     hosted zone only if the service created the hosted zone using your
--     account.
--
--     When you run
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZonesByVPC.html DisassociateVPCFromHostedZone>,
--     if the hosted zone has a value for @OwningAccount@, you can use
--     @DisassociateVPCFromHostedZone@. If the hosted zone has a value for
--     @OwningService@, you can\'t use @DisassociateVPCFromHostedZone@.
--
-- When revoking access, the hosted zone and the Amazon VPC must belong to
-- the same partition. A partition is a group of Amazon Web Services
-- Regions. Each Amazon Web Services account is scoped to one partition.
--
-- The following are the supported partitions:
--
-- -   @aws@ - Amazon Web Services Regions
--
-- -   @aws-cn@ - China Regions
--
-- -   @aws-us-gov@ - Amazon Web Services GovCloud (US) Region
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Access Management>
-- in the /Amazon Web Services General Reference/.
module Amazonka.Route53.DisassociateVPCFromHostedZone
  ( -- * Creating a Request
    DisassociateVPCFromHostedZone (..),
    newDisassociateVPCFromHostedZone,

    -- * Request Lenses
    disassociateVPCFromHostedZone_comment,
    disassociateVPCFromHostedZone_hostedZoneId,
    disassociateVPCFromHostedZone_vpc,

    -- * Destructuring the Response
    DisassociateVPCFromHostedZoneResponse (..),
    newDisassociateVPCFromHostedZoneResponse,

    -- * Response Lenses
    disassociateVPCFromHostedZoneResponse_httpStatus,
    disassociateVPCFromHostedZoneResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the VPC that you want to
-- disassociate from a specified private hosted zone.
--
-- /See:/ 'newDisassociateVPCFromHostedZone' smart constructor.
data DisassociateVPCFromHostedZone = DisassociateVPCFromHostedZone'
  { -- | /Optional:/ A comment about the disassociation request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The ID of the private hosted zone that you want to disassociate a VPC
    -- from.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains information about the VPC that you\'re
    -- disassociating from the specified hosted zone.
    vpc :: VPC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateVPCFromHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'disassociateVPCFromHostedZone_comment' - /Optional:/ A comment about the disassociation request.
--
-- 'hostedZoneId', 'disassociateVPCFromHostedZone_hostedZoneId' - The ID of the private hosted zone that you want to disassociate a VPC
-- from.
--
-- 'vpc', 'disassociateVPCFromHostedZone_vpc' - A complex type that contains information about the VPC that you\'re
-- disassociating from the specified hosted zone.
newDisassociateVPCFromHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  DisassociateVPCFromHostedZone
newDisassociateVPCFromHostedZone pHostedZoneId_ pVPC_ =
  DisassociateVPCFromHostedZone'
    { comment =
        Prelude.Nothing,
      hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | /Optional:/ A comment about the disassociation request.
disassociateVPCFromHostedZone_comment :: Lens.Lens' DisassociateVPCFromHostedZone (Prelude.Maybe Prelude.Text)
disassociateVPCFromHostedZone_comment = Lens.lens (\DisassociateVPCFromHostedZone' {comment} -> comment) (\s@DisassociateVPCFromHostedZone' {} a -> s {comment = a} :: DisassociateVPCFromHostedZone)

-- | The ID of the private hosted zone that you want to disassociate a VPC
-- from.
disassociateVPCFromHostedZone_hostedZoneId :: Lens.Lens' DisassociateVPCFromHostedZone ResourceId
disassociateVPCFromHostedZone_hostedZoneId = Lens.lens (\DisassociateVPCFromHostedZone' {hostedZoneId} -> hostedZoneId) (\s@DisassociateVPCFromHostedZone' {} a -> s {hostedZoneId = a} :: DisassociateVPCFromHostedZone)

-- | A complex type that contains information about the VPC that you\'re
-- disassociating from the specified hosted zone.
disassociateVPCFromHostedZone_vpc :: Lens.Lens' DisassociateVPCFromHostedZone VPC
disassociateVPCFromHostedZone_vpc = Lens.lens (\DisassociateVPCFromHostedZone' {vpc} -> vpc) (\s@DisassociateVPCFromHostedZone' {} a -> s {vpc = a} :: DisassociateVPCFromHostedZone)

instance
  Core.AWSRequest
    DisassociateVPCFromHostedZone
  where
  type
    AWSResponse DisassociateVPCFromHostedZone =
      DisassociateVPCFromHostedZoneResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateVPCFromHostedZoneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ChangeInfo")
      )

instance
  Prelude.Hashable
    DisassociateVPCFromHostedZone
  where
  hashWithSalt _salt DisassociateVPCFromHostedZone' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData DisassociateVPCFromHostedZone where
  rnf DisassociateVPCFromHostedZone' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf vpc

instance Core.ToElement DisassociateVPCFromHostedZone where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DisassociateVPCFromHostedZoneRequest"

instance Core.ToHeaders DisassociateVPCFromHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisassociateVPCFromHostedZone where
  toPath DisassociateVPCFromHostedZone' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Core.toBS hostedZoneId,
        "/disassociatevpc"
      ]

instance Core.ToQuery DisassociateVPCFromHostedZone where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML DisassociateVPCFromHostedZone where
  toXML DisassociateVPCFromHostedZone' {..} =
    Prelude.mconcat
      ["Comment" Core.@= comment, "VPC" Core.@= vpc]

-- | A complex type that contains the response information for the
-- disassociate request.
--
-- /See:/ 'newDisassociateVPCFromHostedZoneResponse' smart constructor.
data DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that describes the changes made to the specified private
    -- hosted zone.
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateVPCFromHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateVPCFromHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'disassociateVPCFromHostedZoneResponse_changeInfo' - A complex type that describes the changes made to the specified private
-- hosted zone.
newDisassociateVPCFromHostedZoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  DisassociateVPCFromHostedZoneResponse
newDisassociateVPCFromHostedZoneResponse
  pHttpStatus_
  pChangeInfo_ =
    DisassociateVPCFromHostedZoneResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
disassociateVPCFromHostedZoneResponse_httpStatus :: Lens.Lens' DisassociateVPCFromHostedZoneResponse Prelude.Int
disassociateVPCFromHostedZoneResponse_httpStatus = Lens.lens (\DisassociateVPCFromHostedZoneResponse' {httpStatus} -> httpStatus) (\s@DisassociateVPCFromHostedZoneResponse' {} a -> s {httpStatus = a} :: DisassociateVPCFromHostedZoneResponse)

-- | A complex type that describes the changes made to the specified private
-- hosted zone.
disassociateVPCFromHostedZoneResponse_changeInfo :: Lens.Lens' DisassociateVPCFromHostedZoneResponse ChangeInfo
disassociateVPCFromHostedZoneResponse_changeInfo = Lens.lens (\DisassociateVPCFromHostedZoneResponse' {changeInfo} -> changeInfo) (\s@DisassociateVPCFromHostedZoneResponse' {} a -> s {changeInfo = a} :: DisassociateVPCFromHostedZoneResponse)

instance
  Prelude.NFData
    DisassociateVPCFromHostedZoneResponse
  where
  rnf DisassociateVPCFromHostedZoneResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
