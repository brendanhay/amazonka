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
-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon VPC with a private hosted zone.
--
-- To perform the association, the VPC and the private hosted zone must
-- already exist. You can\'t convert a public hosted zone into a private
-- hosted zone.
--
-- If you want to associate a VPC that was created by using one AWS account
-- with a private hosted zone that was created by using a different
-- account, the AWS account that created the private hosted zone must first
-- submit a @CreateVPCAssociationAuthorization@ request. Then the account
-- that created the VPC must submit an @AssociateVPCWithHostedZone@
-- request.
module Network.AWS.Route53.AssociateVPCWithHostedZone
  ( -- * Creating a Request
    AssociateVPCWithHostedZone (..),
    newAssociateVPCWithHostedZone,

    -- * Request Lenses
    associateVPCWithHostedZone_comment,
    associateVPCWithHostedZone_hostedZoneId,
    associateVPCWithHostedZone_vpc,

    -- * Destructuring the Response
    AssociateVPCWithHostedZoneResponse (..),
    newAssociateVPCWithHostedZoneResponse,

    -- * Response Lenses
    associateVPCWithHostedZoneResponse_httpStatus,
    associateVPCWithHostedZoneResponse_changeInfo,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to associate
-- a VPC with a private hosted zone.
--
-- /See:/ 'newAssociateVPCWithHostedZone' smart constructor.
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
  { -- | /Optional:/ A comment about the association request.
    comment :: Core.Maybe Core.Text,
    -- | The ID of the private hosted zone that you want to associate an Amazon
    -- VPC with.
    --
    -- Note that you can\'t associate a VPC with a hosted zone that doesn\'t
    -- have an existing VPC association.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains information about the VPC that you want to
    -- associate with a private hosted zone.
    vpc :: VPC
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateVPCWithHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'associateVPCWithHostedZone_comment' - /Optional:/ A comment about the association request.
--
-- 'hostedZoneId', 'associateVPCWithHostedZone_hostedZoneId' - The ID of the private hosted zone that you want to associate an Amazon
-- VPC with.
--
-- Note that you can\'t associate a VPC with a hosted zone that doesn\'t
-- have an existing VPC association.
--
-- 'vpc', 'associateVPCWithHostedZone_vpc' - A complex type that contains information about the VPC that you want to
-- associate with a private hosted zone.
newAssociateVPCWithHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  AssociateVPCWithHostedZone
newAssociateVPCWithHostedZone pHostedZoneId_ pVPC_ =
  AssociateVPCWithHostedZone'
    { comment = Core.Nothing,
      hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | /Optional:/ A comment about the association request.
associateVPCWithHostedZone_comment :: Lens.Lens' AssociateVPCWithHostedZone (Core.Maybe Core.Text)
associateVPCWithHostedZone_comment = Lens.lens (\AssociateVPCWithHostedZone' {comment} -> comment) (\s@AssociateVPCWithHostedZone' {} a -> s {comment = a} :: AssociateVPCWithHostedZone)

-- | The ID of the private hosted zone that you want to associate an Amazon
-- VPC with.
--
-- Note that you can\'t associate a VPC with a hosted zone that doesn\'t
-- have an existing VPC association.
associateVPCWithHostedZone_hostedZoneId :: Lens.Lens' AssociateVPCWithHostedZone ResourceId
associateVPCWithHostedZone_hostedZoneId = Lens.lens (\AssociateVPCWithHostedZone' {hostedZoneId} -> hostedZoneId) (\s@AssociateVPCWithHostedZone' {} a -> s {hostedZoneId = a} :: AssociateVPCWithHostedZone)

-- | A complex type that contains information about the VPC that you want to
-- associate with a private hosted zone.
associateVPCWithHostedZone_vpc :: Lens.Lens' AssociateVPCWithHostedZone VPC
associateVPCWithHostedZone_vpc = Lens.lens (\AssociateVPCWithHostedZone' {vpc} -> vpc) (\s@AssociateVPCWithHostedZone' {} a -> s {vpc = a} :: AssociateVPCWithHostedZone)

instance Core.AWSRequest AssociateVPCWithHostedZone where
  type
    AWSResponse AssociateVPCWithHostedZone =
      AssociateVPCWithHostedZoneResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVPCWithHostedZoneResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "ChangeInfo")
      )

instance Core.Hashable AssociateVPCWithHostedZone

instance Core.NFData AssociateVPCWithHostedZone

instance Core.ToElement AssociateVPCWithHostedZone where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance Core.ToHeaders AssociateVPCWithHostedZone where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AssociateVPCWithHostedZone where
  toPath AssociateVPCWithHostedZone' {..} =
    Core.mconcat
      [ "/2013-04-01/hostedzone/",
        Core.toBS hostedZoneId,
        "/associatevpc"
      ]

instance Core.ToQuery AssociateVPCWithHostedZone where
  toQuery = Core.const Core.mempty

instance Core.ToXML AssociateVPCWithHostedZone where
  toXML AssociateVPCWithHostedZone' {..} =
    Core.mconcat
      ["Comment" Core.@= comment, "VPC" Core.@= vpc]

-- | A complex type that contains the response information for the
-- @AssociateVPCWithHostedZone@ request.
--
-- /See:/ 'newAssociateVPCWithHostedZoneResponse' smart constructor.
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that describes the changes made to your hosted zone.
    changeInfo :: ChangeInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateVPCWithHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateVPCWithHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'associateVPCWithHostedZoneResponse_changeInfo' - A complex type that describes the changes made to your hosted zone.
newAssociateVPCWithHostedZoneResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  AssociateVPCWithHostedZoneResponse
newAssociateVPCWithHostedZoneResponse
  pHttpStatus_
  pChangeInfo_ =
    AssociateVPCWithHostedZoneResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
associateVPCWithHostedZoneResponse_httpStatus :: Lens.Lens' AssociateVPCWithHostedZoneResponse Core.Int
associateVPCWithHostedZoneResponse_httpStatus = Lens.lens (\AssociateVPCWithHostedZoneResponse' {httpStatus} -> httpStatus) (\s@AssociateVPCWithHostedZoneResponse' {} a -> s {httpStatus = a} :: AssociateVPCWithHostedZoneResponse)

-- | A complex type that describes the changes made to your hosted zone.
associateVPCWithHostedZoneResponse_changeInfo :: Lens.Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
associateVPCWithHostedZoneResponse_changeInfo = Lens.lens (\AssociateVPCWithHostedZoneResponse' {changeInfo} -> changeInfo) (\s@AssociateVPCWithHostedZoneResponse' {} a -> s {changeInfo = a} :: AssociateVPCWithHostedZoneResponse)

instance
  Core.NFData
    AssociateVPCWithHostedZoneResponse
