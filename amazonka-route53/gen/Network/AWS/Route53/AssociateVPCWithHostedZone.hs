{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to associate
-- a VPC with a private hosted zone.
--
-- /See:/ 'newAssociateVPCWithHostedZone' smart constructor.
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
  { -- | /Optional:/ A comment about the association request.
    comment :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { comment =
        Prelude.Nothing,
      hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | /Optional:/ A comment about the association request.
associateVPCWithHostedZone_comment :: Lens.Lens' AssociateVPCWithHostedZone (Prelude.Maybe Prelude.Text)
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

instance
  Prelude.AWSRequest
    AssociateVPCWithHostedZone
  where
  type
    Rs AssociateVPCWithHostedZone =
      AssociateVPCWithHostedZoneResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVPCWithHostedZoneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "ChangeInfo")
      )

instance Prelude.Hashable AssociateVPCWithHostedZone

instance Prelude.NFData AssociateVPCWithHostedZone

instance Prelude.ToElement AssociateVPCWithHostedZone where
  toElement =
    Prelude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance Prelude.ToHeaders AssociateVPCWithHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AssociateVPCWithHostedZone where
  toPath AssociateVPCWithHostedZone' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Prelude.toBS hostedZoneId,
        "/associatevpc"
      ]

instance Prelude.ToQuery AssociateVPCWithHostedZone where
  toQuery = Prelude.const Prelude.mempty

instance Prelude.ToXML AssociateVPCWithHostedZone where
  toXML AssociateVPCWithHostedZone' {..} =
    Prelude.mconcat
      ["Comment" Prelude.@= comment, "VPC" Prelude.@= vpc]

-- | A complex type that contains the response information for the
-- @AssociateVPCWithHostedZone@ request.
--
-- /See:/ 'newAssociateVPCWithHostedZoneResponse' smart constructor.
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that describes the changes made to your hosted zone.
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
associateVPCWithHostedZoneResponse_httpStatus :: Lens.Lens' AssociateVPCWithHostedZoneResponse Prelude.Int
associateVPCWithHostedZoneResponse_httpStatus = Lens.lens (\AssociateVPCWithHostedZoneResponse' {httpStatus} -> httpStatus) (\s@AssociateVPCWithHostedZoneResponse' {} a -> s {httpStatus = a} :: AssociateVPCWithHostedZoneResponse)

-- | A complex type that describes the changes made to your hosted zone.
associateVPCWithHostedZoneResponse_changeInfo :: Lens.Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
associateVPCWithHostedZoneResponse_changeInfo = Lens.lens (\AssociateVPCWithHostedZoneResponse' {changeInfo} -> changeInfo) (\s@AssociateVPCWithHostedZoneResponse' {} a -> s {changeInfo = a} :: AssociateVPCWithHostedZoneResponse)

instance
  Prelude.NFData
    AssociateVPCWithHostedZoneResponse
