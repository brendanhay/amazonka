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
-- Module      : Amazonka.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- If you want to associate a VPC that was created by using one Amazon Web
-- Services account with a private hosted zone that was created by using a
-- different account, the Amazon Web Services account that created the
-- private hosted zone must first submit a
-- @CreateVPCAssociationAuthorization@ request. Then the account that
-- created the VPC must submit an @AssociateVPCWithHostedZone@ request.
--
-- When granting access, the hosted zone and the Amazon VPC must belong to
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
module Amazonka.Route53.AssociateVPCWithHostedZone
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest AssociateVPCWithHostedZone where
  type
    AWSResponse AssociateVPCWithHostedZone =
      AssociateVPCWithHostedZoneResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVPCWithHostedZoneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable AssociateVPCWithHostedZone where
  hashWithSalt _salt AssociateVPCWithHostedZone' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData AssociateVPCWithHostedZone where
  rnf AssociateVPCWithHostedZone' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf vpc

instance Data.ToElement AssociateVPCWithHostedZone where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance Data.ToHeaders AssociateVPCWithHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateVPCWithHostedZone where
  toPath AssociateVPCWithHostedZone' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/associatevpc"
      ]

instance Data.ToQuery AssociateVPCWithHostedZone where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML AssociateVPCWithHostedZone where
  toXML AssociateVPCWithHostedZone' {..} =
    Prelude.mconcat
      ["Comment" Data.@= comment, "VPC" Data.@= vpc]

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AssociateVPCWithHostedZoneResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
