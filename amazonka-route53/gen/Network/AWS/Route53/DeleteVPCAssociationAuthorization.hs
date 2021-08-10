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
-- Module      : Network.AWS.Route53.DeleteVPCAssociationAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization to submit an @AssociateVPCWithHostedZone@ request
-- to associate a specified VPC with a hosted zone that was created by a
-- different account. You must use the account that created the hosted zone
-- to submit a @DeleteVPCAssociationAuthorization@ request.
--
-- Sending this request only prevents the AWS account that created the VPC
-- from associating the VPC with the Amazon Route 53 hosted zone in the
-- future. If the VPC is already associated with the hosted zone,
-- @DeleteVPCAssociationAuthorization@ won\'t disassociate the VPC from the
-- hosted zone. If you want to delete an existing association, use
-- @DisassociateVPCFromHostedZone@.
module Network.AWS.Route53.DeleteVPCAssociationAuthorization
  ( -- * Creating a Request
    DeleteVPCAssociationAuthorization (..),
    newDeleteVPCAssociationAuthorization,

    -- * Request Lenses
    deleteVPCAssociationAuthorization_hostedZoneId,
    deleteVPCAssociationAuthorization_vpc,

    -- * Destructuring the Response
    DeleteVPCAssociationAuthorizationResponse (..),
    newDeleteVPCAssociationAuthorizationResponse,

    -- * Response Lenses
    deleteVPCAssociationAuthorizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to remove
-- authorization to associate a VPC that was created by one AWS account
-- with a hosted zone that was created with a different AWS account.
--
-- /See:/ 'newDeleteVPCAssociationAuthorization' smart constructor.
data DeleteVPCAssociationAuthorization = DeleteVPCAssociationAuthorization'
  { -- | When removing authorization to associate a VPC that was created by one
    -- AWS account with a hosted zone that was created with a different AWS
    -- account, the ID of the hosted zone.
    hostedZoneId :: ResourceId,
    -- | When removing authorization to associate a VPC that was created by one
    -- AWS account with a hosted zone that was created with a different AWS
    -- account, a complex type that includes the ID and region of the VPC.
    vpc :: VPC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCAssociationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'deleteVPCAssociationAuthorization_hostedZoneId' - When removing authorization to associate a VPC that was created by one
-- AWS account with a hosted zone that was created with a different AWS
-- account, the ID of the hosted zone.
--
-- 'vpc', 'deleteVPCAssociationAuthorization_vpc' - When removing authorization to associate a VPC that was created by one
-- AWS account with a hosted zone that was created with a different AWS
-- account, a complex type that includes the ID and region of the VPC.
newDeleteVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  DeleteVPCAssociationAuthorization
newDeleteVPCAssociationAuthorization
  pHostedZoneId_
  pVPC_ =
    DeleteVPCAssociationAuthorization'
      { hostedZoneId =
          pHostedZoneId_,
        vpc = pVPC_
      }

-- | When removing authorization to associate a VPC that was created by one
-- AWS account with a hosted zone that was created with a different AWS
-- account, the ID of the hosted zone.
deleteVPCAssociationAuthorization_hostedZoneId :: Lens.Lens' DeleteVPCAssociationAuthorization ResourceId
deleteVPCAssociationAuthorization_hostedZoneId = Lens.lens (\DeleteVPCAssociationAuthorization' {hostedZoneId} -> hostedZoneId) (\s@DeleteVPCAssociationAuthorization' {} a -> s {hostedZoneId = a} :: DeleteVPCAssociationAuthorization)

-- | When removing authorization to associate a VPC that was created by one
-- AWS account with a hosted zone that was created with a different AWS
-- account, a complex type that includes the ID and region of the VPC.
deleteVPCAssociationAuthorization_vpc :: Lens.Lens' DeleteVPCAssociationAuthorization VPC
deleteVPCAssociationAuthorization_vpc = Lens.lens (\DeleteVPCAssociationAuthorization' {vpc} -> vpc) (\s@DeleteVPCAssociationAuthorization' {} a -> s {vpc = a} :: DeleteVPCAssociationAuthorization)

instance
  Core.AWSRequest
    DeleteVPCAssociationAuthorization
  where
  type
    AWSResponse DeleteVPCAssociationAuthorization =
      DeleteVPCAssociationAuthorizationResponse
  request = Request.postXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVPCAssociationAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVPCAssociationAuthorization

instance
  Prelude.NFData
    DeleteVPCAssociationAuthorization

instance
  Core.ToElement
    DeleteVPCAssociationAuthorization
  where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DeleteVPCAssociationAuthorizationRequest"

instance
  Core.ToHeaders
    DeleteVPCAssociationAuthorization
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteVPCAssociationAuthorization
  where
  toPath DeleteVPCAssociationAuthorization' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Core.toBS hostedZoneId,
        "/deauthorizevpcassociation"
      ]

instance
  Core.ToQuery
    DeleteVPCAssociationAuthorization
  where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML DeleteVPCAssociationAuthorization where
  toXML DeleteVPCAssociationAuthorization' {..} =
    Prelude.mconcat ["VPC" Core.@= vpc]

-- | Empty response for the request.
--
-- /See:/ 'newDeleteVPCAssociationAuthorizationResponse' smart constructor.
data DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCAssociationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVPCAssociationAuthorizationResponse_httpStatus' - The response's http status code.
newDeleteVPCAssociationAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVPCAssociationAuthorizationResponse
newDeleteVPCAssociationAuthorizationResponse
  pHttpStatus_ =
    DeleteVPCAssociationAuthorizationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteVPCAssociationAuthorizationResponse_httpStatus :: Lens.Lens' DeleteVPCAssociationAuthorizationResponse Prelude.Int
deleteVPCAssociationAuthorizationResponse_httpStatus = Lens.lens (\DeleteVPCAssociationAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteVPCAssociationAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteVPCAssociationAuthorizationResponse)

instance
  Prelude.NFData
    DeleteVPCAssociationAuthorizationResponse
