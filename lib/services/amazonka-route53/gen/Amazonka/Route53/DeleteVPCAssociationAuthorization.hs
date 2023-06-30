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
-- Module      : Amazonka.Route53.DeleteVPCAssociationAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Sending this request only prevents the Amazon Web Services account that
-- created the VPC from associating the VPC with the Amazon Route 53 hosted
-- zone in the future. If the VPC is already associated with the hosted
-- zone, @DeleteVPCAssociationAuthorization@ won\'t disassociate the VPC
-- from the hosted zone. If you want to delete an existing association, use
-- @DisassociateVPCFromHostedZone@.
module Amazonka.Route53.DeleteVPCAssociationAuthorization
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the request to remove
-- authorization to associate a VPC that was created by one Amazon Web
-- Services account with a hosted zone that was created with a different
-- Amazon Web Services account.
--
-- /See:/ 'newDeleteVPCAssociationAuthorization' smart constructor.
data DeleteVPCAssociationAuthorization = DeleteVPCAssociationAuthorization'
  { -- | When removing authorization to associate a VPC that was created by one
    -- Amazon Web Services account with a hosted zone that was created with a
    -- different Amazon Web Services account, the ID of the hosted zone.
    hostedZoneId :: ResourceId,
    -- | When removing authorization to associate a VPC that was created by one
    -- Amazon Web Services account with a hosted zone that was created with a
    -- different Amazon Web Services account, a complex type that includes the
    -- ID and region of the VPC.
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
-- Amazon Web Services account with a hosted zone that was created with a
-- different Amazon Web Services account, the ID of the hosted zone.
--
-- 'vpc', 'deleteVPCAssociationAuthorization_vpc' - When removing authorization to associate a VPC that was created by one
-- Amazon Web Services account with a hosted zone that was created with a
-- different Amazon Web Services account, a complex type that includes the
-- ID and region of the VPC.
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
-- Amazon Web Services account with a hosted zone that was created with a
-- different Amazon Web Services account, the ID of the hosted zone.
deleteVPCAssociationAuthorization_hostedZoneId :: Lens.Lens' DeleteVPCAssociationAuthorization ResourceId
deleteVPCAssociationAuthorization_hostedZoneId = Lens.lens (\DeleteVPCAssociationAuthorization' {hostedZoneId} -> hostedZoneId) (\s@DeleteVPCAssociationAuthorization' {} a -> s {hostedZoneId = a} :: DeleteVPCAssociationAuthorization)

-- | When removing authorization to associate a VPC that was created by one
-- Amazon Web Services account with a hosted zone that was created with a
-- different Amazon Web Services account, a complex type that includes the
-- ID and region of the VPC.
deleteVPCAssociationAuthorization_vpc :: Lens.Lens' DeleteVPCAssociationAuthorization VPC
deleteVPCAssociationAuthorization_vpc = Lens.lens (\DeleteVPCAssociationAuthorization' {vpc} -> vpc) (\s@DeleteVPCAssociationAuthorization' {} a -> s {vpc = a} :: DeleteVPCAssociationAuthorization)

instance
  Core.AWSRequest
    DeleteVPCAssociationAuthorization
  where
  type
    AWSResponse DeleteVPCAssociationAuthorization =
      DeleteVPCAssociationAuthorizationResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVPCAssociationAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVPCAssociationAuthorization
  where
  hashWithSalt
    _salt
    DeleteVPCAssociationAuthorization' {..} =
      _salt
        `Prelude.hashWithSalt` hostedZoneId
        `Prelude.hashWithSalt` vpc

instance
  Prelude.NFData
    DeleteVPCAssociationAuthorization
  where
  rnf DeleteVPCAssociationAuthorization' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf vpc

instance
  Data.ToElement
    DeleteVPCAssociationAuthorization
  where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DeleteVPCAssociationAuthorizationRequest"

instance
  Data.ToHeaders
    DeleteVPCAssociationAuthorization
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVPCAssociationAuthorization
  where
  toPath DeleteVPCAssociationAuthorization' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/deauthorizevpcassociation"
      ]

instance
  Data.ToQuery
    DeleteVPCAssociationAuthorization
  where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML DeleteVPCAssociationAuthorization where
  toXML DeleteVPCAssociationAuthorization' {..} =
    Prelude.mconcat ["VPC" Data.@= vpc]

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
  where
  rnf DeleteVPCAssociationAuthorizationResponse' {..} =
    Prelude.rnf httpStatus
