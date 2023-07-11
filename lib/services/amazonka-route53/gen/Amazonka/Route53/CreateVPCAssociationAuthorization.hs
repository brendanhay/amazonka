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
-- Module      : Amazonka.Route53.CreateVPCAssociationAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the Amazon Web Services account that created a specified VPC
-- to submit an @AssociateVPCWithHostedZone@ request to associate the VPC
-- with a specified hosted zone that was created by a different account. To
-- submit a @CreateVPCAssociationAuthorization@ request, you must use the
-- account that created the hosted zone. After you authorize the
-- association, use the account that created the VPC to submit an
-- @AssociateVPCWithHostedZone@ request.
--
-- If you want to associate multiple VPCs that you created by using one
-- account with a hosted zone that you created by using a different
-- account, you must submit one authorization request for each VPC.
module Amazonka.Route53.CreateVPCAssociationAuthorization
  ( -- * Creating a Request
    CreateVPCAssociationAuthorization (..),
    newCreateVPCAssociationAuthorization,

    -- * Request Lenses
    createVPCAssociationAuthorization_hostedZoneId,
    createVPCAssociationAuthorization_vpc,

    -- * Destructuring the Response
    CreateVPCAssociationAuthorizationResponse (..),
    newCreateVPCAssociationAuthorizationResponse,

    -- * Response Lenses
    createVPCAssociationAuthorizationResponse_httpStatus,
    createVPCAssociationAuthorizationResponse_hostedZoneId,
    createVPCAssociationAuthorizationResponse_vpc,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the request to authorize
-- associating a VPC with your private hosted zone. Authorization is only
-- required when a private hosted zone and a VPC were created by using
-- different accounts.
--
-- /See:/ 'newCreateVPCAssociationAuthorization' smart constructor.
data CreateVPCAssociationAuthorization = CreateVPCAssociationAuthorization'
  { -- | The ID of the private hosted zone that you want to authorize associating
    -- a VPC with.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains the VPC ID and region for the VPC that you
    -- want to authorize associating with your hosted zone.
    vpc :: VPC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCAssociationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'createVPCAssociationAuthorization_hostedZoneId' - The ID of the private hosted zone that you want to authorize associating
-- a VPC with.
--
-- 'vpc', 'createVPCAssociationAuthorization_vpc' - A complex type that contains the VPC ID and region for the VPC that you
-- want to authorize associating with your hosted zone.
newCreateVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  CreateVPCAssociationAuthorization
newCreateVPCAssociationAuthorization
  pHostedZoneId_
  pVPC_ =
    CreateVPCAssociationAuthorization'
      { hostedZoneId =
          pHostedZoneId_,
        vpc = pVPC_
      }

-- | The ID of the private hosted zone that you want to authorize associating
-- a VPC with.
createVPCAssociationAuthorization_hostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorization ResourceId
createVPCAssociationAuthorization_hostedZoneId = Lens.lens (\CreateVPCAssociationAuthorization' {hostedZoneId} -> hostedZoneId) (\s@CreateVPCAssociationAuthorization' {} a -> s {hostedZoneId = a} :: CreateVPCAssociationAuthorization)

-- | A complex type that contains the VPC ID and region for the VPC that you
-- want to authorize associating with your hosted zone.
createVPCAssociationAuthorization_vpc :: Lens.Lens' CreateVPCAssociationAuthorization VPC
createVPCAssociationAuthorization_vpc = Lens.lens (\CreateVPCAssociationAuthorization' {vpc} -> vpc) (\s@CreateVPCAssociationAuthorization' {} a -> s {vpc = a} :: CreateVPCAssociationAuthorization)

instance
  Core.AWSRequest
    CreateVPCAssociationAuthorization
  where
  type
    AWSResponse CreateVPCAssociationAuthorization =
      CreateVPCAssociationAuthorizationResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVPCAssociationAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HostedZoneId")
            Prelude.<*> (x Data..@ "VPC")
      )

instance
  Prelude.Hashable
    CreateVPCAssociationAuthorization
  where
  hashWithSalt
    _salt
    CreateVPCAssociationAuthorization' {..} =
      _salt
        `Prelude.hashWithSalt` hostedZoneId
        `Prelude.hashWithSalt` vpc

instance
  Prelude.NFData
    CreateVPCAssociationAuthorization
  where
  rnf CreateVPCAssociationAuthorization' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf vpc

instance
  Data.ToElement
    CreateVPCAssociationAuthorization
  where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateVPCAssociationAuthorizationRequest"

instance
  Data.ToHeaders
    CreateVPCAssociationAuthorization
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateVPCAssociationAuthorization
  where
  toPath CreateVPCAssociationAuthorization' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/authorizevpcassociation"
      ]

instance
  Data.ToQuery
    CreateVPCAssociationAuthorization
  where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateVPCAssociationAuthorization where
  toXML CreateVPCAssociationAuthorization' {..} =
    Prelude.mconcat ["VPC" Data.@= vpc]

-- | A complex type that contains the response information from a
-- @CreateVPCAssociationAuthorization@ request.
--
-- /See:/ 'newCreateVPCAssociationAuthorizationResponse' smart constructor.
data CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the hosted zone that you authorized associating a VPC with.
    hostedZoneId :: ResourceId,
    -- | The VPC that you authorized associating with a hosted zone.
    vpc :: VPC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCAssociationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVPCAssociationAuthorizationResponse_httpStatus' - The response's http status code.
--
-- 'hostedZoneId', 'createVPCAssociationAuthorizationResponse_hostedZoneId' - The ID of the hosted zone that you authorized associating a VPC with.
--
-- 'vpc', 'createVPCAssociationAuthorizationResponse_vpc' - The VPC that you authorized associating with a hosted zone.
newCreateVPCAssociationAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  CreateVPCAssociationAuthorizationResponse
newCreateVPCAssociationAuthorizationResponse
  pHttpStatus_
  pHostedZoneId_
  pVPC_ =
    CreateVPCAssociationAuthorizationResponse'
      { httpStatus =
          pHttpStatus_,
        hostedZoneId = pHostedZoneId_,
        vpc = pVPC_
      }

-- | The response's http status code.
createVPCAssociationAuthorizationResponse_httpStatus :: Lens.Lens' CreateVPCAssociationAuthorizationResponse Prelude.Int
createVPCAssociationAuthorizationResponse_httpStatus = Lens.lens (\CreateVPCAssociationAuthorizationResponse' {httpStatus} -> httpStatus) (\s@CreateVPCAssociationAuthorizationResponse' {} a -> s {httpStatus = a} :: CreateVPCAssociationAuthorizationResponse)

-- | The ID of the hosted zone that you authorized associating a VPC with.
createVPCAssociationAuthorizationResponse_hostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorizationResponse ResourceId
createVPCAssociationAuthorizationResponse_hostedZoneId = Lens.lens (\CreateVPCAssociationAuthorizationResponse' {hostedZoneId} -> hostedZoneId) (\s@CreateVPCAssociationAuthorizationResponse' {} a -> s {hostedZoneId = a} :: CreateVPCAssociationAuthorizationResponse)

-- | The VPC that you authorized associating with a hosted zone.
createVPCAssociationAuthorizationResponse_vpc :: Lens.Lens' CreateVPCAssociationAuthorizationResponse VPC
createVPCAssociationAuthorizationResponse_vpc = Lens.lens (\CreateVPCAssociationAuthorizationResponse' {vpc} -> vpc) (\s@CreateVPCAssociationAuthorizationResponse' {} a -> s {vpc = a} :: CreateVPCAssociationAuthorizationResponse)

instance
  Prelude.NFData
    CreateVPCAssociationAuthorizationResponse
  where
  rnf CreateVPCAssociationAuthorizationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf vpc
