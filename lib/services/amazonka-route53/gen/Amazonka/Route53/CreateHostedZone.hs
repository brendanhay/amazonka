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
-- Module      : Amazonka.Route53.CreateHostedZone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new public or private hosted zone. You create records in a
-- public hosted zone to define how you want to route traffic on the
-- internet for a domain, such as example.com, and its subdomains
-- (apex.example.com, acme.example.com). You create records in a private
-- hosted zone to define how you want to route traffic for a domain and its
-- subdomains within one or more Amazon Virtual Private Clouds (Amazon
-- VPCs).
--
-- You can\'t convert a public hosted zone to a private hosted zone or vice
-- versa. Instead, you must create a new hosted zone with the same name and
-- create new resource record sets.
--
-- For more information about charges for hosted zones, see
-- <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
--
-- Note the following:
--
-- -   You can\'t create a hosted zone for a top-level domain (TLD) such as
--     .com.
--
-- -   For public hosted zones, Route 53 automatically creates a default
--     SOA record and four NS records for the zone. For more information
--     about SOA and NS records, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html NS and SOA Records that Route 53 Creates for a Hosted Zone>
--     in the /Amazon Route 53 Developer Guide/.
--
--     If you want to use the same name servers for multiple public hosted
--     zones, you can optionally associate a reusable delegation set with
--     the hosted zone. See the @DelegationSetId@ element.
--
-- -   If your domain is registered with a registrar other than Route 53,
--     you must update the name servers with your registrar to make Route
--     53 the DNS service for the domain. For more information, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/MigratingDNS.html Migrating DNS Service for an Existing Domain to Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/.
--
-- When you submit a @CreateHostedZone@ request, the initial status of the
-- hosted zone is @PENDING@. For public hosted zones, this means that the
-- NS and SOA records are not yet available on all Route 53 DNS servers.
-- When the NS and SOA records are available, the status of the zone
-- changes to @INSYNC@.
--
-- The @CreateHostedZone@ request requires the caller to have an
-- @ec2:DescribeVpcs@ permission.
--
-- When creating private hosted zones, the Amazon VPC must belong to the
-- same partition where the hosted zone is created. A partition is a group
-- of Amazon Web Services Regions. Each Amazon Web Services account is
-- scoped to one partition.
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
module Amazonka.Route53.CreateHostedZone
  ( -- * Creating a Request
    CreateHostedZone (..),
    newCreateHostedZone,

    -- * Request Lenses
    createHostedZone_delegationSetId,
    createHostedZone_hostedZoneConfig,
    createHostedZone_vpc,
    createHostedZone_name,
    createHostedZone_callerReference,

    -- * Destructuring the Response
    CreateHostedZoneResponse (..),
    newCreateHostedZoneResponse,

    -- * Response Lenses
    createHostedZoneResponse_vpc,
    createHostedZoneResponse_httpStatus,
    createHostedZoneResponse_hostedZone,
    createHostedZoneResponse_changeInfo,
    createHostedZoneResponse_delegationSet,
    createHostedZoneResponse_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the request to create a
-- public or private hosted zone.
--
-- /See:/ 'newCreateHostedZone' smart constructor.
data CreateHostedZone = CreateHostedZone'
  { -- | If you want to associate a reusable delegation set with this hosted
    -- zone, the ID that Amazon Route 53 assigned to the reusable delegation
    -- set when you created it. For more information about reusable delegation
    -- sets, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet>.
    delegationSetId :: Prelude.Maybe ResourceId,
    -- | (Optional) A complex type that contains the following optional values:
    --
    -- -   For public and private hosted zones, an optional comment
    --
    -- -   For private hosted zones, an optional @PrivateZone@ element
    --
    -- If you don\'t specify a comment or the @PrivateZone@ element, omit
    -- @HostedZoneConfig@ and the other elements.
    hostedZoneConfig :: Prelude.Maybe HostedZoneConfig,
    -- | (Private hosted zones only) A complex type that contains information
    -- about the Amazon VPC that you\'re associating with this hosted zone.
    --
    -- You can specify only one Amazon VPC when you create a private hosted
    -- zone. If you are associating a VPC with a hosted zone with this request,
    -- the paramaters @VPCId@ and @VPCRegion@ are also required.
    --
    -- To associate additional Amazon VPCs with the hosted zone, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone>
    -- after you create a hosted zone.
    vpc :: Prelude.Maybe VPC,
    -- | The name of the domain. Specify a fully qualified domain name, for
    -- example, /www.example.com/. The trailing dot is optional; Amazon
    -- Route 53 assumes that the domain name is fully qualified. This means
    -- that Route 53 treats /www.example.com/ (without a trailing dot) and
    -- /www.example.com./ (with a trailing dot) as identical.
    --
    -- If you\'re creating a public hosted zone, this is the name you have
    -- registered with your DNS registrar. If your domain name is registered
    -- with a registrar other than Route 53, change the name servers for your
    -- domain to the set of @NameServers@ that @CreateHostedZone@ returns in
    -- @DelegationSet@.
    name :: Prelude.Text,
    -- | A unique string that identifies the request and that allows failed
    -- @CreateHostedZone@ requests to be retried without the risk of executing
    -- the operation twice. You must use a unique @CallerReference@ string
    -- every time you submit a @CreateHostedZone@ request. @CallerReference@
    -- can be any unique string, for example, a date\/time stamp.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegationSetId', 'createHostedZone_delegationSetId' - If you want to associate a reusable delegation set with this hosted
-- zone, the ID that Amazon Route 53 assigned to the reusable delegation
-- set when you created it. For more information about reusable delegation
-- sets, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet>.
--
-- 'hostedZoneConfig', 'createHostedZone_hostedZoneConfig' - (Optional) A complex type that contains the following optional values:
--
-- -   For public and private hosted zones, an optional comment
--
-- -   For private hosted zones, an optional @PrivateZone@ element
--
-- If you don\'t specify a comment or the @PrivateZone@ element, omit
-- @HostedZoneConfig@ and the other elements.
--
-- 'vpc', 'createHostedZone_vpc' - (Private hosted zones only) A complex type that contains information
-- about the Amazon VPC that you\'re associating with this hosted zone.
--
-- You can specify only one Amazon VPC when you create a private hosted
-- zone. If you are associating a VPC with a hosted zone with this request,
-- the paramaters @VPCId@ and @VPCRegion@ are also required.
--
-- To associate additional Amazon VPCs with the hosted zone, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone>
-- after you create a hosted zone.
--
-- 'name', 'createHostedZone_name' - The name of the domain. Specify a fully qualified domain name, for
-- example, /www.example.com/. The trailing dot is optional; Amazon
-- Route 53 assumes that the domain name is fully qualified. This means
-- that Route 53 treats /www.example.com/ (without a trailing dot) and
-- /www.example.com./ (with a trailing dot) as identical.
--
-- If you\'re creating a public hosted zone, this is the name you have
-- registered with your DNS registrar. If your domain name is registered
-- with a registrar other than Route 53, change the name servers for your
-- domain to the set of @NameServers@ that @CreateHostedZone@ returns in
-- @DelegationSet@.
--
-- 'callerReference', 'createHostedZone_callerReference' - A unique string that identifies the request and that allows failed
-- @CreateHostedZone@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CallerReference@ string
-- every time you submit a @CreateHostedZone@ request. @CallerReference@
-- can be any unique string, for example, a date\/time stamp.
newCreateHostedZone ::
  -- | 'name'
  Prelude.Text ->
  -- | 'callerReference'
  Prelude.Text ->
  CreateHostedZone
newCreateHostedZone pName_ pCallerReference_ =
  CreateHostedZone'
    { delegationSetId =
        Prelude.Nothing,
      hostedZoneConfig = Prelude.Nothing,
      vpc = Prelude.Nothing,
      name = pName_,
      callerReference = pCallerReference_
    }

-- | If you want to associate a reusable delegation set with this hosted
-- zone, the ID that Amazon Route 53 assigned to the reusable delegation
-- set when you created it. For more information about reusable delegation
-- sets, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet>.
createHostedZone_delegationSetId :: Lens.Lens' CreateHostedZone (Prelude.Maybe ResourceId)
createHostedZone_delegationSetId = Lens.lens (\CreateHostedZone' {delegationSetId} -> delegationSetId) (\s@CreateHostedZone' {} a -> s {delegationSetId = a} :: CreateHostedZone)

-- | (Optional) A complex type that contains the following optional values:
--
-- -   For public and private hosted zones, an optional comment
--
-- -   For private hosted zones, an optional @PrivateZone@ element
--
-- If you don\'t specify a comment or the @PrivateZone@ element, omit
-- @HostedZoneConfig@ and the other elements.
createHostedZone_hostedZoneConfig :: Lens.Lens' CreateHostedZone (Prelude.Maybe HostedZoneConfig)
createHostedZone_hostedZoneConfig = Lens.lens (\CreateHostedZone' {hostedZoneConfig} -> hostedZoneConfig) (\s@CreateHostedZone' {} a -> s {hostedZoneConfig = a} :: CreateHostedZone)

-- | (Private hosted zones only) A complex type that contains information
-- about the Amazon VPC that you\'re associating with this hosted zone.
--
-- You can specify only one Amazon VPC when you create a private hosted
-- zone. If you are associating a VPC with a hosted zone with this request,
-- the paramaters @VPCId@ and @VPCRegion@ are also required.
--
-- To associate additional Amazon VPCs with the hosted zone, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone>
-- after you create a hosted zone.
createHostedZone_vpc :: Lens.Lens' CreateHostedZone (Prelude.Maybe VPC)
createHostedZone_vpc = Lens.lens (\CreateHostedZone' {vpc} -> vpc) (\s@CreateHostedZone' {} a -> s {vpc = a} :: CreateHostedZone)

-- | The name of the domain. Specify a fully qualified domain name, for
-- example, /www.example.com/. The trailing dot is optional; Amazon
-- Route 53 assumes that the domain name is fully qualified. This means
-- that Route 53 treats /www.example.com/ (without a trailing dot) and
-- /www.example.com./ (with a trailing dot) as identical.
--
-- If you\'re creating a public hosted zone, this is the name you have
-- registered with your DNS registrar. If your domain name is registered
-- with a registrar other than Route 53, change the name servers for your
-- domain to the set of @NameServers@ that @CreateHostedZone@ returns in
-- @DelegationSet@.
createHostedZone_name :: Lens.Lens' CreateHostedZone Prelude.Text
createHostedZone_name = Lens.lens (\CreateHostedZone' {name} -> name) (\s@CreateHostedZone' {} a -> s {name = a} :: CreateHostedZone)

-- | A unique string that identifies the request and that allows failed
-- @CreateHostedZone@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CallerReference@ string
-- every time you submit a @CreateHostedZone@ request. @CallerReference@
-- can be any unique string, for example, a date\/time stamp.
createHostedZone_callerReference :: Lens.Lens' CreateHostedZone Prelude.Text
createHostedZone_callerReference = Lens.lens (\CreateHostedZone' {callerReference} -> callerReference) (\s@CreateHostedZone' {} a -> s {callerReference = a} :: CreateHostedZone)

instance Core.AWSRequest CreateHostedZone where
  type
    AWSResponse CreateHostedZone =
      CreateHostedZoneResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateHostedZoneResponse'
            Prelude.<$> (x Data..@? "VPC")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HostedZone")
            Prelude.<*> (x Data..@ "ChangeInfo")
            Prelude.<*> (x Data..@ "DelegationSet")
            Prelude.<*> (h Data..# "Location")
      )

instance Prelude.Hashable CreateHostedZone where
  hashWithSalt _salt CreateHostedZone' {..} =
    _salt
      `Prelude.hashWithSalt` delegationSetId
      `Prelude.hashWithSalt` hostedZoneConfig
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData CreateHostedZone where
  rnf CreateHostedZone' {..} =
    Prelude.rnf delegationSetId `Prelude.seq`
      Prelude.rnf hostedZoneConfig `Prelude.seq`
        Prelude.rnf vpc `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf callerReference

instance Data.ToElement CreateHostedZone where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHostedZoneRequest"

instance Data.ToHeaders CreateHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateHostedZone where
  toPath = Prelude.const "/2013-04-01/hostedzone"

instance Data.ToQuery CreateHostedZone where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateHostedZone where
  toXML CreateHostedZone' {..} =
    Prelude.mconcat
      [ "DelegationSetId" Data.@= delegationSetId,
        "HostedZoneConfig" Data.@= hostedZoneConfig,
        "VPC" Data.@= vpc,
        "Name" Data.@= name,
        "CallerReference" Data.@= callerReference
      ]

-- | A complex type containing the response information for the hosted zone.
--
-- /See:/ 'newCreateHostedZoneResponse' smart constructor.
data CreateHostedZoneResponse = CreateHostedZoneResponse'
  { -- | A complex type that contains information about an Amazon VPC that you
    -- associated with this hosted zone.
    vpc :: Prelude.Maybe VPC,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains general information about the hosted zone.
    hostedZone :: HostedZone,
    -- | A complex type that contains information about the @CreateHostedZone@
    -- request.
    changeInfo :: ChangeInfo,
    -- | A complex type that describes the name servers for this hosted zone.
    delegationSet :: DelegationSet,
    -- | The unique URL representing the new hosted zone.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'createHostedZoneResponse_vpc' - A complex type that contains information about an Amazon VPC that you
-- associated with this hosted zone.
--
-- 'httpStatus', 'createHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'hostedZone', 'createHostedZoneResponse_hostedZone' - A complex type that contains general information about the hosted zone.
--
-- 'changeInfo', 'createHostedZoneResponse_changeInfo' - A complex type that contains information about the @CreateHostedZone@
-- request.
--
-- 'delegationSet', 'createHostedZoneResponse_delegationSet' - A complex type that describes the name servers for this hosted zone.
--
-- 'location', 'createHostedZoneResponse_location' - The unique URL representing the new hosted zone.
newCreateHostedZoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostedZone'
  HostedZone ->
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'delegationSet'
  DelegationSet ->
  -- | 'location'
  Prelude.Text ->
  CreateHostedZoneResponse
newCreateHostedZoneResponse
  pHttpStatus_
  pHostedZone_
  pChangeInfo_
  pDelegationSet_
  pLocation_ =
    CreateHostedZoneResponse'
      { vpc = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hostedZone = pHostedZone_,
        changeInfo = pChangeInfo_,
        delegationSet = pDelegationSet_,
        location = pLocation_
      }

-- | A complex type that contains information about an Amazon VPC that you
-- associated with this hosted zone.
createHostedZoneResponse_vpc :: Lens.Lens' CreateHostedZoneResponse (Prelude.Maybe VPC)
createHostedZoneResponse_vpc = Lens.lens (\CreateHostedZoneResponse' {vpc} -> vpc) (\s@CreateHostedZoneResponse' {} a -> s {vpc = a} :: CreateHostedZoneResponse)

-- | The response's http status code.
createHostedZoneResponse_httpStatus :: Lens.Lens' CreateHostedZoneResponse Prelude.Int
createHostedZoneResponse_httpStatus = Lens.lens (\CreateHostedZoneResponse' {httpStatus} -> httpStatus) (\s@CreateHostedZoneResponse' {} a -> s {httpStatus = a} :: CreateHostedZoneResponse)

-- | A complex type that contains general information about the hosted zone.
createHostedZoneResponse_hostedZone :: Lens.Lens' CreateHostedZoneResponse HostedZone
createHostedZoneResponse_hostedZone = Lens.lens (\CreateHostedZoneResponse' {hostedZone} -> hostedZone) (\s@CreateHostedZoneResponse' {} a -> s {hostedZone = a} :: CreateHostedZoneResponse)

-- | A complex type that contains information about the @CreateHostedZone@
-- request.
createHostedZoneResponse_changeInfo :: Lens.Lens' CreateHostedZoneResponse ChangeInfo
createHostedZoneResponse_changeInfo = Lens.lens (\CreateHostedZoneResponse' {changeInfo} -> changeInfo) (\s@CreateHostedZoneResponse' {} a -> s {changeInfo = a} :: CreateHostedZoneResponse)

-- | A complex type that describes the name servers for this hosted zone.
createHostedZoneResponse_delegationSet :: Lens.Lens' CreateHostedZoneResponse DelegationSet
createHostedZoneResponse_delegationSet = Lens.lens (\CreateHostedZoneResponse' {delegationSet} -> delegationSet) (\s@CreateHostedZoneResponse' {} a -> s {delegationSet = a} :: CreateHostedZoneResponse)

-- | The unique URL representing the new hosted zone.
createHostedZoneResponse_location :: Lens.Lens' CreateHostedZoneResponse Prelude.Text
createHostedZoneResponse_location = Lens.lens (\CreateHostedZoneResponse' {location} -> location) (\s@CreateHostedZoneResponse' {} a -> s {location = a} :: CreateHostedZoneResponse)

instance Prelude.NFData CreateHostedZoneResponse where
  rnf CreateHostedZoneResponse' {..} =
    Prelude.rnf vpc `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf hostedZone `Prelude.seq`
          Prelude.rnf changeInfo `Prelude.seq`
            Prelude.rnf delegationSet `Prelude.seq`
              Prelude.rnf location
