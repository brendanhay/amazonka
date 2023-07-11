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
-- Module      : Amazonka.EC2.CreateVerifiedAccessEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An Amazon Web Services Verified Access endpoint is where you define your
-- application along with an optional endpoint-level access policy.
module Amazonka.EC2.CreateVerifiedAccessEndpoint
  ( -- * Creating a Request
    CreateVerifiedAccessEndpoint (..),
    newCreateVerifiedAccessEndpoint,

    -- * Request Lenses
    createVerifiedAccessEndpoint_clientToken,
    createVerifiedAccessEndpoint_description,
    createVerifiedAccessEndpoint_dryRun,
    createVerifiedAccessEndpoint_loadBalancerOptions,
    createVerifiedAccessEndpoint_networkInterfaceOptions,
    createVerifiedAccessEndpoint_policyDocument,
    createVerifiedAccessEndpoint_securityGroupIds,
    createVerifiedAccessEndpoint_tagSpecifications,
    createVerifiedAccessEndpoint_verifiedAccessGroupId,
    createVerifiedAccessEndpoint_endpointType,
    createVerifiedAccessEndpoint_attachmentType,
    createVerifiedAccessEndpoint_domainCertificateArn,
    createVerifiedAccessEndpoint_applicationDomain,
    createVerifiedAccessEndpoint_endpointDomainPrefix,

    -- * Destructuring the Response
    CreateVerifiedAccessEndpointResponse (..),
    newCreateVerifiedAccessEndpointResponse,

    -- * Response Lenses
    createVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    createVerifiedAccessEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVerifiedAccessEndpoint' smart constructor.
data CreateVerifiedAccessEndpoint = CreateVerifiedAccessEndpoint'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The load balancer details if creating the Amazon Web Services Verified
    -- Access endpoint as @load-balancer@type.
    loadBalancerOptions :: Prelude.Maybe CreateVerifiedAccessEndpointLoadBalancerOptions,
    -- | The network interface details if creating the Amazon Web Services
    -- Verified Access endpoint as @network-interface@type.
    networkInterfaceOptions :: Prelude.Maybe CreateVerifiedAccessEndpointEniOptions,
    -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 security groups to associate with the Amazon Web Services
    -- Verified Access endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to assign to the Amazon Web Services Verified Access endpoint.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the Verified Access group to associate the endpoint with.
    verifiedAccessGroupId :: Prelude.Text,
    -- | The type of Amazon Web Services Verified Access endpoint to create.
    endpointType :: VerifiedAccessEndpointType,
    -- | The Amazon Web Services network component Verified Access attaches to.
    attachmentType :: VerifiedAccessEndpointAttachmentType,
    -- | The ARN of the public TLS\/SSL certificate in Amazon Web Services
    -- Certificate Manager to associate with the endpoint. The CN in the
    -- certificate must match the DNS name your end users will use to reach
    -- your application.
    domainCertificateArn :: Prelude.Text,
    -- | The DNS name for users to reach your application.
    applicationDomain :: Prelude.Text,
    -- | A custom identifier that gets prepended to a DNS name that is generated
    -- for the endpoint.
    endpointDomainPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVerifiedAccessEndpoint_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createVerifiedAccessEndpoint_description' - A description for the Amazon Web Services Verified Access endpoint.
--
-- 'dryRun', 'createVerifiedAccessEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'loadBalancerOptions', 'createVerifiedAccessEndpoint_loadBalancerOptions' - The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
--
-- 'networkInterfaceOptions', 'createVerifiedAccessEndpoint_networkInterfaceOptions' - The network interface details if creating the Amazon Web Services
-- Verified Access endpoint as @network-interface@type.
--
-- 'policyDocument', 'createVerifiedAccessEndpoint_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'securityGroupIds', 'createVerifiedAccessEndpoint_securityGroupIds' - The Amazon EC2 security groups to associate with the Amazon Web Services
-- Verified Access endpoint.
--
-- 'tagSpecifications', 'createVerifiedAccessEndpoint_tagSpecifications' - The tags to assign to the Amazon Web Services Verified Access endpoint.
--
-- 'verifiedAccessGroupId', 'createVerifiedAccessEndpoint_verifiedAccessGroupId' - The ID of the Verified Access group to associate the endpoint with.
--
-- 'endpointType', 'createVerifiedAccessEndpoint_endpointType' - The type of Amazon Web Services Verified Access endpoint to create.
--
-- 'attachmentType', 'createVerifiedAccessEndpoint_attachmentType' - The Amazon Web Services network component Verified Access attaches to.
--
-- 'domainCertificateArn', 'createVerifiedAccessEndpoint_domainCertificateArn' - The ARN of the public TLS\/SSL certificate in Amazon Web Services
-- Certificate Manager to associate with the endpoint. The CN in the
-- certificate must match the DNS name your end users will use to reach
-- your application.
--
-- 'applicationDomain', 'createVerifiedAccessEndpoint_applicationDomain' - The DNS name for users to reach your application.
--
-- 'endpointDomainPrefix', 'createVerifiedAccessEndpoint_endpointDomainPrefix' - A custom identifier that gets prepended to a DNS name that is generated
-- for the endpoint.
newCreateVerifiedAccessEndpoint ::
  -- | 'verifiedAccessGroupId'
  Prelude.Text ->
  -- | 'endpointType'
  VerifiedAccessEndpointType ->
  -- | 'attachmentType'
  VerifiedAccessEndpointAttachmentType ->
  -- | 'domainCertificateArn'
  Prelude.Text ->
  -- | 'applicationDomain'
  Prelude.Text ->
  -- | 'endpointDomainPrefix'
  Prelude.Text ->
  CreateVerifiedAccessEndpoint
newCreateVerifiedAccessEndpoint
  pVerifiedAccessGroupId_
  pEndpointType_
  pAttachmentType_
  pDomainCertificateArn_
  pApplicationDomain_
  pEndpointDomainPrefix_ =
    CreateVerifiedAccessEndpoint'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        loadBalancerOptions = Prelude.Nothing,
        networkInterfaceOptions = Prelude.Nothing,
        policyDocument = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        verifiedAccessGroupId =
          pVerifiedAccessGroupId_,
        endpointType = pEndpointType_,
        attachmentType = pAttachmentType_,
        domainCertificateArn = pDomainCertificateArn_,
        applicationDomain = pApplicationDomain_,
        endpointDomainPrefix = pEndpointDomainPrefix_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createVerifiedAccessEndpoint_clientToken :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
createVerifiedAccessEndpoint_clientToken = Lens.lens (\CreateVerifiedAccessEndpoint' {clientToken} -> clientToken) (\s@CreateVerifiedAccessEndpoint' {} a -> s {clientToken = a} :: CreateVerifiedAccessEndpoint)

-- | A description for the Amazon Web Services Verified Access endpoint.
createVerifiedAccessEndpoint_description :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
createVerifiedAccessEndpoint_description = Lens.lens (\CreateVerifiedAccessEndpoint' {description} -> description) (\s@CreateVerifiedAccessEndpoint' {} a -> s {description = a} :: CreateVerifiedAccessEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVerifiedAccessEndpoint_dryRun :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe Prelude.Bool)
createVerifiedAccessEndpoint_dryRun = Lens.lens (\CreateVerifiedAccessEndpoint' {dryRun} -> dryRun) (\s@CreateVerifiedAccessEndpoint' {} a -> s {dryRun = a} :: CreateVerifiedAccessEndpoint)

-- | The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
createVerifiedAccessEndpoint_loadBalancerOptions :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe CreateVerifiedAccessEndpointLoadBalancerOptions)
createVerifiedAccessEndpoint_loadBalancerOptions = Lens.lens (\CreateVerifiedAccessEndpoint' {loadBalancerOptions} -> loadBalancerOptions) (\s@CreateVerifiedAccessEndpoint' {} a -> s {loadBalancerOptions = a} :: CreateVerifiedAccessEndpoint)

-- | The network interface details if creating the Amazon Web Services
-- Verified Access endpoint as @network-interface@type.
createVerifiedAccessEndpoint_networkInterfaceOptions :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe CreateVerifiedAccessEndpointEniOptions)
createVerifiedAccessEndpoint_networkInterfaceOptions = Lens.lens (\CreateVerifiedAccessEndpoint' {networkInterfaceOptions} -> networkInterfaceOptions) (\s@CreateVerifiedAccessEndpoint' {} a -> s {networkInterfaceOptions = a} :: CreateVerifiedAccessEndpoint)

-- | The Amazon Web Services Verified Access policy document.
createVerifiedAccessEndpoint_policyDocument :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
createVerifiedAccessEndpoint_policyDocument = Lens.lens (\CreateVerifiedAccessEndpoint' {policyDocument} -> policyDocument) (\s@CreateVerifiedAccessEndpoint' {} a -> s {policyDocument = a} :: CreateVerifiedAccessEndpoint)

-- | The Amazon EC2 security groups to associate with the Amazon Web Services
-- Verified Access endpoint.
createVerifiedAccessEndpoint_securityGroupIds :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe [Prelude.Text])
createVerifiedAccessEndpoint_securityGroupIds = Lens.lens (\CreateVerifiedAccessEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateVerifiedAccessEndpoint' {} a -> s {securityGroupIds = a} :: CreateVerifiedAccessEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The tags to assign to the Amazon Web Services Verified Access endpoint.
createVerifiedAccessEndpoint_tagSpecifications :: Lens.Lens' CreateVerifiedAccessEndpoint (Prelude.Maybe [TagSpecification])
createVerifiedAccessEndpoint_tagSpecifications = Lens.lens (\CreateVerifiedAccessEndpoint' {tagSpecifications} -> tagSpecifications) (\s@CreateVerifiedAccessEndpoint' {} a -> s {tagSpecifications = a} :: CreateVerifiedAccessEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Verified Access group to associate the endpoint with.
createVerifiedAccessEndpoint_verifiedAccessGroupId :: Lens.Lens' CreateVerifiedAccessEndpoint Prelude.Text
createVerifiedAccessEndpoint_verifiedAccessGroupId = Lens.lens (\CreateVerifiedAccessEndpoint' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@CreateVerifiedAccessEndpoint' {} a -> s {verifiedAccessGroupId = a} :: CreateVerifiedAccessEndpoint)

-- | The type of Amazon Web Services Verified Access endpoint to create.
createVerifiedAccessEndpoint_endpointType :: Lens.Lens' CreateVerifiedAccessEndpoint VerifiedAccessEndpointType
createVerifiedAccessEndpoint_endpointType = Lens.lens (\CreateVerifiedAccessEndpoint' {endpointType} -> endpointType) (\s@CreateVerifiedAccessEndpoint' {} a -> s {endpointType = a} :: CreateVerifiedAccessEndpoint)

-- | The Amazon Web Services network component Verified Access attaches to.
createVerifiedAccessEndpoint_attachmentType :: Lens.Lens' CreateVerifiedAccessEndpoint VerifiedAccessEndpointAttachmentType
createVerifiedAccessEndpoint_attachmentType = Lens.lens (\CreateVerifiedAccessEndpoint' {attachmentType} -> attachmentType) (\s@CreateVerifiedAccessEndpoint' {} a -> s {attachmentType = a} :: CreateVerifiedAccessEndpoint)

-- | The ARN of the public TLS\/SSL certificate in Amazon Web Services
-- Certificate Manager to associate with the endpoint. The CN in the
-- certificate must match the DNS name your end users will use to reach
-- your application.
createVerifiedAccessEndpoint_domainCertificateArn :: Lens.Lens' CreateVerifiedAccessEndpoint Prelude.Text
createVerifiedAccessEndpoint_domainCertificateArn = Lens.lens (\CreateVerifiedAccessEndpoint' {domainCertificateArn} -> domainCertificateArn) (\s@CreateVerifiedAccessEndpoint' {} a -> s {domainCertificateArn = a} :: CreateVerifiedAccessEndpoint)

-- | The DNS name for users to reach your application.
createVerifiedAccessEndpoint_applicationDomain :: Lens.Lens' CreateVerifiedAccessEndpoint Prelude.Text
createVerifiedAccessEndpoint_applicationDomain = Lens.lens (\CreateVerifiedAccessEndpoint' {applicationDomain} -> applicationDomain) (\s@CreateVerifiedAccessEndpoint' {} a -> s {applicationDomain = a} :: CreateVerifiedAccessEndpoint)

-- | A custom identifier that gets prepended to a DNS name that is generated
-- for the endpoint.
createVerifiedAccessEndpoint_endpointDomainPrefix :: Lens.Lens' CreateVerifiedAccessEndpoint Prelude.Text
createVerifiedAccessEndpoint_endpointDomainPrefix = Lens.lens (\CreateVerifiedAccessEndpoint' {endpointDomainPrefix} -> endpointDomainPrefix) (\s@CreateVerifiedAccessEndpoint' {} a -> s {endpointDomainPrefix = a} :: CreateVerifiedAccessEndpoint)

instance Core.AWSRequest CreateVerifiedAccessEndpoint where
  type
    AWSResponse CreateVerifiedAccessEndpoint =
      CreateVerifiedAccessEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVerifiedAccessEndpointResponse'
            Prelude.<$> (x Data..@? "verifiedAccessEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVerifiedAccessEndpoint
  where
  hashWithSalt _salt CreateVerifiedAccessEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` loadBalancerOptions
      `Prelude.hashWithSalt` networkInterfaceOptions
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` verifiedAccessGroupId
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` attachmentType
      `Prelude.hashWithSalt` domainCertificateArn
      `Prelude.hashWithSalt` applicationDomain
      `Prelude.hashWithSalt` endpointDomainPrefix

instance Prelude.NFData CreateVerifiedAccessEndpoint where
  rnf CreateVerifiedAccessEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf loadBalancerOptions
      `Prelude.seq` Prelude.rnf networkInterfaceOptions
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf domainCertificateArn
      `Prelude.seq` Prelude.rnf applicationDomain
      `Prelude.seq` Prelude.rnf endpointDomainPrefix

instance Data.ToHeaders CreateVerifiedAccessEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVerifiedAccessEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVerifiedAccessEndpoint where
  toQuery CreateVerifiedAccessEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateVerifiedAccessEndpoint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "LoadBalancerOptions" Data.=: loadBalancerOptions,
        "NetworkInterfaceOptions"
          Data.=: networkInterfaceOptions,
        "PolicyDocument" Data.=: policyDocument,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId,
        "EndpointType" Data.=: endpointType,
        "AttachmentType" Data.=: attachmentType,
        "DomainCertificateArn" Data.=: domainCertificateArn,
        "ApplicationDomain" Data.=: applicationDomain,
        "EndpointDomainPrefix" Data.=: endpointDomainPrefix
      ]

-- | /See:/ 'newCreateVerifiedAccessEndpointResponse' smart constructor.
data CreateVerifiedAccessEndpointResponse = CreateVerifiedAccessEndpointResponse'
  { -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpoint :: Prelude.Maybe VerifiedAccessEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessEndpoint', 'createVerifiedAccessEndpointResponse_verifiedAccessEndpoint' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'httpStatus', 'createVerifiedAccessEndpointResponse_httpStatus' - The response's http status code.
newCreateVerifiedAccessEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVerifiedAccessEndpointResponse
newCreateVerifiedAccessEndpointResponse pHttpStatus_ =
  CreateVerifiedAccessEndpointResponse'
    { verifiedAccessEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access endpoint.
createVerifiedAccessEndpointResponse_verifiedAccessEndpoint :: Lens.Lens' CreateVerifiedAccessEndpointResponse (Prelude.Maybe VerifiedAccessEndpoint)
createVerifiedAccessEndpointResponse_verifiedAccessEndpoint = Lens.lens (\CreateVerifiedAccessEndpointResponse' {verifiedAccessEndpoint} -> verifiedAccessEndpoint) (\s@CreateVerifiedAccessEndpointResponse' {} a -> s {verifiedAccessEndpoint = a} :: CreateVerifiedAccessEndpointResponse)

-- | The response's http status code.
createVerifiedAccessEndpointResponse_httpStatus :: Lens.Lens' CreateVerifiedAccessEndpointResponse Prelude.Int
createVerifiedAccessEndpointResponse_httpStatus = Lens.lens (\CreateVerifiedAccessEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateVerifiedAccessEndpointResponse' {} a -> s {httpStatus = a} :: CreateVerifiedAccessEndpointResponse)

instance
  Prelude.NFData
    CreateVerifiedAccessEndpointResponse
  where
  rnf CreateVerifiedAccessEndpointResponse' {..} =
    Prelude.rnf verifiedAccessEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
