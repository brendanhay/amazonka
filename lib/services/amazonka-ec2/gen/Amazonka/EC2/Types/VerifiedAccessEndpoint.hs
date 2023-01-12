{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.VerifiedAccessEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VerifiedAccessEndpointAttachmentType
import Amazonka.EC2.Types.VerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointStatus
import Amazonka.EC2.Types.VerifiedAccessEndpointType
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Web Services Verified Access endpoint specifies the
-- application that Amazon Web Services Verified Access provides access to.
-- It must be attached to an Amazon Web Services Verified Access group. An
-- Amazon Web Services Verified Access endpoint must also have an attached
-- access policy before you attached it to a group.
--
-- /See:/ 'newVerifiedAccessEndpoint' smart constructor.
data VerifiedAccessEndpoint = VerifiedAccessEndpoint'
  { -- | The DNS name for users to reach your application.
    applicationDomain :: Prelude.Maybe Prelude.Text,
    -- | The type of attachment used to provide connectivity between the Amazon
    -- Web Services Verified Access endpoint and the application.
    attachmentType :: Prelude.Maybe VerifiedAccessEndpointAttachmentType,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The deletion time.
    deletionTime :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Returned if endpoint has a device trust provider attached.
    deviceValidationDomain :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a public TLS\/SSL certificate imported into or created with
    -- ACM.
    domainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | A DNS name that is generated for the endpoint.
    endpointDomain :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services Verified Access endpoint. Incoming
    -- application requests will be sent to an IP address, load balancer or a
    -- network interface depending on the endpoint type specified.
    endpointType :: Prelude.Maybe VerifiedAccessEndpointType,
    -- | The last updated time.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The load balancer details if creating the Amazon Web Services Verified
    -- Access endpoint as @load-balancer@type.
    loadBalancerOptions :: Prelude.Maybe VerifiedAccessEndpointLoadBalancerOptions,
    -- | The options for network-interface type endpoint.
    networkInterfaceOptions :: Prelude.Maybe VerifiedAccessEndpointEniOptions,
    -- | The IDs of the security groups for the endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The endpoint status.
    status :: Prelude.Maybe VerifiedAccessEndpointStatus,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationDomain', 'verifiedAccessEndpoint_applicationDomain' - The DNS name for users to reach your application.
--
-- 'attachmentType', 'verifiedAccessEndpoint_attachmentType' - The type of attachment used to provide connectivity between the Amazon
-- Web Services Verified Access endpoint and the application.
--
-- 'creationTime', 'verifiedAccessEndpoint_creationTime' - The creation time.
--
-- 'deletionTime', 'verifiedAccessEndpoint_deletionTime' - The deletion time.
--
-- 'description', 'verifiedAccessEndpoint_description' - A description for the Amazon Web Services Verified Access endpoint.
--
-- 'deviceValidationDomain', 'verifiedAccessEndpoint_deviceValidationDomain' - Returned if endpoint has a device trust provider attached.
--
-- 'domainCertificateArn', 'verifiedAccessEndpoint_domainCertificateArn' - The ARN of a public TLS\/SSL certificate imported into or created with
-- ACM.
--
-- 'endpointDomain', 'verifiedAccessEndpoint_endpointDomain' - A DNS name that is generated for the endpoint.
--
-- 'endpointType', 'verifiedAccessEndpoint_endpointType' - The type of Amazon Web Services Verified Access endpoint. Incoming
-- application requests will be sent to an IP address, load balancer or a
-- network interface depending on the endpoint type specified.
--
-- 'lastUpdatedTime', 'verifiedAccessEndpoint_lastUpdatedTime' - The last updated time.
--
-- 'loadBalancerOptions', 'verifiedAccessEndpoint_loadBalancerOptions' - The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
--
-- 'networkInterfaceOptions', 'verifiedAccessEndpoint_networkInterfaceOptions' - The options for network-interface type endpoint.
--
-- 'securityGroupIds', 'verifiedAccessEndpoint_securityGroupIds' - The IDs of the security groups for the endpoint.
--
-- 'status', 'verifiedAccessEndpoint_status' - The endpoint status.
--
-- 'tags', 'verifiedAccessEndpoint_tags' - The tags.
--
-- 'verifiedAccessEndpointId', 'verifiedAccessEndpoint_verifiedAccessEndpointId' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'verifiedAccessGroupId', 'verifiedAccessEndpoint_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
--
-- 'verifiedAccessInstanceId', 'verifiedAccessEndpoint_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newVerifiedAccessEndpoint ::
  VerifiedAccessEndpoint
newVerifiedAccessEndpoint =
  VerifiedAccessEndpoint'
    { applicationDomain =
        Prelude.Nothing,
      attachmentType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceValidationDomain = Prelude.Nothing,
      domainCertificateArn = Prelude.Nothing,
      endpointDomain = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      loadBalancerOptions = Prelude.Nothing,
      networkInterfaceOptions = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      verifiedAccessEndpointId = Prelude.Nothing,
      verifiedAccessGroupId = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing
    }

-- | The DNS name for users to reach your application.
verifiedAccessEndpoint_applicationDomain :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_applicationDomain = Lens.lens (\VerifiedAccessEndpoint' {applicationDomain} -> applicationDomain) (\s@VerifiedAccessEndpoint' {} a -> s {applicationDomain = a} :: VerifiedAccessEndpoint)

-- | The type of attachment used to provide connectivity between the Amazon
-- Web Services Verified Access endpoint and the application.
verifiedAccessEndpoint_attachmentType :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe VerifiedAccessEndpointAttachmentType)
verifiedAccessEndpoint_attachmentType = Lens.lens (\VerifiedAccessEndpoint' {attachmentType} -> attachmentType) (\s@VerifiedAccessEndpoint' {} a -> s {attachmentType = a} :: VerifiedAccessEndpoint)

-- | The creation time.
verifiedAccessEndpoint_creationTime :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_creationTime = Lens.lens (\VerifiedAccessEndpoint' {creationTime} -> creationTime) (\s@VerifiedAccessEndpoint' {} a -> s {creationTime = a} :: VerifiedAccessEndpoint)

-- | The deletion time.
verifiedAccessEndpoint_deletionTime :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_deletionTime = Lens.lens (\VerifiedAccessEndpoint' {deletionTime} -> deletionTime) (\s@VerifiedAccessEndpoint' {} a -> s {deletionTime = a} :: VerifiedAccessEndpoint)

-- | A description for the Amazon Web Services Verified Access endpoint.
verifiedAccessEndpoint_description :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_description = Lens.lens (\VerifiedAccessEndpoint' {description} -> description) (\s@VerifiedAccessEndpoint' {} a -> s {description = a} :: VerifiedAccessEndpoint)

-- | Returned if endpoint has a device trust provider attached.
verifiedAccessEndpoint_deviceValidationDomain :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_deviceValidationDomain = Lens.lens (\VerifiedAccessEndpoint' {deviceValidationDomain} -> deviceValidationDomain) (\s@VerifiedAccessEndpoint' {} a -> s {deviceValidationDomain = a} :: VerifiedAccessEndpoint)

-- | The ARN of a public TLS\/SSL certificate imported into or created with
-- ACM.
verifiedAccessEndpoint_domainCertificateArn :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_domainCertificateArn = Lens.lens (\VerifiedAccessEndpoint' {domainCertificateArn} -> domainCertificateArn) (\s@VerifiedAccessEndpoint' {} a -> s {domainCertificateArn = a} :: VerifiedAccessEndpoint)

-- | A DNS name that is generated for the endpoint.
verifiedAccessEndpoint_endpointDomain :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_endpointDomain = Lens.lens (\VerifiedAccessEndpoint' {endpointDomain} -> endpointDomain) (\s@VerifiedAccessEndpoint' {} a -> s {endpointDomain = a} :: VerifiedAccessEndpoint)

-- | The type of Amazon Web Services Verified Access endpoint. Incoming
-- application requests will be sent to an IP address, load balancer or a
-- network interface depending on the endpoint type specified.
verifiedAccessEndpoint_endpointType :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe VerifiedAccessEndpointType)
verifiedAccessEndpoint_endpointType = Lens.lens (\VerifiedAccessEndpoint' {endpointType} -> endpointType) (\s@VerifiedAccessEndpoint' {} a -> s {endpointType = a} :: VerifiedAccessEndpoint)

-- | The last updated time.
verifiedAccessEndpoint_lastUpdatedTime :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_lastUpdatedTime = Lens.lens (\VerifiedAccessEndpoint' {lastUpdatedTime} -> lastUpdatedTime) (\s@VerifiedAccessEndpoint' {} a -> s {lastUpdatedTime = a} :: VerifiedAccessEndpoint)

-- | The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
verifiedAccessEndpoint_loadBalancerOptions :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe VerifiedAccessEndpointLoadBalancerOptions)
verifiedAccessEndpoint_loadBalancerOptions = Lens.lens (\VerifiedAccessEndpoint' {loadBalancerOptions} -> loadBalancerOptions) (\s@VerifiedAccessEndpoint' {} a -> s {loadBalancerOptions = a} :: VerifiedAccessEndpoint)

-- | The options for network-interface type endpoint.
verifiedAccessEndpoint_networkInterfaceOptions :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe VerifiedAccessEndpointEniOptions)
verifiedAccessEndpoint_networkInterfaceOptions = Lens.lens (\VerifiedAccessEndpoint' {networkInterfaceOptions} -> networkInterfaceOptions) (\s@VerifiedAccessEndpoint' {} a -> s {networkInterfaceOptions = a} :: VerifiedAccessEndpoint)

-- | The IDs of the security groups for the endpoint.
verifiedAccessEndpoint_securityGroupIds :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe [Prelude.Text])
verifiedAccessEndpoint_securityGroupIds = Lens.lens (\VerifiedAccessEndpoint' {securityGroupIds} -> securityGroupIds) (\s@VerifiedAccessEndpoint' {} a -> s {securityGroupIds = a} :: VerifiedAccessEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint status.
verifiedAccessEndpoint_status :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe VerifiedAccessEndpointStatus)
verifiedAccessEndpoint_status = Lens.lens (\VerifiedAccessEndpoint' {status} -> status) (\s@VerifiedAccessEndpoint' {} a -> s {status = a} :: VerifiedAccessEndpoint)

-- | The tags.
verifiedAccessEndpoint_tags :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe [Tag])
verifiedAccessEndpoint_tags = Lens.lens (\VerifiedAccessEndpoint' {tags} -> tags) (\s@VerifiedAccessEndpoint' {} a -> s {tags = a} :: VerifiedAccessEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Verified Access endpoint.
verifiedAccessEndpoint_verifiedAccessEndpointId :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_verifiedAccessEndpointId = Lens.lens (\VerifiedAccessEndpoint' {verifiedAccessEndpointId} -> verifiedAccessEndpointId) (\s@VerifiedAccessEndpoint' {} a -> s {verifiedAccessEndpointId = a} :: VerifiedAccessEndpoint)

-- | The ID of the Amazon Web Services Verified Access group.
verifiedAccessEndpoint_verifiedAccessGroupId :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_verifiedAccessGroupId = Lens.lens (\VerifiedAccessEndpoint' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@VerifiedAccessEndpoint' {} a -> s {verifiedAccessGroupId = a} :: VerifiedAccessEndpoint)

-- | The ID of the Amazon Web Services Verified Access instance.
verifiedAccessEndpoint_verifiedAccessInstanceId :: Lens.Lens' VerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
verifiedAccessEndpoint_verifiedAccessInstanceId = Lens.lens (\VerifiedAccessEndpoint' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@VerifiedAccessEndpoint' {} a -> s {verifiedAccessInstanceId = a} :: VerifiedAccessEndpoint)

instance Data.FromXML VerifiedAccessEndpoint where
  parseXML x =
    VerifiedAccessEndpoint'
      Prelude.<$> (x Data..@? "applicationDomain")
      Prelude.<*> (x Data..@? "attachmentType")
      Prelude.<*> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "deletionTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "deviceValidationDomain")
      Prelude.<*> (x Data..@? "domainCertificateArn")
      Prelude.<*> (x Data..@? "endpointDomain")
      Prelude.<*> (x Data..@? "endpointType")
      Prelude.<*> (x Data..@? "lastUpdatedTime")
      Prelude.<*> (x Data..@? "loadBalancerOptions")
      Prelude.<*> (x Data..@? "networkInterfaceOptions")
      Prelude.<*> ( x Data..@? "securityGroupIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "verifiedAccessEndpointId")
      Prelude.<*> (x Data..@? "verifiedAccessGroupId")
      Prelude.<*> (x Data..@? "verifiedAccessInstanceId")

instance Prelude.Hashable VerifiedAccessEndpoint where
  hashWithSalt _salt VerifiedAccessEndpoint' {..} =
    _salt `Prelude.hashWithSalt` applicationDomain
      `Prelude.hashWithSalt` attachmentType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deletionTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceValidationDomain
      `Prelude.hashWithSalt` domainCertificateArn
      `Prelude.hashWithSalt` endpointDomain
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` loadBalancerOptions
      `Prelude.hashWithSalt` networkInterfaceOptions
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` verifiedAccessEndpointId
      `Prelude.hashWithSalt` verifiedAccessGroupId
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData VerifiedAccessEndpoint where
  rnf VerifiedAccessEndpoint' {..} =
    Prelude.rnf applicationDomain
      `Prelude.seq` Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceValidationDomain
      `Prelude.seq` Prelude.rnf domainCertificateArn
      `Prelude.seq` Prelude.rnf endpointDomain
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf loadBalancerOptions
      `Prelude.seq` Prelude.rnf networkInterfaceOptions
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf verifiedAccessEndpointId
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId
      `Prelude.seq` Prelude.rnf
        verifiedAccessInstanceId
