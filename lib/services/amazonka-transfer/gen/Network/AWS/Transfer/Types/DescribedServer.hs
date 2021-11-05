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
-- Module      : Amazonka.Transfer.Types.DescribedServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.Domain
import Amazonka.Transfer.Types.EndpointDetails
import Amazonka.Transfer.Types.EndpointType
import Amazonka.Transfer.Types.IdentityProviderDetails
import Amazonka.Transfer.Types.IdentityProviderType
import Amazonka.Transfer.Types.Protocol
import Amazonka.Transfer.Types.ProtocolDetails
import Amazonka.Transfer.Types.State
import Amazonka.Transfer.Types.Tag
import Amazonka.Transfer.Types.WorkflowDetails

-- | Describes the properties of a file transfer protocol-enabled server that
-- was specified.
--
-- /See:/ 'newDescribedServer' smart constructor.
data DescribedServer = DescribedServer'
  { -- | The protocol settings that are configured for your server.
    --
    -- Use the @PassiveIp@ parameter to indicate passive mode. Enter a single
    -- dotted-quad IPv4 address, such as the external IP address of a firewall,
    -- router, or load balancer.
    protocolDetails :: Prelude.Maybe ProtocolDetails,
    -- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
    -- Identity and Access Management (IAM) role that allows a server to turn
    -- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
    -- set, user activity can be viewed in your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the condition of a server for the server that was described. A
    -- value of @ONLINE@ indicates that the server can accept jobs and transfer
    -- files. A @State@ value of @OFFLINE@ means that the server cannot perform
    -- file transfer operations.
    --
    -- The states of @STARTING@ and @STOPPING@ indicate that the server is in
    -- an intermediate state, either not fully able to respond, or not fully
    -- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
    -- error condition.
    state :: Prelude.Maybe State,
    -- | Specifies the mode of authentication for a server. The default value is
    -- @SERVICE_MANAGED@, which allows you to store and access user credentials
    -- within the Amazon Web Services Transfer Family service.
    --
    -- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
    -- in Amazon Web Services Managed Active Directory or Microsoft Active
    -- Directory in your on-premises environment or in Amazon Web Services
    -- using AD Connectors. This option also requires you to provide a
    -- Directory ID using the @IdentityProviderDetails@ parameter.
    --
    -- Use the @API_GATEWAY@ value to integrate with an identity provider of
    -- your choosing. The @API_GATEWAY@ setting requires you to provide an API
    -- Gateway endpoint URL to call for authentication using the
    -- @IdentityProviderDetails@ parameter.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | Specifies the file transfer protocol or protocols over which your file
    -- transfer protocol client can connect to your server\'s endpoint. The
    -- available protocols are:
    --
    -- -   @SFTP@ (Secure Shell (SSH) File Transfer Protocol): File transfer
    --     over SSH
    --
    -- -   @FTPS@ (File Transfer Protocol Secure): File transfer with TLS
    --     encryption
    --
    -- -   @FTP@ (File Transfer Protocol): Unencrypted file transfer
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | Specifies the unique system-assigned identifier for a server that you
    -- instantiate.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the domain of the storage system that is used for file
    -- transfers.
    domain :: Prelude.Maybe Domain,
    -- | Defines the type of endpoint that your server is connected to. If your
    -- server is connected to a VPC endpoint, your server isn\'t accessible
    -- over the public internet.
    endpointType :: Prelude.Maybe EndpointType,
    -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
    -- key. This value is equivalent to the output of the
    -- @ssh-keygen -l -f my-new-server-key@ command.
    hostKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of users that are assigned to a server you
    -- specified with the @ServerId@.
    userCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
    -- certificate. Required when @Protocols@ is set to @FTPS@.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Specifies information to call a customer-supplied authentication API.
    -- This field is not populated when the @IdentityProviderType@ of a server
    -- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
    identityProviderDetails :: Prelude.Maybe IdentityProviderDetails,
    -- | Specifies the workflow ID for the workflow to assign and the execution
    -- role used for executing the workflow.
    workflowDetails :: Prelude.Maybe WorkflowDetails,
    -- | Specifies the key-value pairs that you can use to search for and group
    -- servers that were assigned to the server that was described.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The virtual private cloud (VPC) endpoint settings that are configured
    -- for your server. When you host your endpoint within your VPC, you can
    -- make it accessible only to resources within your VPC, or you can attach
    -- Elastic IP addresses and make it accessible to clients over the
    -- internet. Your VPC\'s default security groups are automatically assigned
    -- to your endpoint.
    endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | Specifies the unique Amazon Resource Name (ARN) of the server.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocolDetails', 'describedServer_protocolDetails' - The protocol settings that are configured for your server.
--
-- Use the @PassiveIp@ parameter to indicate passive mode. Enter a single
-- dotted-quad IPv4 address, such as the external IP address of a firewall,
-- router, or load balancer.
--
-- 'loggingRole', 'describedServer_loggingRole' - Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
--
-- 'state', 'describedServer_state' - Specifies the condition of a server for the server that was described. A
-- value of @ONLINE@ indicates that the server can accept jobs and transfer
-- files. A @State@ value of @OFFLINE@ means that the server cannot perform
-- file transfer operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
--
-- 'identityProviderType', 'describedServer_identityProviderType' - Specifies the mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Amazon Web Services Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Amazon Web Services Managed Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connectors. This option also requires you to provide a
-- Directory ID using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an API
-- Gateway endpoint URL to call for authentication using the
-- @IdentityProviderDetails@ parameter.
--
-- 'protocols', 'describedServer_protocols' - Specifies the file transfer protocol or protocols over which your file
-- transfer protocol client can connect to your server\'s endpoint. The
-- available protocols are:
--
-- -   @SFTP@ (Secure Shell (SSH) File Transfer Protocol): File transfer
--     over SSH
--
-- -   @FTPS@ (File Transfer Protocol Secure): File transfer with TLS
--     encryption
--
-- -   @FTP@ (File Transfer Protocol): Unencrypted file transfer
--
-- 'serverId', 'describedServer_serverId' - Specifies the unique system-assigned identifier for a server that you
-- instantiate.
--
-- 'domain', 'describedServer_domain' - Specifies the domain of the storage system that is used for file
-- transfers.
--
-- 'endpointType', 'describedServer_endpointType' - Defines the type of endpoint that your server is connected to. If your
-- server is connected to a VPC endpoint, your server isn\'t accessible
-- over the public internet.
--
-- 'securityPolicyName', 'describedServer_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
--
-- 'hostKeyFingerprint', 'describedServer_hostKeyFingerprint' - Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
-- key. This value is equivalent to the output of the
-- @ssh-keygen -l -f my-new-server-key@ command.
--
-- 'userCount', 'describedServer_userCount' - Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
--
-- 'certificate', 'describedServer_certificate' - Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
--
-- 'identityProviderDetails', 'describedServer_identityProviderDetails' - Specifies information to call a customer-supplied authentication API.
-- This field is not populated when the @IdentityProviderType@ of a server
-- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
--
-- 'workflowDetails', 'describedServer_workflowDetails' - Specifies the workflow ID for the workflow to assign and the execution
-- role used for executing the workflow.
--
-- 'tags', 'describedServer_tags' - Specifies the key-value pairs that you can use to search for and group
-- servers that were assigned to the server that was described.
--
-- 'endpointDetails', 'describedServer_endpointDetails' - The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make it accessible only to resources within your VPC, or you can attach
-- Elastic IP addresses and make it accessible to clients over the
-- internet. Your VPC\'s default security groups are automatically assigned
-- to your endpoint.
--
-- 'arn', 'describedServer_arn' - Specifies the unique Amazon Resource Name (ARN) of the server.
newDescribedServer ::
  -- | 'arn'
  Prelude.Text ->
  DescribedServer
newDescribedServer pArn_ =
  DescribedServer'
    { protocolDetails = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      state = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      protocols = Prelude.Nothing,
      serverId = Prelude.Nothing,
      domain = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      securityPolicyName = Prelude.Nothing,
      hostKeyFingerprint = Prelude.Nothing,
      userCount = Prelude.Nothing,
      certificate = Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      workflowDetails = Prelude.Nothing,
      tags = Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      arn = pArn_
    }

-- | The protocol settings that are configured for your server.
--
-- Use the @PassiveIp@ parameter to indicate passive mode. Enter a single
-- dotted-quad IPv4 address, such as the external IP address of a firewall,
-- router, or load balancer.
describedServer_protocolDetails :: Lens.Lens' DescribedServer (Prelude.Maybe ProtocolDetails)
describedServer_protocolDetails = Lens.lens (\DescribedServer' {protocolDetails} -> protocolDetails) (\s@DescribedServer' {} a -> s {protocolDetails = a} :: DescribedServer)

-- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
describedServer_loggingRole :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_loggingRole = Lens.lens (\DescribedServer' {loggingRole} -> loggingRole) (\s@DescribedServer' {} a -> s {loggingRole = a} :: DescribedServer)

-- | Specifies the condition of a server for the server that was described. A
-- value of @ONLINE@ indicates that the server can accept jobs and transfer
-- files. A @State@ value of @OFFLINE@ means that the server cannot perform
-- file transfer operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
describedServer_state :: Lens.Lens' DescribedServer (Prelude.Maybe State)
describedServer_state = Lens.lens (\DescribedServer' {state} -> state) (\s@DescribedServer' {} a -> s {state = a} :: DescribedServer)

-- | Specifies the mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Amazon Web Services Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Amazon Web Services Managed Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connectors. This option also requires you to provide a
-- Directory ID using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an API
-- Gateway endpoint URL to call for authentication using the
-- @IdentityProviderDetails@ parameter.
describedServer_identityProviderType :: Lens.Lens' DescribedServer (Prelude.Maybe IdentityProviderType)
describedServer_identityProviderType = Lens.lens (\DescribedServer' {identityProviderType} -> identityProviderType) (\s@DescribedServer' {} a -> s {identityProviderType = a} :: DescribedServer)

-- | Specifies the file transfer protocol or protocols over which your file
-- transfer protocol client can connect to your server\'s endpoint. The
-- available protocols are:
--
-- -   @SFTP@ (Secure Shell (SSH) File Transfer Protocol): File transfer
--     over SSH
--
-- -   @FTPS@ (File Transfer Protocol Secure): File transfer with TLS
--     encryption
--
-- -   @FTP@ (File Transfer Protocol): Unencrypted file transfer
describedServer_protocols :: Lens.Lens' DescribedServer (Prelude.Maybe (Prelude.NonEmpty Protocol))
describedServer_protocols = Lens.lens (\DescribedServer' {protocols} -> protocols) (\s@DescribedServer' {} a -> s {protocols = a} :: DescribedServer) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the unique system-assigned identifier for a server that you
-- instantiate.
describedServer_serverId :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_serverId = Lens.lens (\DescribedServer' {serverId} -> serverId) (\s@DescribedServer' {} a -> s {serverId = a} :: DescribedServer)

-- | Specifies the domain of the storage system that is used for file
-- transfers.
describedServer_domain :: Lens.Lens' DescribedServer (Prelude.Maybe Domain)
describedServer_domain = Lens.lens (\DescribedServer' {domain} -> domain) (\s@DescribedServer' {} a -> s {domain = a} :: DescribedServer)

-- | Defines the type of endpoint that your server is connected to. If your
-- server is connected to a VPC endpoint, your server isn\'t accessible
-- over the public internet.
describedServer_endpointType :: Lens.Lens' DescribedServer (Prelude.Maybe EndpointType)
describedServer_endpointType = Lens.lens (\DescribedServer' {endpointType} -> endpointType) (\s@DescribedServer' {} a -> s {endpointType = a} :: DescribedServer)

-- | Specifies the name of the security policy that is attached to the
-- server.
describedServer_securityPolicyName :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_securityPolicyName = Lens.lens (\DescribedServer' {securityPolicyName} -> securityPolicyName) (\s@DescribedServer' {} a -> s {securityPolicyName = a} :: DescribedServer)

-- | Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
-- key. This value is equivalent to the output of the
-- @ssh-keygen -l -f my-new-server-key@ command.
describedServer_hostKeyFingerprint :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_hostKeyFingerprint = Lens.lens (\DescribedServer' {hostKeyFingerprint} -> hostKeyFingerprint) (\s@DescribedServer' {} a -> s {hostKeyFingerprint = a} :: DescribedServer)

-- | Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
describedServer_userCount :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Int)
describedServer_userCount = Lens.lens (\DescribedServer' {userCount} -> userCount) (\s@DescribedServer' {} a -> s {userCount = a} :: DescribedServer)

-- | Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
describedServer_certificate :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_certificate = Lens.lens (\DescribedServer' {certificate} -> certificate) (\s@DescribedServer' {} a -> s {certificate = a} :: DescribedServer)

-- | Specifies information to call a customer-supplied authentication API.
-- This field is not populated when the @IdentityProviderType@ of a server
-- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
describedServer_identityProviderDetails :: Lens.Lens' DescribedServer (Prelude.Maybe IdentityProviderDetails)
describedServer_identityProviderDetails = Lens.lens (\DescribedServer' {identityProviderDetails} -> identityProviderDetails) (\s@DescribedServer' {} a -> s {identityProviderDetails = a} :: DescribedServer)

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role used for executing the workflow.
describedServer_workflowDetails :: Lens.Lens' DescribedServer (Prelude.Maybe WorkflowDetails)
describedServer_workflowDetails = Lens.lens (\DescribedServer' {workflowDetails} -> workflowDetails) (\s@DescribedServer' {} a -> s {workflowDetails = a} :: DescribedServer)

-- | Specifies the key-value pairs that you can use to search for and group
-- servers that were assigned to the server that was described.
describedServer_tags :: Lens.Lens' DescribedServer (Prelude.Maybe (Prelude.NonEmpty Tag))
describedServer_tags = Lens.lens (\DescribedServer' {tags} -> tags) (\s@DescribedServer' {} a -> s {tags = a} :: DescribedServer) Prelude.. Lens.mapping Lens.coerced

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make it accessible only to resources within your VPC, or you can attach
-- Elastic IP addresses and make it accessible to clients over the
-- internet. Your VPC\'s default security groups are automatically assigned
-- to your endpoint.
describedServer_endpointDetails :: Lens.Lens' DescribedServer (Prelude.Maybe EndpointDetails)
describedServer_endpointDetails = Lens.lens (\DescribedServer' {endpointDetails} -> endpointDetails) (\s@DescribedServer' {} a -> s {endpointDetails = a} :: DescribedServer)

-- | Specifies the unique Amazon Resource Name (ARN) of the server.
describedServer_arn :: Lens.Lens' DescribedServer Prelude.Text
describedServer_arn = Lens.lens (\DescribedServer' {arn} -> arn) (\s@DescribedServer' {} a -> s {arn = a} :: DescribedServer)

instance Core.FromJSON DescribedServer where
  parseJSON =
    Core.withObject
      "DescribedServer"
      ( \x ->
          DescribedServer'
            Prelude.<$> (x Core..:? "ProtocolDetails")
            Prelude.<*> (x Core..:? "LoggingRole")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "IdentityProviderType")
            Prelude.<*> (x Core..:? "Protocols")
            Prelude.<*> (x Core..:? "ServerId")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "SecurityPolicyName")
            Prelude.<*> (x Core..:? "HostKeyFingerprint")
            Prelude.<*> (x Core..:? "UserCount")
            Prelude.<*> (x Core..:? "Certificate")
            Prelude.<*> (x Core..:? "IdentityProviderDetails")
            Prelude.<*> (x Core..:? "WorkflowDetails")
            Prelude.<*> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "EndpointDetails")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedServer

instance Prelude.NFData DescribedServer
