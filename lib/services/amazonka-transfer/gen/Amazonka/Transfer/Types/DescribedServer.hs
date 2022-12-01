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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | Specifies the key-value pairs that you can use to search for and group
    -- servers that were assigned to the server that was described.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the number of users that are assigned to a server you
    -- specified with the @ServerId@.
    userCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies a string to display when users connect to a server. This
    -- string is displayed before the user authenticates. For example, the
    -- following banner displays details about using the system:
    --
    -- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
    preAuthenticationLoginBanner :: Prelude.Maybe Prelude.Text,
    -- | The protocol settings that are configured for your server.
    --
    -- -   To indicate passive mode (for FTP and FTPS protocols), use the
    --     @PassiveIp@ parameter. Enter a single dotted-quad IPv4 address, such
    --     as the external IP address of a firewall, router, or load balancer.
    --
    -- -   To ignore the error that is generated when the client attempts to
    --     use the @SETSTAT@ command on a file that you are uploading to an
    --     Amazon S3 bucket, use the @SetStatOption@ parameter. To have the
    --     Transfer Family server ignore the @SETSTAT@ command and upload files
    --     without needing to make any changes to your SFTP client, set the
    --     value to @ENABLE_NO_OP@. If you set the @SetStatOption@ parameter to
    --     @ENABLE_NO_OP@, Transfer Family generates a log entry to Amazon
    --     CloudWatch Logs, so that you can determine when the client is making
    --     a @SETSTAT@ call.
    --
    -- -   To determine whether your Transfer Family server resumes recent,
    --     negotiated sessions through a unique session ID, use the
    --     @TlsSessionResumptionMode@ parameter.
    --
    -- -   @As2Transports@ indicates the transport method for the AS2 messages.
    --     Currently, only HTTP is supported.
    protocolDetails :: Prelude.Maybe ProtocolDetails,
    -- | Specifies information to call a customer-supplied authentication API.
    -- This field is not populated when the @IdentityProviderType@ of a server
    -- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
    identityProviderDetails :: Prelude.Maybe IdentityProviderDetails,
    -- | Specifies the domain of the storage system that is used for file
    -- transfers.
    domain :: Prelude.Maybe Domain,
    -- | The mode of authentication for a server. The default value is
    -- @SERVICE_MANAGED@, which allows you to store and access user credentials
    -- within the Transfer Family service.
    --
    -- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
    -- in Directory Service for Microsoft Active Directory or Microsoft Active
    -- Directory in your on-premises environment or in Amazon Web Services
    -- using AD Connector. This option also requires you to provide a Directory
    -- ID by using the @IdentityProviderDetails@ parameter.
    --
    -- Use the @API_GATEWAY@ value to integrate with an identity provider of
    -- your choosing. The @API_GATEWAY@ setting requires you to provide an
    -- Amazon API Gateway endpoint URL to call for authentication by using the
    -- @IdentityProviderDetails@ parameter.
    --
    -- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
    -- identity provider. If you choose this value, you must specify the ARN
    -- for the Lambda function in the @Function@ parameter or the
    -- @IdentityProviderDetails@ data type.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) endpoint settings that are configured
    -- for your server. When you host your endpoint within your VPC, you can
    -- make your endpoint accessible only to resources within your VPC, or you
    -- can attach Elastic IP addresses and make your endpoint accessible to
    -- clients over the internet. Your VPC\'s default security groups are
    -- automatically assigned to your endpoint.
    endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | The condition of the server that was described. A value of @ONLINE@
    -- indicates that the server can accept jobs and transfer files. A @State@
    -- value of @OFFLINE@ means that the server cannot perform file transfer
    -- operations.
    --
    -- The states of @STARTING@ and @STOPPING@ indicate that the server is in
    -- an intermediate state, either not fully able to respond, or not fully
    -- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
    -- error condition.
    state :: Prelude.Maybe State,
    -- | Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
    -- certificate. Required when @Protocols@ is set to @FTPS@.
    certificate :: Prelude.Maybe Prelude.Text,
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
    --
    -- -   @AS2@ (Applicability Statement 2): used for transporting structured
    --     business-to-business data
    --
    -- -   If you select @FTPS@, you must choose a certificate stored in
    --     Certificate Manager (ACM) which is used to identify your server when
    --     clients connect to it over FTPS.
    --
    -- -   If @Protocol@ includes either @FTP@ or @FTPS@, then the
    --     @EndpointType@ must be @VPC@ and the @IdentityProviderType@ must be
    --     @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
    --
    -- -   If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
    --     associated.
    --
    -- -   If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set
    --     to @PUBLIC@ and the @IdentityProviderType@ can be set to
    --     @SERVICE_MANAGED@.
    --
    -- -   If @Protocol@ includes @AS2@, then the @EndpointType@ must be @VPC@,
    --     and domain must be Amazon S3.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | Defines the type of endpoint that your server is connected to. If your
    -- server is connected to a VPC endpoint, your server isn\'t accessible
    -- over the public internet.
    endpointType :: Prelude.Maybe EndpointType,
    -- | Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
    -- key. This value is equivalent to the output of the
    -- @ssh-keygen -l -f my-new-server-key@ command.
    hostKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
    -- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
    -- your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the unique system-assigned identifier for a server that you
    -- instantiate.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | Specifies a string to display when users connect to a server. This
    -- string is displayed after the user authenticates.
    --
    -- The SFTP protocol does not support post-authentication display banners.
    postAuthenticationLoginBanner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the workflow ID for the workflow to assign and the execution
    -- role that\'s used for executing the workflow.
    --
    -- In addition to a workflow to execute when a file is uploaded completely,
    -- @WorkflowDetails@ can also contain a workflow ID (and execution role)
    -- for a workflow to execute on partial upload. A partial upload occurs
    -- when a file is open when the session disconnects.
    workflowDetails :: Prelude.Maybe WorkflowDetails,
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
-- 'tags', 'describedServer_tags' - Specifies the key-value pairs that you can use to search for and group
-- servers that were assigned to the server that was described.
--
-- 'userCount', 'describedServer_userCount' - Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
--
-- 'preAuthenticationLoginBanner', 'describedServer_preAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
--
-- 'protocolDetails', 'describedServer_protocolDetails' - The protocol settings that are configured for your server.
--
-- -   To indicate passive mode (for FTP and FTPS protocols), use the
--     @PassiveIp@ parameter. Enter a single dotted-quad IPv4 address, such
--     as the external IP address of a firewall, router, or load balancer.
--
-- -   To ignore the error that is generated when the client attempts to
--     use the @SETSTAT@ command on a file that you are uploading to an
--     Amazon S3 bucket, use the @SetStatOption@ parameter. To have the
--     Transfer Family server ignore the @SETSTAT@ command and upload files
--     without needing to make any changes to your SFTP client, set the
--     value to @ENABLE_NO_OP@. If you set the @SetStatOption@ parameter to
--     @ENABLE_NO_OP@, Transfer Family generates a log entry to Amazon
--     CloudWatch Logs, so that you can determine when the client is making
--     a @SETSTAT@ call.
--
-- -   To determine whether your Transfer Family server resumes recent,
--     negotiated sessions through a unique session ID, use the
--     @TlsSessionResumptionMode@ parameter.
--
-- -   @As2Transports@ indicates the transport method for the AS2 messages.
--     Currently, only HTTP is supported.
--
-- 'identityProviderDetails', 'describedServer_identityProviderDetails' - Specifies information to call a customer-supplied authentication API.
-- This field is not populated when the @IdentityProviderType@ of a server
-- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
--
-- 'domain', 'describedServer_domain' - Specifies the domain of the storage system that is used for file
-- transfers.
--
-- 'identityProviderType', 'describedServer_identityProviderType' - The mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Directory Service for Microsoft Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connector. This option also requires you to provide a Directory
-- ID by using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an
-- Amazon API Gateway endpoint URL to call for authentication by using the
-- @IdentityProviderDetails@ parameter.
--
-- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
-- identity provider. If you choose this value, you must specify the ARN
-- for the Lambda function in the @Function@ parameter or the
-- @IdentityProviderDetails@ data type.
--
-- 'securityPolicyName', 'describedServer_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
--
-- 'endpointDetails', 'describedServer_endpointDetails' - The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
--
-- 'state', 'describedServer_state' - The condition of the server that was described. A value of @ONLINE@
-- indicates that the server can accept jobs and transfer files. A @State@
-- value of @OFFLINE@ means that the server cannot perform file transfer
-- operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
--
-- 'certificate', 'describedServer_certificate' - Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
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
-- -   @AS2@ (Applicability Statement 2): used for transporting structured
--     business-to-business data
--
-- -   If you select @FTPS@, you must choose a certificate stored in
--     Certificate Manager (ACM) which is used to identify your server when
--     clients connect to it over FTPS.
--
-- -   If @Protocol@ includes either @FTP@ or @FTPS@, then the
--     @EndpointType@ must be @VPC@ and the @IdentityProviderType@ must be
--     @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
--
-- -   If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
--     associated.
--
-- -   If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set
--     to @PUBLIC@ and the @IdentityProviderType@ can be set to
--     @SERVICE_MANAGED@.
--
-- -   If @Protocol@ includes @AS2@, then the @EndpointType@ must be @VPC@,
--     and domain must be Amazon S3.
--
-- 'endpointType', 'describedServer_endpointType' - Defines the type of endpoint that your server is connected to. If your
-- server is connected to a VPC endpoint, your server isn\'t accessible
-- over the public internet.
--
-- 'hostKeyFingerprint', 'describedServer_hostKeyFingerprint' - Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
-- key. This value is equivalent to the output of the
-- @ssh-keygen -l -f my-new-server-key@ command.
--
-- 'loggingRole', 'describedServer_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
--
-- 'serverId', 'describedServer_serverId' - Specifies the unique system-assigned identifier for a server that you
-- instantiate.
--
-- 'postAuthenticationLoginBanner', 'describedServer_postAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
--
-- 'workflowDetails', 'describedServer_workflowDetails' - Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In addition to a workflow to execute when a file is uploaded completely,
-- @WorkflowDetails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
--
-- 'arn', 'describedServer_arn' - Specifies the unique Amazon Resource Name (ARN) of the server.
newDescribedServer ::
  -- | 'arn'
  Prelude.Text ->
  DescribedServer
newDescribedServer pArn_ =
  DescribedServer'
    { tags = Prelude.Nothing,
      userCount = Prelude.Nothing,
      preAuthenticationLoginBanner = Prelude.Nothing,
      protocolDetails = Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      domain = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      securityPolicyName = Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      state = Prelude.Nothing,
      certificate = Prelude.Nothing,
      protocols = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      hostKeyFingerprint = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      serverId = Prelude.Nothing,
      postAuthenticationLoginBanner = Prelude.Nothing,
      workflowDetails = Prelude.Nothing,
      arn = pArn_
    }

-- | Specifies the key-value pairs that you can use to search for and group
-- servers that were assigned to the server that was described.
describedServer_tags :: Lens.Lens' DescribedServer (Prelude.Maybe (Prelude.NonEmpty Tag))
describedServer_tags = Lens.lens (\DescribedServer' {tags} -> tags) (\s@DescribedServer' {} a -> s {tags = a} :: DescribedServer) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
describedServer_userCount :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Int)
describedServer_userCount = Lens.lens (\DescribedServer' {userCount} -> userCount) (\s@DescribedServer' {} a -> s {userCount = a} :: DescribedServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
describedServer_preAuthenticationLoginBanner :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_preAuthenticationLoginBanner = Lens.lens (\DescribedServer' {preAuthenticationLoginBanner} -> preAuthenticationLoginBanner) (\s@DescribedServer' {} a -> s {preAuthenticationLoginBanner = a} :: DescribedServer)

-- | The protocol settings that are configured for your server.
--
-- -   To indicate passive mode (for FTP and FTPS protocols), use the
--     @PassiveIp@ parameter. Enter a single dotted-quad IPv4 address, such
--     as the external IP address of a firewall, router, or load balancer.
--
-- -   To ignore the error that is generated when the client attempts to
--     use the @SETSTAT@ command on a file that you are uploading to an
--     Amazon S3 bucket, use the @SetStatOption@ parameter. To have the
--     Transfer Family server ignore the @SETSTAT@ command and upload files
--     without needing to make any changes to your SFTP client, set the
--     value to @ENABLE_NO_OP@. If you set the @SetStatOption@ parameter to
--     @ENABLE_NO_OP@, Transfer Family generates a log entry to Amazon
--     CloudWatch Logs, so that you can determine when the client is making
--     a @SETSTAT@ call.
--
-- -   To determine whether your Transfer Family server resumes recent,
--     negotiated sessions through a unique session ID, use the
--     @TlsSessionResumptionMode@ parameter.
--
-- -   @As2Transports@ indicates the transport method for the AS2 messages.
--     Currently, only HTTP is supported.
describedServer_protocolDetails :: Lens.Lens' DescribedServer (Prelude.Maybe ProtocolDetails)
describedServer_protocolDetails = Lens.lens (\DescribedServer' {protocolDetails} -> protocolDetails) (\s@DescribedServer' {} a -> s {protocolDetails = a} :: DescribedServer)

-- | Specifies information to call a customer-supplied authentication API.
-- This field is not populated when the @IdentityProviderType@ of a server
-- is @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@.
describedServer_identityProviderDetails :: Lens.Lens' DescribedServer (Prelude.Maybe IdentityProviderDetails)
describedServer_identityProviderDetails = Lens.lens (\DescribedServer' {identityProviderDetails} -> identityProviderDetails) (\s@DescribedServer' {} a -> s {identityProviderDetails = a} :: DescribedServer)

-- | Specifies the domain of the storage system that is used for file
-- transfers.
describedServer_domain :: Lens.Lens' DescribedServer (Prelude.Maybe Domain)
describedServer_domain = Lens.lens (\DescribedServer' {domain} -> domain) (\s@DescribedServer' {} a -> s {domain = a} :: DescribedServer)

-- | The mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Directory Service for Microsoft Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connector. This option also requires you to provide a Directory
-- ID by using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an
-- Amazon API Gateway endpoint URL to call for authentication by using the
-- @IdentityProviderDetails@ parameter.
--
-- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
-- identity provider. If you choose this value, you must specify the ARN
-- for the Lambda function in the @Function@ parameter or the
-- @IdentityProviderDetails@ data type.
describedServer_identityProviderType :: Lens.Lens' DescribedServer (Prelude.Maybe IdentityProviderType)
describedServer_identityProviderType = Lens.lens (\DescribedServer' {identityProviderType} -> identityProviderType) (\s@DescribedServer' {} a -> s {identityProviderType = a} :: DescribedServer)

-- | Specifies the name of the security policy that is attached to the
-- server.
describedServer_securityPolicyName :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_securityPolicyName = Lens.lens (\DescribedServer' {securityPolicyName} -> securityPolicyName) (\s@DescribedServer' {} a -> s {securityPolicyName = a} :: DescribedServer)

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
describedServer_endpointDetails :: Lens.Lens' DescribedServer (Prelude.Maybe EndpointDetails)
describedServer_endpointDetails = Lens.lens (\DescribedServer' {endpointDetails} -> endpointDetails) (\s@DescribedServer' {} a -> s {endpointDetails = a} :: DescribedServer)

-- | The condition of the server that was described. A value of @ONLINE@
-- indicates that the server can accept jobs and transfer files. A @State@
-- value of @OFFLINE@ means that the server cannot perform file transfer
-- operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
describedServer_state :: Lens.Lens' DescribedServer (Prelude.Maybe State)
describedServer_state = Lens.lens (\DescribedServer' {state} -> state) (\s@DescribedServer' {} a -> s {state = a} :: DescribedServer)

-- | Specifies the ARN of the Amazon Web ServicesCertificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
describedServer_certificate :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_certificate = Lens.lens (\DescribedServer' {certificate} -> certificate) (\s@DescribedServer' {} a -> s {certificate = a} :: DescribedServer)

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
--
-- -   @AS2@ (Applicability Statement 2): used for transporting structured
--     business-to-business data
--
-- -   If you select @FTPS@, you must choose a certificate stored in
--     Certificate Manager (ACM) which is used to identify your server when
--     clients connect to it over FTPS.
--
-- -   If @Protocol@ includes either @FTP@ or @FTPS@, then the
--     @EndpointType@ must be @VPC@ and the @IdentityProviderType@ must be
--     @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
--
-- -   If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
--     associated.
--
-- -   If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set
--     to @PUBLIC@ and the @IdentityProviderType@ can be set to
--     @SERVICE_MANAGED@.
--
-- -   If @Protocol@ includes @AS2@, then the @EndpointType@ must be @VPC@,
--     and domain must be Amazon S3.
describedServer_protocols :: Lens.Lens' DescribedServer (Prelude.Maybe (Prelude.NonEmpty Protocol))
describedServer_protocols = Lens.lens (\DescribedServer' {protocols} -> protocols) (\s@DescribedServer' {} a -> s {protocols = a} :: DescribedServer) Prelude.. Lens.mapping Lens.coerced

-- | Defines the type of endpoint that your server is connected to. If your
-- server is connected to a VPC endpoint, your server isn\'t accessible
-- over the public internet.
describedServer_endpointType :: Lens.Lens' DescribedServer (Prelude.Maybe EndpointType)
describedServer_endpointType = Lens.lens (\DescribedServer' {endpointType} -> endpointType) (\s@DescribedServer' {} a -> s {endpointType = a} :: DescribedServer)

-- | Specifies the Base64-encoded SHA256 fingerprint of the server\'s host
-- key. This value is equivalent to the output of the
-- @ssh-keygen -l -f my-new-server-key@ command.
describedServer_hostKeyFingerprint :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_hostKeyFingerprint = Lens.lens (\DescribedServer' {hostKeyFingerprint} -> hostKeyFingerprint) (\s@DescribedServer' {} a -> s {hostKeyFingerprint = a} :: DescribedServer)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
describedServer_loggingRole :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_loggingRole = Lens.lens (\DescribedServer' {loggingRole} -> loggingRole) (\s@DescribedServer' {} a -> s {loggingRole = a} :: DescribedServer)

-- | Specifies the unique system-assigned identifier for a server that you
-- instantiate.
describedServer_serverId :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_serverId = Lens.lens (\DescribedServer' {serverId} -> serverId) (\s@DescribedServer' {} a -> s {serverId = a} :: DescribedServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
describedServer_postAuthenticationLoginBanner :: Lens.Lens' DescribedServer (Prelude.Maybe Prelude.Text)
describedServer_postAuthenticationLoginBanner = Lens.lens (\DescribedServer' {postAuthenticationLoginBanner} -> postAuthenticationLoginBanner) (\s@DescribedServer' {} a -> s {postAuthenticationLoginBanner = a} :: DescribedServer)

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In addition to a workflow to execute when a file is uploaded completely,
-- @WorkflowDetails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
describedServer_workflowDetails :: Lens.Lens' DescribedServer (Prelude.Maybe WorkflowDetails)
describedServer_workflowDetails = Lens.lens (\DescribedServer' {workflowDetails} -> workflowDetails) (\s@DescribedServer' {} a -> s {workflowDetails = a} :: DescribedServer)

-- | Specifies the unique Amazon Resource Name (ARN) of the server.
describedServer_arn :: Lens.Lens' DescribedServer Prelude.Text
describedServer_arn = Lens.lens (\DescribedServer' {arn} -> arn) (\s@DescribedServer' {} a -> s {arn = a} :: DescribedServer)

instance Core.FromJSON DescribedServer where
  parseJSON =
    Core.withObject
      "DescribedServer"
      ( \x ->
          DescribedServer'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "UserCount")
            Prelude.<*> (x Core..:? "PreAuthenticationLoginBanner")
            Prelude.<*> (x Core..:? "ProtocolDetails")
            Prelude.<*> (x Core..:? "IdentityProviderDetails")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "IdentityProviderType")
            Prelude.<*> (x Core..:? "SecurityPolicyName")
            Prelude.<*> (x Core..:? "EndpointDetails")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Certificate")
            Prelude.<*> (x Core..:? "Protocols")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "HostKeyFingerprint")
            Prelude.<*> (x Core..:? "LoggingRole")
            Prelude.<*> (x Core..:? "ServerId")
            Prelude.<*> (x Core..:? "PostAuthenticationLoginBanner")
            Prelude.<*> (x Core..:? "WorkflowDetails")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedServer where
  hashWithSalt _salt DescribedServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userCount
      `Prelude.hashWithSalt` preAuthenticationLoginBanner
      `Prelude.hashWithSalt` protocolDetails
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` securityPolicyName
      `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` hostKeyFingerprint
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` postAuthenticationLoginBanner
      `Prelude.hashWithSalt` workflowDetails
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribedServer where
  rnf DescribedServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userCount
      `Prelude.seq` Prelude.rnf preAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf protocolDetails
      `Prelude.seq` Prelude.rnf identityProviderDetails
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProviderType
      `Prelude.seq` Prelude.rnf securityPolicyName
      `Prelude.seq` Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf hostKeyFingerprint
      `Prelude.seq` Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf
        postAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf workflowDetails
      `Prelude.seq` Prelude.rnf arn
