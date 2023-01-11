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
-- Module      : Amazonka.Transfer.CreateServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instantiates an auto-scaling virtual server based on the selected file
-- transfer protocol in Amazon Web Services. When you make updates to your
-- file transfer protocol-enabled server or when you work with users, use
-- the service-generated @ServerId@ property that is assigned to the newly
-- created server.
module Amazonka.Transfer.CreateServer
  ( -- * Creating a Request
    CreateServer (..),
    newCreateServer,

    -- * Request Lenses
    createServer_certificate,
    createServer_domain,
    createServer_endpointDetails,
    createServer_endpointType,
    createServer_hostKey,
    createServer_identityProviderDetails,
    createServer_identityProviderType,
    createServer_loggingRole,
    createServer_postAuthenticationLoginBanner,
    createServer_preAuthenticationLoginBanner,
    createServer_protocolDetails,
    createServer_protocols,
    createServer_securityPolicyName,
    createServer_tags,
    createServer_workflowDetails,

    -- * Destructuring the Response
    CreateServerResponse (..),
    newCreateServerResponse,

    -- * Response Lenses
    createServerResponse_httpStatus,
    createServerResponse_serverId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateServer' smart constructor.
data CreateServer = CreateServer'
  { -- | The Amazon Resource Name (ARN) of the Certificate Manager (ACM)
    -- certificate. Required when @Protocols@ is set to @FTPS@.
    --
    -- To request a new public certificate, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
    -- in the /Certificate Manager User Guide/.
    --
    -- To import an existing certificate into ACM, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
    -- in the /Certificate Manager User Guide/.
    --
    -- To request a private certificate to use FTPS through private IP
    -- addresses, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
    -- in the /Certificate Manager User Guide/.
    --
    -- Certificates with the following cryptographic algorithms and key sizes
    -- are supported:
    --
    -- -   2048-bit RSA (RSA_2048)
    --
    -- -   4096-bit RSA (RSA_4096)
    --
    -- -   Elliptic Prime Curve 256 bit (EC_prime256v1)
    --
    -- -   Elliptic Prime Curve 384 bit (EC_secp384r1)
    --
    -- -   Elliptic Prime Curve 521 bit (EC_secp521r1)
    --
    -- The certificate must be a valid SSL\/TLS X.509 version 3 certificate
    -- with FQDN or IP address specified and information about the issuer.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The domain of the storage system that is used for file transfers. There
    -- are two domains available: Amazon Simple Storage Service (Amazon S3) and
    -- Amazon Elastic File System (Amazon EFS). The default value is S3.
    --
    -- After the server is created, the domain cannot be changed.
    domain :: Prelude.Maybe Domain,
    -- | The virtual private cloud (VPC) endpoint settings that are configured
    -- for your server. When you host your endpoint within your VPC, you can
    -- make your endpoint accessible only to resources within your VPC, or you
    -- can attach Elastic IP addresses and make your endpoint accessible to
    -- clients over the internet. Your VPC\'s default security groups are
    -- automatically assigned to your endpoint.
    endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | The type of endpoint that you want your server to use. You can choose to
    -- make your server\'s endpoint publicly accessible (PUBLIC) or host it
    -- inside your VPC. With an endpoint that is hosted in a VPC, you can
    -- restrict access to your server and resources only within your VPC or
    -- choose to make it internet facing by attaching Elastic IP addresses
    -- directly to it.
    --
    -- After May 19, 2021, you won\'t be able to create a server using
    -- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Services account if your
    -- account hasn\'t already done so before May 19, 2021. If you have already
    -- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
    -- Services account on or before May 19, 2021, you will not be affected.
    -- After this date, use @EndpointType@=@VPC@.
    --
    -- For more information, see
    -- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
    --
    -- It is recommended that you use @VPC@ as the @EndpointType@. With this
    -- endpoint type, you have the option to directly associate up to three
    -- Elastic IPv4 addresses (BYO IP included) with your server\'s endpoint
    -- and use VPC security groups to restrict traffic by the client\'s public
    -- IP address. This is not possible with @EndpointType@ set to
    -- @VPC_ENDPOINT@.
    endpointType :: Prelude.Maybe EndpointType,
    -- | The RSA, ECDSA, or ED25519 private key to use for your SFTP-enabled
    -- server. You can add multiple host keys, in case you want to rotate keys,
    -- or have a set of active keys that use different algorithms.
    --
    -- Use the following command to generate an RSA 2048 bit key with no
    -- passphrase:
    --
    -- @ssh-keygen -t rsa -b 2048 -N \"\" -m PEM -f my-new-server-key@.
    --
    -- Use a minimum value of 2048 for the @-b@ option. You can create a
    -- stronger key by using 3072 or 4096.
    --
    -- Use the following command to generate an ECDSA 256 bit key with no
    -- passphrase:
    --
    -- @ssh-keygen -t ecdsa -b 256 -N \"\" -m PEM -f my-new-server-key@.
    --
    -- Valid values for the @-b@ option for ECDSA are 256, 384, and 521.
    --
    -- Use the following command to generate an ED25519 key with no passphrase:
    --
    -- @ssh-keygen -t ed25519 -N \"\" -f my-new-server-key@.
    --
    -- For all of these commands, you can replace /my-new-server-key/ with a
    -- string of your choice.
    --
    -- If you aren\'t planning to migrate existing users from an existing
    -- SFTP-enabled server to a new server, don\'t update the host key.
    -- Accidentally changing a server\'s host key can be disruptive.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Update host keys for your SFTP-enabled server>
    -- in the /Transfer Family User Guide/.
    hostKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
    -- or @API_GATEWAY@. Accepts an array containing all of the information
    -- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
    -- customer-supplied authentication API, including the API Gateway URL. Not
    -- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
    identityProviderDetails :: Prelude.Maybe IdentityProviderDetails,
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
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
    -- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
    -- your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies a string to display when users connect to a server. This
    -- string is displayed after the user authenticates.
    --
    -- The SFTP protocol does not support post-authentication display banners.
    postAuthenticationLoginBanner :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs that can be used to group and search for servers.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the workflow ID for the workflow to assign and the execution
    -- role that\'s used for executing the workflow.
    --
    -- In additon to a workflow to execute when a file is uploaded completely,
    -- @WorkflowDeatails@ can also contain a workflow ID (and execution role)
    -- for a workflow to execute on partial upload. A partial upload occurs
    -- when a file is open when the session disconnects.
    workflowDetails :: Prelude.Maybe WorkflowDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'createServer_certificate' - The Amazon Resource Name (ARN) of the Certificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Certificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Certificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Certificate Manager User Guide/.
--
-- Certificates with the following cryptographic algorithms and key sizes
-- are supported:
--
-- -   2048-bit RSA (RSA_2048)
--
-- -   4096-bit RSA (RSA_4096)
--
-- -   Elliptic Prime Curve 256 bit (EC_prime256v1)
--
-- -   Elliptic Prime Curve 384 bit (EC_secp384r1)
--
-- -   Elliptic Prime Curve 521 bit (EC_secp521r1)
--
-- The certificate must be a valid SSL\/TLS X.509 version 3 certificate
-- with FQDN or IP address specified and information about the issuer.
--
-- 'domain', 'createServer_domain' - The domain of the storage system that is used for file transfers. There
-- are two domains available: Amazon Simple Storage Service (Amazon S3) and
-- Amazon Elastic File System (Amazon EFS). The default value is S3.
--
-- After the server is created, the domain cannot be changed.
--
-- 'endpointDetails', 'createServer_endpointDetails' - The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
--
-- 'endpointType', 'createServer_endpointType' - The type of endpoint that you want your server to use. You can choose to
-- make your server\'s endpoint publicly accessible (PUBLIC) or host it
-- inside your VPC. With an endpoint that is hosted in a VPC, you can
-- restrict access to your server and resources only within your VPC or
-- choose to make it internet facing by attaching Elastic IP addresses
-- directly to it.
--
-- After May 19, 2021, you won\'t be able to create a server using
-- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Services account if your
-- account hasn\'t already done so before May 19, 2021. If you have already
-- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
-- Services account on or before May 19, 2021, you will not be affected.
-- After this date, use @EndpointType@=@VPC@.
--
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
--
-- It is recommended that you use @VPC@ as the @EndpointType@. With this
-- endpoint type, you have the option to directly associate up to three
-- Elastic IPv4 addresses (BYO IP included) with your server\'s endpoint
-- and use VPC security groups to restrict traffic by the client\'s public
-- IP address. This is not possible with @EndpointType@ set to
-- @VPC_ENDPOINT@.
--
-- 'hostKey', 'createServer_hostKey' - The RSA, ECDSA, or ED25519 private key to use for your SFTP-enabled
-- server. You can add multiple host keys, in case you want to rotate keys,
-- or have a set of active keys that use different algorithms.
--
-- Use the following command to generate an RSA 2048 bit key with no
-- passphrase:
--
-- @ssh-keygen -t rsa -b 2048 -N \"\" -m PEM -f my-new-server-key@.
--
-- Use a minimum value of 2048 for the @-b@ option. You can create a
-- stronger key by using 3072 or 4096.
--
-- Use the following command to generate an ECDSA 256 bit key with no
-- passphrase:
--
-- @ssh-keygen -t ecdsa -b 256 -N \"\" -m PEM -f my-new-server-key@.
--
-- Valid values for the @-b@ option for ECDSA are 256, 384, and 521.
--
-- Use the following command to generate an ED25519 key with no passphrase:
--
-- @ssh-keygen -t ed25519 -N \"\" -f my-new-server-key@.
--
-- For all of these commands, you can replace /my-new-server-key/ with a
-- string of your choice.
--
-- If you aren\'t planning to migrate existing users from an existing
-- SFTP-enabled server to a new server, don\'t update the host key.
-- Accidentally changing a server\'s host key can be disruptive.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Update host keys for your SFTP-enabled server>
-- in the /Transfer Family User Guide/.
--
-- 'identityProviderDetails', 'createServer_identityProviderDetails' - Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
-- or @API_GATEWAY@. Accepts an array containing all of the information
-- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
-- customer-supplied authentication API, including the API Gateway URL. Not
-- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
--
-- 'identityProviderType', 'createServer_identityProviderType' - The mode of authentication for a server. The default value is
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
-- 'loggingRole', 'createServer_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
--
-- 'postAuthenticationLoginBanner', 'createServer_postAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
--
-- 'preAuthenticationLoginBanner', 'createServer_preAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
--
-- 'protocolDetails', 'createServer_protocolDetails' - The protocol settings that are configured for your server.
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
-- 'protocols', 'createServer_protocols' - Specifies the file transfer protocol or protocols over which your file
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
-- 'securityPolicyName', 'createServer_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
--
-- 'tags', 'createServer_tags' - Key-value pairs that can be used to group and search for servers.
--
-- 'workflowDetails', 'createServer_workflowDetails' - Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In additon to a workflow to execute when a file is uploaded completely,
-- @WorkflowDeatails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
newCreateServer ::
  CreateServer
newCreateServer =
  CreateServer'
    { certificate = Prelude.Nothing,
      domain = Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      hostKey = Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      postAuthenticationLoginBanner = Prelude.Nothing,
      preAuthenticationLoginBanner = Prelude.Nothing,
      protocolDetails = Prelude.Nothing,
      protocols = Prelude.Nothing,
      securityPolicyName = Prelude.Nothing,
      tags = Prelude.Nothing,
      workflowDetails = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Certificate Manager (ACM)
-- certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Certificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Certificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Certificate Manager User Guide/.
--
-- Certificates with the following cryptographic algorithms and key sizes
-- are supported:
--
-- -   2048-bit RSA (RSA_2048)
--
-- -   4096-bit RSA (RSA_4096)
--
-- -   Elliptic Prime Curve 256 bit (EC_prime256v1)
--
-- -   Elliptic Prime Curve 384 bit (EC_secp384r1)
--
-- -   Elliptic Prime Curve 521 bit (EC_secp521r1)
--
-- The certificate must be a valid SSL\/TLS X.509 version 3 certificate
-- with FQDN or IP address specified and information about the issuer.
createServer_certificate :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_certificate = Lens.lens (\CreateServer' {certificate} -> certificate) (\s@CreateServer' {} a -> s {certificate = a} :: CreateServer)

-- | The domain of the storage system that is used for file transfers. There
-- are two domains available: Amazon Simple Storage Service (Amazon S3) and
-- Amazon Elastic File System (Amazon EFS). The default value is S3.
--
-- After the server is created, the domain cannot be changed.
createServer_domain :: Lens.Lens' CreateServer (Prelude.Maybe Domain)
createServer_domain = Lens.lens (\CreateServer' {domain} -> domain) (\s@CreateServer' {} a -> s {domain = a} :: CreateServer)

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
createServer_endpointDetails :: Lens.Lens' CreateServer (Prelude.Maybe EndpointDetails)
createServer_endpointDetails = Lens.lens (\CreateServer' {endpointDetails} -> endpointDetails) (\s@CreateServer' {} a -> s {endpointDetails = a} :: CreateServer)

-- | The type of endpoint that you want your server to use. You can choose to
-- make your server\'s endpoint publicly accessible (PUBLIC) or host it
-- inside your VPC. With an endpoint that is hosted in a VPC, you can
-- restrict access to your server and resources only within your VPC or
-- choose to make it internet facing by attaching Elastic IP addresses
-- directly to it.
--
-- After May 19, 2021, you won\'t be able to create a server using
-- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Services account if your
-- account hasn\'t already done so before May 19, 2021. If you have already
-- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
-- Services account on or before May 19, 2021, you will not be affected.
-- After this date, use @EndpointType@=@VPC@.
--
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
--
-- It is recommended that you use @VPC@ as the @EndpointType@. With this
-- endpoint type, you have the option to directly associate up to three
-- Elastic IPv4 addresses (BYO IP included) with your server\'s endpoint
-- and use VPC security groups to restrict traffic by the client\'s public
-- IP address. This is not possible with @EndpointType@ set to
-- @VPC_ENDPOINT@.
createServer_endpointType :: Lens.Lens' CreateServer (Prelude.Maybe EndpointType)
createServer_endpointType = Lens.lens (\CreateServer' {endpointType} -> endpointType) (\s@CreateServer' {} a -> s {endpointType = a} :: CreateServer)

-- | The RSA, ECDSA, or ED25519 private key to use for your SFTP-enabled
-- server. You can add multiple host keys, in case you want to rotate keys,
-- or have a set of active keys that use different algorithms.
--
-- Use the following command to generate an RSA 2048 bit key with no
-- passphrase:
--
-- @ssh-keygen -t rsa -b 2048 -N \"\" -m PEM -f my-new-server-key@.
--
-- Use a minimum value of 2048 for the @-b@ option. You can create a
-- stronger key by using 3072 or 4096.
--
-- Use the following command to generate an ECDSA 256 bit key with no
-- passphrase:
--
-- @ssh-keygen -t ecdsa -b 256 -N \"\" -m PEM -f my-new-server-key@.
--
-- Valid values for the @-b@ option for ECDSA are 256, 384, and 521.
--
-- Use the following command to generate an ED25519 key with no passphrase:
--
-- @ssh-keygen -t ed25519 -N \"\" -f my-new-server-key@.
--
-- For all of these commands, you can replace /my-new-server-key/ with a
-- string of your choice.
--
-- If you aren\'t planning to migrate existing users from an existing
-- SFTP-enabled server to a new server, don\'t update the host key.
-- Accidentally changing a server\'s host key can be disruptive.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Update host keys for your SFTP-enabled server>
-- in the /Transfer Family User Guide/.
createServer_hostKey :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_hostKey = Lens.lens (\CreateServer' {hostKey} -> hostKey) (\s@CreateServer' {} a -> s {hostKey = a} :: CreateServer) Prelude.. Lens.mapping Data._Sensitive

-- | Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
-- or @API_GATEWAY@. Accepts an array containing all of the information
-- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
-- customer-supplied authentication API, including the API Gateway URL. Not
-- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
createServer_identityProviderDetails :: Lens.Lens' CreateServer (Prelude.Maybe IdentityProviderDetails)
createServer_identityProviderDetails = Lens.lens (\CreateServer' {identityProviderDetails} -> identityProviderDetails) (\s@CreateServer' {} a -> s {identityProviderDetails = a} :: CreateServer)

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
createServer_identityProviderType :: Lens.Lens' CreateServer (Prelude.Maybe IdentityProviderType)
createServer_identityProviderType = Lens.lens (\CreateServer' {identityProviderType} -> identityProviderType) (\s@CreateServer' {} a -> s {identityProviderType = a} :: CreateServer)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
createServer_loggingRole :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_loggingRole = Lens.lens (\CreateServer' {loggingRole} -> loggingRole) (\s@CreateServer' {} a -> s {loggingRole = a} :: CreateServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
createServer_postAuthenticationLoginBanner :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_postAuthenticationLoginBanner = Lens.lens (\CreateServer' {postAuthenticationLoginBanner} -> postAuthenticationLoginBanner) (\s@CreateServer' {} a -> s {postAuthenticationLoginBanner = a} :: CreateServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
createServer_preAuthenticationLoginBanner :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_preAuthenticationLoginBanner = Lens.lens (\CreateServer' {preAuthenticationLoginBanner} -> preAuthenticationLoginBanner) (\s@CreateServer' {} a -> s {preAuthenticationLoginBanner = a} :: CreateServer)

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
createServer_protocolDetails :: Lens.Lens' CreateServer (Prelude.Maybe ProtocolDetails)
createServer_protocolDetails = Lens.lens (\CreateServer' {protocolDetails} -> protocolDetails) (\s@CreateServer' {} a -> s {protocolDetails = a} :: CreateServer)

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
createServer_protocols :: Lens.Lens' CreateServer (Prelude.Maybe (Prelude.NonEmpty Protocol))
createServer_protocols = Lens.lens (\CreateServer' {protocols} -> protocols) (\s@CreateServer' {} a -> s {protocols = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the security policy that is attached to the
-- server.
createServer_securityPolicyName :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_securityPolicyName = Lens.lens (\CreateServer' {securityPolicyName} -> securityPolicyName) (\s@CreateServer' {} a -> s {securityPolicyName = a} :: CreateServer)

-- | Key-value pairs that can be used to group and search for servers.
createServer_tags :: Lens.Lens' CreateServer (Prelude.Maybe (Prelude.NonEmpty Tag))
createServer_tags = Lens.lens (\CreateServer' {tags} -> tags) (\s@CreateServer' {} a -> s {tags = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In additon to a workflow to execute when a file is uploaded completely,
-- @WorkflowDeatails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
createServer_workflowDetails :: Lens.Lens' CreateServer (Prelude.Maybe WorkflowDetails)
createServer_workflowDetails = Lens.lens (\CreateServer' {workflowDetails} -> workflowDetails) (\s@CreateServer' {} a -> s {workflowDetails = a} :: CreateServer)

instance Core.AWSRequest CreateServer where
  type AWSResponse CreateServer = CreateServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
      )

instance Prelude.Hashable CreateServer where
  hashWithSalt _salt CreateServer' {..} =
    _salt `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` hostKey
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` postAuthenticationLoginBanner
      `Prelude.hashWithSalt` preAuthenticationLoginBanner
      `Prelude.hashWithSalt` protocolDetails
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` securityPolicyName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workflowDetails

instance Prelude.NFData CreateServer where
  rnf CreateServer' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf hostKey
      `Prelude.seq` Prelude.rnf identityProviderDetails
      `Prelude.seq` Prelude.rnf identityProviderType
      `Prelude.seq` Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf postAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf preAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf protocolDetails
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf securityPolicyName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workflowDetails

instance Data.ToHeaders CreateServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.CreateServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServer where
  toJSON CreateServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Certificate" Data..=) Prelude.<$> certificate,
            ("Domain" Data..=) Prelude.<$> domain,
            ("EndpointDetails" Data..=)
              Prelude.<$> endpointDetails,
            ("EndpointType" Data..=) Prelude.<$> endpointType,
            ("HostKey" Data..=) Prelude.<$> hostKey,
            ("IdentityProviderDetails" Data..=)
              Prelude.<$> identityProviderDetails,
            ("IdentityProviderType" Data..=)
              Prelude.<$> identityProviderType,
            ("LoggingRole" Data..=) Prelude.<$> loggingRole,
            ("PostAuthenticationLoginBanner" Data..=)
              Prelude.<$> postAuthenticationLoginBanner,
            ("PreAuthenticationLoginBanner" Data..=)
              Prelude.<$> preAuthenticationLoginBanner,
            ("ProtocolDetails" Data..=)
              Prelude.<$> protocolDetails,
            ("Protocols" Data..=) Prelude.<$> protocols,
            ("SecurityPolicyName" Data..=)
              Prelude.<$> securityPolicyName,
            ("Tags" Data..=) Prelude.<$> tags,
            ("WorkflowDetails" Data..=)
              Prelude.<$> workflowDetails
          ]
      )

instance Data.ToPath CreateServer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service-assigned identifier of the server that is created.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createServerResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'createServerResponse_serverId' - The service-assigned identifier of the server that is created.
newCreateServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  CreateServerResponse
newCreateServerResponse pHttpStatus_ pServerId_ =
  CreateServerResponse'
    { httpStatus = pHttpStatus_,
      serverId = pServerId_
    }

-- | The response's http status code.
createServerResponse_httpStatus :: Lens.Lens' CreateServerResponse Prelude.Int
createServerResponse_httpStatus = Lens.lens (\CreateServerResponse' {httpStatus} -> httpStatus) (\s@CreateServerResponse' {} a -> s {httpStatus = a} :: CreateServerResponse)

-- | The service-assigned identifier of the server that is created.
createServerResponse_serverId :: Lens.Lens' CreateServerResponse Prelude.Text
createServerResponse_serverId = Lens.lens (\CreateServerResponse' {serverId} -> serverId) (\s@CreateServerResponse' {} a -> s {serverId = a} :: CreateServerResponse)

instance Prelude.NFData CreateServerResponse where
  rnf CreateServerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
