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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createServer_tags,
    createServer_identityProviderDetails,
    createServer_domain,
    createServer_identityProviderType,
    createServer_securityPolicyName,
    createServer_endpointDetails,
    createServer_certificate,
    createServer_protocols,
    createServer_endpointType,
    createServer_loggingRole,
    createServer_workflowDetails,
    createServer_hostKey,

    -- * Destructuring the Response
    CreateServerResponse (..),
    newCreateServerResponse,

    -- * Response Lenses
    createServerResponse_httpStatus,
    createServerResponse_serverId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateServer' smart constructor.
data CreateServer = CreateServer'
  { -- | Key-value pairs that can be used to group and search for servers.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
    -- or @API_GATEWAY@. Accepts an array containing all of the information
    -- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
    -- customer-supplied authentication API, including the API Gateway URL. Not
    -- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
    identityProviderDetails :: Prelude.Maybe IdentityProviderDetails,
    -- | The domain of the storage system that is used for file transfers. There
    -- are two domains available: Amazon Simple Storage Service (Amazon S3) and
    -- Amazon Elastic File System (Amazon EFS). The default value is S3.
    --
    -- After the server is created, the domain cannot be changed.
    domain :: Prelude.Maybe Domain,
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
    -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) endpoint settings that are configured
    -- for your server. When you host your endpoint within your VPC, you can
    -- make it accessible only to resources within your VPC, or you can attach
    -- Elastic IP addresses and make it accessible to clients over the
    -- internet. Your VPC\'s default security groups are automatically assigned
    -- to your endpoint.
    endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
    -- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
    --
    -- To request a new public certificate, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
    --
    -- To import an existing certificate into ACM, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
    --
    -- To request a private certificate to use FTPS through private IP
    -- addresses, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
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
    -- If you select @FTPS@, you must choose a certificate stored in Amazon Web
    -- Services Certificate Manager (ACM) which is used to identify your server
    -- when clients connect to it over FTPS.
    --
    -- If @Protocol@ includes either @FTP@ or @FTPS@, then the @EndpointType@
    -- must be @VPC@ and the @IdentityProviderType@ must be
    -- @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
    --
    -- If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
    -- associated.
    --
    -- If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set to
    -- @PUBLIC@ and the @IdentityProviderType@ can be set to @SERVICE_MANAGED@.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
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
    -- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
    -- Identity and Access Management (IAM) role that allows a server to turn
    -- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
    -- set, user activity can be viewed in your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the workflow ID for the workflow to assign and the execution
    -- role used for executing the workflow.
    workflowDetails :: Prelude.Maybe WorkflowDetails,
    -- | The RSA private key as generated by the
    -- @ssh-keygen -N \"\" -m PEM -f my-new-server-key@ command.
    --
    -- If you aren\'t planning to migrate existing users from an existing
    -- SFTP-enabled server to a new server, don\'t update the host key.
    -- Accidentally changing a server\'s host key can be disruptive.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Change the host key for your SFTP-enabled server>
    -- in the /Amazon Web Services Transfer Family User Guide/.
    hostKey :: Prelude.Maybe (Core.Sensitive Prelude.Text)
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
-- 'tags', 'createServer_tags' - Key-value pairs that can be used to group and search for servers.
--
-- 'identityProviderDetails', 'createServer_identityProviderDetails' - Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
-- or @API_GATEWAY@. Accepts an array containing all of the information
-- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
-- customer-supplied authentication API, including the API Gateway URL. Not
-- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
--
-- 'domain', 'createServer_domain' - The domain of the storage system that is used for file transfers. There
-- are two domains available: Amazon Simple Storage Service (Amazon S3) and
-- Amazon Elastic File System (Amazon EFS). The default value is S3.
--
-- After the server is created, the domain cannot be changed.
--
-- 'identityProviderType', 'createServer_identityProviderType' - Specifies the mode of authentication for a server. The default value is
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
-- 'securityPolicyName', 'createServer_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
--
-- 'endpointDetails', 'createServer_endpointDetails' - The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make it accessible only to resources within your VPC, or you can attach
-- Elastic IP addresses and make it accessible to clients over the
-- internet. Your VPC\'s default security groups are automatically assigned
-- to your endpoint.
--
-- 'certificate', 'createServer_certificate' - The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
-- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Amazon Web Services Certificate Manager User Guide/.
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
-- If you select @FTPS@, you must choose a certificate stored in Amazon Web
-- Services Certificate Manager (ACM) which is used to identify your server
-- when clients connect to it over FTPS.
--
-- If @Protocol@ includes either @FTP@ or @FTPS@, then the @EndpointType@
-- must be @VPC@ and the @IdentityProviderType@ must be
-- @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
--
-- If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
-- associated.
--
-- If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set to
-- @PUBLIC@ and the @IdentityProviderType@ can be set to @SERVICE_MANAGED@.
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
-- 'loggingRole', 'createServer_loggingRole' - Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
--
-- 'workflowDetails', 'createServer_workflowDetails' - Specifies the workflow ID for the workflow to assign and the execution
-- role used for executing the workflow.
--
-- 'hostKey', 'createServer_hostKey' - The RSA private key as generated by the
-- @ssh-keygen -N \"\" -m PEM -f my-new-server-key@ command.
--
-- If you aren\'t planning to migrate existing users from an existing
-- SFTP-enabled server to a new server, don\'t update the host key.
-- Accidentally changing a server\'s host key can be disruptive.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Change the host key for your SFTP-enabled server>
-- in the /Amazon Web Services Transfer Family User Guide/.
newCreateServer ::
  CreateServer
newCreateServer =
  CreateServer'
    { tags = Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      domain = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      securityPolicyName = Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      certificate = Prelude.Nothing,
      protocols = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      workflowDetails = Prelude.Nothing,
      hostKey = Prelude.Nothing
    }

-- | Key-value pairs that can be used to group and search for servers.
createServer_tags :: Lens.Lens' CreateServer (Prelude.Maybe (Prelude.NonEmpty Tag))
createServer_tags = Lens.lens (\CreateServer' {tags} -> tags) (\s@CreateServer' {} a -> s {tags = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | Required when @IdentityProviderType@ is set to @AWS_DIRECTORY_SERVICE@
-- or @API_GATEWAY@. Accepts an array containing all of the information
-- required to use a directory in @AWS_DIRECTORY_SERVICE@ or invoke a
-- customer-supplied authentication API, including the API Gateway URL. Not
-- required when @IdentityProviderType@ is set to @SERVICE_MANAGED@.
createServer_identityProviderDetails :: Lens.Lens' CreateServer (Prelude.Maybe IdentityProviderDetails)
createServer_identityProviderDetails = Lens.lens (\CreateServer' {identityProviderDetails} -> identityProviderDetails) (\s@CreateServer' {} a -> s {identityProviderDetails = a} :: CreateServer)

-- | The domain of the storage system that is used for file transfers. There
-- are two domains available: Amazon Simple Storage Service (Amazon S3) and
-- Amazon Elastic File System (Amazon EFS). The default value is S3.
--
-- After the server is created, the domain cannot be changed.
createServer_domain :: Lens.Lens' CreateServer (Prelude.Maybe Domain)
createServer_domain = Lens.lens (\CreateServer' {domain} -> domain) (\s@CreateServer' {} a -> s {domain = a} :: CreateServer)

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
createServer_identityProviderType :: Lens.Lens' CreateServer (Prelude.Maybe IdentityProviderType)
createServer_identityProviderType = Lens.lens (\CreateServer' {identityProviderType} -> identityProviderType) (\s@CreateServer' {} a -> s {identityProviderType = a} :: CreateServer)

-- | Specifies the name of the security policy that is attached to the
-- server.
createServer_securityPolicyName :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_securityPolicyName = Lens.lens (\CreateServer' {securityPolicyName} -> securityPolicyName) (\s@CreateServer' {} a -> s {securityPolicyName = a} :: CreateServer)

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make it accessible only to resources within your VPC, or you can attach
-- Elastic IP addresses and make it accessible to clients over the
-- internet. Your VPC\'s default security groups are automatically assigned
-- to your endpoint.
createServer_endpointDetails :: Lens.Lens' CreateServer (Prelude.Maybe EndpointDetails)
createServer_endpointDetails = Lens.lens (\CreateServer' {endpointDetails} -> endpointDetails) (\s@CreateServer' {} a -> s {endpointDetails = a} :: CreateServer)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
-- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Amazon Web Services Certificate Manager User Guide/.
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
-- If you select @FTPS@, you must choose a certificate stored in Amazon Web
-- Services Certificate Manager (ACM) which is used to identify your server
-- when clients connect to it over FTPS.
--
-- If @Protocol@ includes either @FTP@ or @FTPS@, then the @EndpointType@
-- must be @VPC@ and the @IdentityProviderType@ must be
-- @AWS_DIRECTORY_SERVICE@ or @API_GATEWAY@.
--
-- If @Protocol@ includes @FTP@, then @AddressAllocationIds@ cannot be
-- associated.
--
-- If @Protocol@ is set only to @SFTP@, the @EndpointType@ can be set to
-- @PUBLIC@ and the @IdentityProviderType@ can be set to @SERVICE_MANAGED@.
createServer_protocols :: Lens.Lens' CreateServer (Prelude.Maybe (Prelude.NonEmpty Protocol))
createServer_protocols = Lens.lens (\CreateServer' {protocols} -> protocols) (\s@CreateServer' {} a -> s {protocols = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

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

-- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
createServer_loggingRole :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_loggingRole = Lens.lens (\CreateServer' {loggingRole} -> loggingRole) (\s@CreateServer' {} a -> s {loggingRole = a} :: CreateServer)

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role used for executing the workflow.
createServer_workflowDetails :: Lens.Lens' CreateServer (Prelude.Maybe WorkflowDetails)
createServer_workflowDetails = Lens.lens (\CreateServer' {workflowDetails} -> workflowDetails) (\s@CreateServer' {} a -> s {workflowDetails = a} :: CreateServer)

-- | The RSA private key as generated by the
-- @ssh-keygen -N \"\" -m PEM -f my-new-server-key@ command.
--
-- If you aren\'t planning to migrate existing users from an existing
-- SFTP-enabled server to a new server, don\'t update the host key.
-- Accidentally changing a server\'s host key can be disruptive.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/edit-server-config.html#configuring-servers-change-host-key Change the host key for your SFTP-enabled server>
-- in the /Amazon Web Services Transfer Family User Guide/.
createServer_hostKey :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_hostKey = Lens.lens (\CreateServer' {hostKey} -> hostKey) (\s@CreateServer' {} a -> s {hostKey = a} :: CreateServer) Prelude.. Lens.mapping Core._Sensitive

instance Core.AWSRequest CreateServer where
  type AWSResponse CreateServer = CreateServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ServerId")
      )

instance Prelude.Hashable CreateServer where
  hashWithSalt _salt CreateServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` securityPolicyName
      `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` workflowDetails
      `Prelude.hashWithSalt` hostKey

instance Prelude.NFData CreateServer where
  rnf CreateServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf identityProviderDetails
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProviderType
      `Prelude.seq` Prelude.rnf securityPolicyName
      `Prelude.seq` Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf workflowDetails
      `Prelude.seq` Prelude.rnf hostKey

instance Core.ToHeaders CreateServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.CreateServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateServer where
  toJSON CreateServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("IdentityProviderDetails" Core..=)
              Prelude.<$> identityProviderDetails,
            ("Domain" Core..=) Prelude.<$> domain,
            ("IdentityProviderType" Core..=)
              Prelude.<$> identityProviderType,
            ("SecurityPolicyName" Core..=)
              Prelude.<$> securityPolicyName,
            ("EndpointDetails" Core..=)
              Prelude.<$> endpointDetails,
            ("Certificate" Core..=) Prelude.<$> certificate,
            ("Protocols" Core..=) Prelude.<$> protocols,
            ("EndpointType" Core..=) Prelude.<$> endpointType,
            ("LoggingRole" Core..=) Prelude.<$> loggingRole,
            ("WorkflowDetails" Core..=)
              Prelude.<$> workflowDetails,
            ("HostKey" Core..=) Prelude.<$> hostKey
          ]
      )

instance Core.ToPath CreateServer where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service-assigned ID of the server that is created.
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
-- 'serverId', 'createServerResponse_serverId' - The service-assigned ID of the server that is created.
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

-- | The service-assigned ID of the server that is created.
createServerResponse_serverId :: Lens.Lens' CreateServerResponse Prelude.Text
createServerResponse_serverId = Lens.lens (\CreateServerResponse' {serverId} -> serverId) (\s@CreateServerResponse' {} a -> s {serverId = a} :: CreateServerResponse)

instance Prelude.NFData CreateServerResponse where
  rnf CreateServerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
