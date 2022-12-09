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
-- Module      : Amazonka.Transfer.UpdateServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file transfer protocol-enabled server\'s properties after
-- that server has been created.
--
-- The @UpdateServer@ call returns the @ServerId@ of the server you
-- updated.
module Amazonka.Transfer.UpdateServer
  ( -- * Creating a Request
    UpdateServer (..),
    newUpdateServer,

    -- * Request Lenses
    updateServer_certificate,
    updateServer_endpointDetails,
    updateServer_endpointType,
    updateServer_hostKey,
    updateServer_identityProviderDetails,
    updateServer_loggingRole,
    updateServer_postAuthenticationLoginBanner,
    updateServer_preAuthenticationLoginBanner,
    updateServer_protocolDetails,
    updateServer_protocols,
    updateServer_securityPolicyName,
    updateServer_workflowDetails,
    updateServer_serverId,

    -- * Destructuring the Response
    UpdateServerResponse (..),
    newUpdateServerResponse,

    -- * Response Lenses
    updateServerResponse_httpStatus,
    updateServerResponse_serverId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { -- | The Amazon Resource Name (ARN) of the Amazon Web ServicesCertificate
    -- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
    --
    -- To request a new public certificate, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
    -- in the /Amazon Web ServicesCertificate Manager User Guide/.
    --
    -- To import an existing certificate into ACM, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
    -- in the /Amazon Web ServicesCertificate Manager User Guide/.
    --
    -- To request a private certificate to use FTPS through private IP
    -- addresses, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
    -- in the /Amazon Web ServicesCertificate Manager User Guide/.
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
    -- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Servicesaccount if your
    -- account hasn\'t already done so before May 19, 2021. If you have already
    -- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
    -- Servicesaccount on or before May 19, 2021, you will not be affected.
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
    -- | An array containing all of the information required to call a
    -- customer\'s authentication API method.
    identityProviderDetails :: Prelude.Maybe IdentityProviderDetails,
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
    -- | Specifies the workflow ID for the workflow to assign and the execution
    -- role that\'s used for executing the workflow.
    --
    -- In addition to a workflow to execute when a file is uploaded completely,
    -- @WorkflowDetails@ can also contain a workflow ID (and execution role)
    -- for a workflow to execute on partial upload. A partial upload occurs
    -- when a file is open when the session disconnects.
    --
    -- To remove an associated workflow from a server, you can provide an empty
    -- @OnUpload@ object, as in the following example.
    --
    -- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
    workflowDetails :: Prelude.Maybe WorkflowDetails,
    -- | A system-assigned unique identifier for a server instance that the user
    -- account is assigned to.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'updateServer_certificate' - The Amazon Resource Name (ARN) of the Amazon Web ServicesCertificate
-- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
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
-- 'endpointDetails', 'updateServer_endpointDetails' - The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
--
-- 'endpointType', 'updateServer_endpointType' - The type of endpoint that you want your server to use. You can choose to
-- make your server\'s endpoint publicly accessible (PUBLIC) or host it
-- inside your VPC. With an endpoint that is hosted in a VPC, you can
-- restrict access to your server and resources only within your VPC or
-- choose to make it internet facing by attaching Elastic IP addresses
-- directly to it.
--
-- After May 19, 2021, you won\'t be able to create a server using
-- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Servicesaccount if your
-- account hasn\'t already done so before May 19, 2021. If you have already
-- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
-- Servicesaccount on or before May 19, 2021, you will not be affected.
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
-- 'hostKey', 'updateServer_hostKey' - The RSA, ECDSA, or ED25519 private key to use for your SFTP-enabled
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
-- 'identityProviderDetails', 'updateServer_identityProviderDetails' - An array containing all of the information required to call a
-- customer\'s authentication API method.
--
-- 'loggingRole', 'updateServer_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
--
-- 'postAuthenticationLoginBanner', 'updateServer_postAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
--
-- 'preAuthenticationLoginBanner', 'updateServer_preAuthenticationLoginBanner' - Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
--
-- 'protocolDetails', 'updateServer_protocolDetails' - The protocol settings that are configured for your server.
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
-- 'protocols', 'updateServer_protocols' - Specifies the file transfer protocol or protocols over which your file
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
-- 'securityPolicyName', 'updateServer_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
--
-- 'workflowDetails', 'updateServer_workflowDetails' - Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In addition to a workflow to execute when a file is uploaded completely,
-- @WorkflowDetails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
--
-- To remove an associated workflow from a server, you can provide an empty
-- @OnUpload@ object, as in the following example.
--
-- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
--
-- 'serverId', 'updateServer_serverId' - A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
newUpdateServer ::
  -- | 'serverId'
  Prelude.Text ->
  UpdateServer
newUpdateServer pServerId_ =
  UpdateServer'
    { certificate = Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      hostKey = Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      postAuthenticationLoginBanner = Prelude.Nothing,
      preAuthenticationLoginBanner = Prelude.Nothing,
      protocolDetails = Prelude.Nothing,
      protocols = Prelude.Nothing,
      securityPolicyName = Prelude.Nothing,
      workflowDetails = Prelude.Nothing,
      serverId = pServerId_
    }

-- | The Amazon Resource Name (ARN) of the Amazon Web ServicesCertificate
-- Manager (ACM) certificate. Required when @Protocols@ is set to @FTPS@.
--
-- To request a new public certificate, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
--
-- To import an existing certificate into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates into ACM>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
--
-- To request a private certificate to use FTPS through private IP
-- addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-private.html Request a private certificate>
-- in the /Amazon Web ServicesCertificate Manager User Guide/.
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
updateServer_certificate :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_certificate = Lens.lens (\UpdateServer' {certificate} -> certificate) (\s@UpdateServer' {} a -> s {certificate = a} :: UpdateServer)

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your server. When you host your endpoint within your VPC, you can
-- make your endpoint accessible only to resources within your VPC, or you
-- can attach Elastic IP addresses and make your endpoint accessible to
-- clients over the internet. Your VPC\'s default security groups are
-- automatically assigned to your endpoint.
updateServer_endpointDetails :: Lens.Lens' UpdateServer (Prelude.Maybe EndpointDetails)
updateServer_endpointDetails = Lens.lens (\UpdateServer' {endpointDetails} -> endpointDetails) (\s@UpdateServer' {} a -> s {endpointDetails = a} :: UpdateServer)

-- | The type of endpoint that you want your server to use. You can choose to
-- make your server\'s endpoint publicly accessible (PUBLIC) or host it
-- inside your VPC. With an endpoint that is hosted in a VPC, you can
-- restrict access to your server and resources only within your VPC or
-- choose to make it internet facing by attaching Elastic IP addresses
-- directly to it.
--
-- After May 19, 2021, you won\'t be able to create a server using
-- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Servicesaccount if your
-- account hasn\'t already done so before May 19, 2021. If you have already
-- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
-- Servicesaccount on or before May 19, 2021, you will not be affected.
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
updateServer_endpointType :: Lens.Lens' UpdateServer (Prelude.Maybe EndpointType)
updateServer_endpointType = Lens.lens (\UpdateServer' {endpointType} -> endpointType) (\s@UpdateServer' {} a -> s {endpointType = a} :: UpdateServer)

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
updateServer_hostKey :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_hostKey = Lens.lens (\UpdateServer' {hostKey} -> hostKey) (\s@UpdateServer' {} a -> s {hostKey = a} :: UpdateServer) Prelude.. Lens.mapping Data._Sensitive

-- | An array containing all of the information required to call a
-- customer\'s authentication API method.
updateServer_identityProviderDetails :: Lens.Lens' UpdateServer (Prelude.Maybe IdentityProviderDetails)
updateServer_identityProviderDetails = Lens.lens (\UpdateServer' {identityProviderDetails} -> identityProviderDetails) (\s@UpdateServer' {} a -> s {identityProviderDetails = a} :: UpdateServer)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
updateServer_loggingRole :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_loggingRole = Lens.lens (\UpdateServer' {loggingRole} -> loggingRole) (\s@UpdateServer' {} a -> s {loggingRole = a} :: UpdateServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed after the user authenticates.
--
-- The SFTP protocol does not support post-authentication display banners.
updateServer_postAuthenticationLoginBanner :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_postAuthenticationLoginBanner = Lens.lens (\UpdateServer' {postAuthenticationLoginBanner} -> postAuthenticationLoginBanner) (\s@UpdateServer' {} a -> s {postAuthenticationLoginBanner = a} :: UpdateServer)

-- | Specifies a string to display when users connect to a server. This
-- string is displayed before the user authenticates. For example, the
-- following banner displays details about using the system:
--
-- @This system is for the use of authorized users only. Individuals using this computer system without authority, or in excess of their authority, are subject to having all of their activities on this system monitored and recorded by system personnel.@
updateServer_preAuthenticationLoginBanner :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_preAuthenticationLoginBanner = Lens.lens (\UpdateServer' {preAuthenticationLoginBanner} -> preAuthenticationLoginBanner) (\s@UpdateServer' {} a -> s {preAuthenticationLoginBanner = a} :: UpdateServer)

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
updateServer_protocolDetails :: Lens.Lens' UpdateServer (Prelude.Maybe ProtocolDetails)
updateServer_protocolDetails = Lens.lens (\UpdateServer' {protocolDetails} -> protocolDetails) (\s@UpdateServer' {} a -> s {protocolDetails = a} :: UpdateServer)

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
updateServer_protocols :: Lens.Lens' UpdateServer (Prelude.Maybe (Prelude.NonEmpty Protocol))
updateServer_protocols = Lens.lens (\UpdateServer' {protocols} -> protocols) (\s@UpdateServer' {} a -> s {protocols = a} :: UpdateServer) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the security policy that is attached to the
-- server.
updateServer_securityPolicyName :: Lens.Lens' UpdateServer (Prelude.Maybe Prelude.Text)
updateServer_securityPolicyName = Lens.lens (\UpdateServer' {securityPolicyName} -> securityPolicyName) (\s@UpdateServer' {} a -> s {securityPolicyName = a} :: UpdateServer)

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In addition to a workflow to execute when a file is uploaded completely,
-- @WorkflowDetails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
--
-- To remove an associated workflow from a server, you can provide an empty
-- @OnUpload@ object, as in the following example.
--
-- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
updateServer_workflowDetails :: Lens.Lens' UpdateServer (Prelude.Maybe WorkflowDetails)
updateServer_workflowDetails = Lens.lens (\UpdateServer' {workflowDetails} -> workflowDetails) (\s@UpdateServer' {} a -> s {workflowDetails = a} :: UpdateServer)

-- | A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
updateServer_serverId :: Lens.Lens' UpdateServer Prelude.Text
updateServer_serverId = Lens.lens (\UpdateServer' {serverId} -> serverId) (\s@UpdateServer' {} a -> s {serverId = a} :: UpdateServer)

instance Core.AWSRequest UpdateServer where
  type AWSResponse UpdateServer = UpdateServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
      )

instance Prelude.Hashable UpdateServer where
  hashWithSalt _salt UpdateServer' {..} =
    _salt `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` hostKey
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` postAuthenticationLoginBanner
      `Prelude.hashWithSalt` preAuthenticationLoginBanner
      `Prelude.hashWithSalt` protocolDetails
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` securityPolicyName
      `Prelude.hashWithSalt` workflowDetails
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData UpdateServer where
  rnf UpdateServer' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf hostKey
      `Prelude.seq` Prelude.rnf identityProviderDetails
      `Prelude.seq` Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf postAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf preAuthenticationLoginBanner
      `Prelude.seq` Prelude.rnf protocolDetails
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf securityPolicyName
      `Prelude.seq` Prelude.rnf workflowDetails
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders UpdateServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UpdateServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServer where
  toJSON UpdateServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Certificate" Data..=) Prelude.<$> certificate,
            ("EndpointDetails" Data..=)
              Prelude.<$> endpointDetails,
            ("EndpointType" Data..=) Prelude.<$> endpointType,
            ("HostKey" Data..=) Prelude.<$> hostKey,
            ("IdentityProviderDetails" Data..=)
              Prelude.<$> identityProviderDetails,
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
            ("WorkflowDetails" Data..=)
              Prelude.<$> workflowDetails,
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath UpdateServer where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server that the user account
    -- is assigned to.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServerResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'updateServerResponse_serverId' - A system-assigned unique identifier for a server that the user account
-- is assigned to.
newUpdateServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  UpdateServerResponse
newUpdateServerResponse pHttpStatus_ pServerId_ =
  UpdateServerResponse'
    { httpStatus = pHttpStatus_,
      serverId = pServerId_
    }

-- | The response's http status code.
updateServerResponse_httpStatus :: Lens.Lens' UpdateServerResponse Prelude.Int
updateServerResponse_httpStatus = Lens.lens (\UpdateServerResponse' {httpStatus} -> httpStatus) (\s@UpdateServerResponse' {} a -> s {httpStatus = a} :: UpdateServerResponse)

-- | A system-assigned unique identifier for a server that the user account
-- is assigned to.
updateServerResponse_serverId :: Lens.Lens' UpdateServerResponse Prelude.Text
updateServerResponse_serverId = Lens.lens (\UpdateServerResponse' {serverId} -> serverId) (\s@UpdateServerResponse' {} a -> s {serverId = a} :: UpdateServerResponse)

instance Prelude.NFData UpdateServerResponse where
  rnf UpdateServerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
