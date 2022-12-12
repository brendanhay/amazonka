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
-- Module      : Amazonka.OpsWorksCM.CreateServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and immedately starts a new server. The server is ready to use
-- when it is in the @HEALTHY@ state. By default, you can create a maximum
-- of 10 servers.
--
-- This operation is asynchronous.
--
-- A @LimitExceededException@ is thrown when you have created the maximum
-- number of servers (10). A @ResourceAlreadyExistsException@ is thrown
-- when a server with the same name already exists in the account. A
-- @ResourceNotFoundException@ is thrown when you specify a backup ID that
-- is not valid or is for a backup that does not exist. A
-- @ValidationException@ is thrown when parameters of the request are not
-- valid.
--
-- If you do not specify a security group by adding the @SecurityGroupIds@
-- parameter, AWS OpsWorks creates a new security group.
--
-- /Chef Automate:/ The default security group opens the Chef server to the
-- world on TCP port 443. If a KeyName is present, AWS OpsWorks enables SSH
-- access. SSH is also open to the world on TCP port 22.
--
-- /Puppet Enterprise:/ The default security group opens TCP ports 22, 443,
-- 4433, 8140, 8142, 8143, and 8170. If a KeyName is present, AWS OpsWorks
-- enables SSH access. SSH is also open to the world on TCP port 22.
--
-- By default, your server is accessible from any IP address. We recommend
-- that you update your security group rules to allow access from known IP
-- addresses and address ranges only. To edit security group rules, open
-- Security Groups in the navigation pane of the EC2 management console.
--
-- To specify your own domain for a server, and provide your own
-- self-signed or CA-signed certificate and private key, specify values for
-- @CustomDomain@, @CustomCertificate@, and @CustomPrivateKey@.
module Amazonka.OpsWorksCM.CreateServer
  ( -- * Creating a Request
    CreateServer (..),
    newCreateServer,

    -- * Request Lenses
    createServer_associatePublicIpAddress,
    createServer_backupId,
    createServer_backupRetentionCount,
    createServer_customCertificate,
    createServer_customDomain,
    createServer_customPrivateKey,
    createServer_disableAutomatedBackup,
    createServer_engineAttributes,
    createServer_engineModel,
    createServer_engineVersion,
    createServer_keyPair,
    createServer_preferredBackupWindow,
    createServer_preferredMaintenanceWindow,
    createServer_securityGroupIds,
    createServer_subnetIds,
    createServer_tags,
    createServer_engine,
    createServer_serverName,
    createServer_instanceProfileArn,
    createServer_instanceType,
    createServer_serviceRoleArn,

    -- * Destructuring the Response
    CreateServerResponse (..),
    newCreateServerResponse,

    -- * Response Lenses
    createServerResponse_server,
    createServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServer' smart constructor.
data CreateServer = CreateServer'
  { -- | Associate a public IP address with a server that you are launching.
    -- Valid values are @true@ or @false@. The default value is @true@.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | If you specify this field, AWS OpsWorks CM creates the server by using
    -- the backup represented by BackupId.
    backupId :: Prelude.Maybe Prelude.Text,
    -- | The number of automated backups that you want to keep. Whenever a new
    -- backup is created, AWS OpsWorks CM deletes the oldest backups if this
    -- number is exceeded. The default value is @1@.
    backupRetentionCount :: Prelude.Maybe Prelude.Natural,
    -- | A PEM-formatted HTTPS certificate. The value can be be a single,
    -- self-signed certificate, or a certificate chain. If you specify a custom
    -- certificate, you must also specify values for @CustomDomain@ and
    -- @CustomPrivateKey@. The following are requirements for the
    -- @CustomCertificate@ value:
    --
    -- -   You can provide either a self-signed, custom certificate, or the
    --     full certificate chain.
    --
    -- -   The certificate must be a valid X509 certificate, or a certificate
    --     chain in PEM format.
    --
    -- -   The certificate must be valid at the time of upload. A certificate
    --     can\'t be used before its validity period begins (the certificate\'s
    --     @NotBefore@ date), or after it expires (the certificate\'s
    --     @NotAfter@ date).
    --
    -- -   The certificate’s common name or subject alternative names (SANs),
    --     if present, must match the value of @CustomDomain@.
    --
    -- -   The certificate must match the value of @CustomPrivateKey@.
    customCertificate :: Prelude.Maybe Prelude.Text,
    -- | An optional public endpoint of a server, such as
    -- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
    -- record in your preferred DNS service that points the custom domain to
    -- the endpoint that is generated when the server is created (the value of
    -- the CreateServer Endpoint attribute). You cannot access the server by
    -- using the generated @Endpoint@ value if the server is using a custom
    -- domain. If you specify a custom domain, you must also specify values for
    -- @CustomCertificate@ and @CustomPrivateKey@.
    customDomain :: Prelude.Maybe Prelude.Text,
    -- | A private key in PEM format for connecting to the server by using HTTPS.
    -- The private key must not be encrypted; it cannot be protected by a
    -- password or passphrase. If you specify a custom private key, you must
    -- also specify values for @CustomDomain@ and @CustomCertificate@.
    customPrivateKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Enable or disable scheduled backups. Valid values are @true@ or @false@.
    -- The default value is @true@.
    disableAutomatedBackup :: Prelude.Maybe Prelude.Bool,
    -- | Optional engine attributes on a specified server.
    --
    -- __Attributes accepted in a Chef createServer request:__
    --
    -- -   @CHEF_AUTOMATE_PIVOTAL_KEY@: A base64-encoded RSA public key. The
    --     corresponding private key is required to access the Chef API. When
    --     no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and
    --     returned in the response.
    --
    -- -   @CHEF_AUTOMATE_ADMIN_PASSWORD@: The password for the administrative
    --     user in the Chef Automate web-based dashboard. The password length
    --     is a minimum of eight characters, and a maximum of 32. The password
    --     can contain letters, numbers, and special characters
    --     (!\/\@#$%^&+=_). The password must contain at least one lower case
    --     letter, one upper case letter, one number, and one special
    --     character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is
    --     generated and returned in the response.
    --
    -- __Attributes accepted in a Puppet createServer request:__
    --
    -- -   @PUPPET_ADMIN_PASSWORD@: To work with the Puppet Enterprise console,
    --     a password must use ASCII characters.
    --
    -- -   @PUPPET_R10K_REMOTE@: The r10k remote is the URL of your control
    --     repository (for example,
    --     ssh:\/\/git\@your.git-repo.com:user\/control-repo.git). Specifying
    --     an r10k remote opens TCP port 8170.
    --
    -- -   @PUPPET_R10K_PRIVATE_KEY@: If you are using a private Git
    --     repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded
    --     private SSH key.
    engineAttributes :: Prelude.Maybe [EngineAttribute],
    -- | The engine model of the server. Valid values in this release include
    -- @Monolithic@ for Puppet and @Single@ for Chef.
    engineModel :: Prelude.Maybe Prelude.Text,
    -- | The major release version of the engine that you want to use. For a Chef
    -- server, the valid value for EngineVersion is currently @2@. For a Puppet
    -- server, valid values are @2019@ or @2017@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 key pair to set for the instance. This parameter is
    -- optional; if desired, you may specify this parameter to connect to your
    -- instances by using SSH.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | The start time for a one-hour period during which AWS OpsWorks CM backs
    -- up application-level data on your server if automated backups are
    -- enabled. Valid values must be specified in one of the following formats:
    --
    -- -   @HH:MM@ for daily backups
    --
    -- -   @DDD:HH:MM@ for weekly backups
    --
    -- @MM@ must be specified as @00@. The specified time is in coordinated
    -- universal time (UTC). The default value is a random, daily start time.
    --
    -- __Example:__ @08:00@, which represents a daily start time of 08:00 UTC.
    --
    -- __Example:__ @Mon:08:00@, which represents a start time of every Monday
    -- at 08:00 UTC. (8:00 a.m.)
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The start time for a one-hour period each week during which AWS OpsWorks
    -- CM performs maintenance on the instance. Valid values must be specified
    -- in the following format: @DDD:HH:MM@. @MM@ must be specified as @00@.
    -- The specified time is in coordinated universal time (UTC). The default
    -- value is a random one-hour period on Tuesday, Wednesday, or Friday. See
    -- @TimeWindowDefinition@ for more information.
    --
    -- __Example:__ @Mon:08:00@, which represents a start time of every Monday
    -- at 08:00 UTC. (8:00 a.m.)
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of security group IDs to attach to the Amazon EC2 instance. If
    -- you add this parameter, the specified security groups must be within the
    -- VPC that is specified by @SubnetIds@.
    --
    -- If you do not specify this parameter, AWS OpsWorks CM creates one new
    -- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
    -- (everyone).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of subnets in which to launch the server EC2 instance.
    --
    -- Amazon EC2-Classic customers: This field is required. All servers must
    -- run within a VPC. The VPC must have \"Auto Assign Public IP\" enabled.
    --
    -- EC2-VPC customers: This field is optional. If you do not specify subnet
    -- IDs, your EC2 instances are created in a default subnet that is selected
    -- by Amazon EC2. If you specify subnet IDs, the VPC must have \"Auto
    -- Assign Public IP\" enabled.
    --
    -- For more information about supported Amazon EC2 platforms, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A map that contains tag keys and tag values to attach to an AWS OpsWorks
    -- for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
    --
    -- -   The key cannot be empty.
    --
    -- -   The key can be a maximum of 127 characters, and can contain only
    --     Unicode letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/ \@@
    --
    -- -   The value can be a maximum 255 characters, and contain only Unicode
    --     letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/ \@@
    --
    -- -   Leading and trailing white spaces are trimmed from both the key and
    --     value.
    --
    -- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
    --     server.
    tags :: Prelude.Maybe [Tag],
    -- | The configuration management engine to use. Valid values include
    -- @ChefAutomate@ and @Puppet@.
    engine :: Prelude.Text,
    -- | The name of the server. The server name must be unique within your AWS
    -- account, within each region. Server names must start with a letter; then
    -- letters, numbers, or hyphens (-) are allowed, up to a maximum of 40
    -- characters.
    serverName :: Prelude.Text,
    -- | The ARN of the instance profile that your Amazon EC2 instances use.
    -- Although the AWS OpsWorks console typically creates the instance profile
    -- for you, if you are using API commands instead, run the
    -- service-role-creation.yaml AWS CloudFormation template, located at
    -- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
    -- This template creates a CloudFormation stack that includes the instance
    -- profile you need.
    instanceProfileArn :: Prelude.Text,
    -- | The Amazon EC2 instance type to use. For example, @m5.large@.
    instanceType :: Prelude.Text,
    -- | The service role that the AWS OpsWorks CM service backend uses to work
    -- with your account. Although the AWS OpsWorks management console
    -- typically creates the service role for you, if you are using the AWS CLI
    -- or API commands, run the service-role-creation.yaml AWS CloudFormation
    -- template, located at
    -- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
    -- This template creates a CloudFormation stack that includes the service
    -- role and instance profile that you need.
    serviceRoleArn :: Prelude.Text
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
-- 'associatePublicIpAddress', 'createServer_associatePublicIpAddress' - Associate a public IP address with a server that you are launching.
-- Valid values are @true@ or @false@. The default value is @true@.
--
-- 'backupId', 'createServer_backupId' - If you specify this field, AWS OpsWorks CM creates the server by using
-- the backup represented by BackupId.
--
-- 'backupRetentionCount', 'createServer_backupRetentionCount' - The number of automated backups that you want to keep. Whenever a new
-- backup is created, AWS OpsWorks CM deletes the oldest backups if this
-- number is exceeded. The default value is @1@.
--
-- 'customCertificate', 'createServer_customCertificate' - A PEM-formatted HTTPS certificate. The value can be be a single,
-- self-signed certificate, or a certificate chain. If you specify a custom
-- certificate, you must also specify values for @CustomDomain@ and
-- @CustomPrivateKey@. The following are requirements for the
-- @CustomCertificate@ value:
--
-- -   You can provide either a self-signed, custom certificate, or the
--     full certificate chain.
--
-- -   The certificate must be a valid X509 certificate, or a certificate
--     chain in PEM format.
--
-- -   The certificate must be valid at the time of upload. A certificate
--     can\'t be used before its validity period begins (the certificate\'s
--     @NotBefore@ date), or after it expires (the certificate\'s
--     @NotAfter@ date).
--
-- -   The certificate’s common name or subject alternative names (SANs),
--     if present, must match the value of @CustomDomain@.
--
-- -   The certificate must match the value of @CustomPrivateKey@.
--
-- 'customDomain', 'createServer_customDomain' - An optional public endpoint of a server, such as
-- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
-- record in your preferred DNS service that points the custom domain to
-- the endpoint that is generated when the server is created (the value of
-- the CreateServer Endpoint attribute). You cannot access the server by
-- using the generated @Endpoint@ value if the server is using a custom
-- domain. If you specify a custom domain, you must also specify values for
-- @CustomCertificate@ and @CustomPrivateKey@.
--
-- 'customPrivateKey', 'createServer_customPrivateKey' - A private key in PEM format for connecting to the server by using HTTPS.
-- The private key must not be encrypted; it cannot be protected by a
-- password or passphrase. If you specify a custom private key, you must
-- also specify values for @CustomDomain@ and @CustomCertificate@.
--
-- 'disableAutomatedBackup', 'createServer_disableAutomatedBackup' - Enable or disable scheduled backups. Valid values are @true@ or @false@.
-- The default value is @true@.
--
-- 'engineAttributes', 'createServer_engineAttributes' - Optional engine attributes on a specified server.
--
-- __Attributes accepted in a Chef createServer request:__
--
-- -   @CHEF_AUTOMATE_PIVOTAL_KEY@: A base64-encoded RSA public key. The
--     corresponding private key is required to access the Chef API. When
--     no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and
--     returned in the response.
--
-- -   @CHEF_AUTOMATE_ADMIN_PASSWORD@: The password for the administrative
--     user in the Chef Automate web-based dashboard. The password length
--     is a minimum of eight characters, and a maximum of 32. The password
--     can contain letters, numbers, and special characters
--     (!\/\@#$%^&+=_). The password must contain at least one lower case
--     letter, one upper case letter, one number, and one special
--     character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is
--     generated and returned in the response.
--
-- __Attributes accepted in a Puppet createServer request:__
--
-- -   @PUPPET_ADMIN_PASSWORD@: To work with the Puppet Enterprise console,
--     a password must use ASCII characters.
--
-- -   @PUPPET_R10K_REMOTE@: The r10k remote is the URL of your control
--     repository (for example,
--     ssh:\/\/git\@your.git-repo.com:user\/control-repo.git). Specifying
--     an r10k remote opens TCP port 8170.
--
-- -   @PUPPET_R10K_PRIVATE_KEY@: If you are using a private Git
--     repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded
--     private SSH key.
--
-- 'engineModel', 'createServer_engineModel' - The engine model of the server. Valid values in this release include
-- @Monolithic@ for Puppet and @Single@ for Chef.
--
-- 'engineVersion', 'createServer_engineVersion' - The major release version of the engine that you want to use. For a Chef
-- server, the valid value for EngineVersion is currently @2@. For a Puppet
-- server, valid values are @2019@ or @2017@.
--
-- 'keyPair', 'createServer_keyPair' - The Amazon EC2 key pair to set for the instance. This parameter is
-- optional; if desired, you may specify this parameter to connect to your
-- instances by using SSH.
--
-- 'preferredBackupWindow', 'createServer_preferredBackupWindow' - The start time for a one-hour period during which AWS OpsWorks CM backs
-- up application-level data on your server if automated backups are
-- enabled. Valid values must be specified in one of the following formats:
--
-- -   @HH:MM@ for daily backups
--
-- -   @DDD:HH:MM@ for weekly backups
--
-- @MM@ must be specified as @00@. The specified time is in coordinated
-- universal time (UTC). The default value is a random, daily start time.
--
-- __Example:__ @08:00@, which represents a daily start time of 08:00 UTC.
--
-- __Example:__ @Mon:08:00@, which represents a start time of every Monday
-- at 08:00 UTC. (8:00 a.m.)
--
-- 'preferredMaintenanceWindow', 'createServer_preferredMaintenanceWindow' - The start time for a one-hour period each week during which AWS OpsWorks
-- CM performs maintenance on the instance. Valid values must be specified
-- in the following format: @DDD:HH:MM@. @MM@ must be specified as @00@.
-- The specified time is in coordinated universal time (UTC). The default
-- value is a random one-hour period on Tuesday, Wednesday, or Friday. See
-- @TimeWindowDefinition@ for more information.
--
-- __Example:__ @Mon:08:00@, which represents a start time of every Monday
-- at 08:00 UTC. (8:00 a.m.)
--
-- 'securityGroupIds', 'createServer_securityGroupIds' - A list of security group IDs to attach to the Amazon EC2 instance. If
-- you add this parameter, the specified security groups must be within the
-- VPC that is specified by @SubnetIds@.
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new
-- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
-- (everyone).
--
-- 'subnetIds', 'createServer_subnetIds' - The IDs of subnets in which to launch the server EC2 instance.
--
-- Amazon EC2-Classic customers: This field is required. All servers must
-- run within a VPC. The VPC must have \"Auto Assign Public IP\" enabled.
--
-- EC2-VPC customers: This field is optional. If you do not specify subnet
-- IDs, your EC2 instances are created in a default subnet that is selected
-- by Amazon EC2. If you specify subnet IDs, the VPC must have \"Auto
-- Assign Public IP\" enabled.
--
-- For more information about supported Amazon EC2 platforms, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
--
-- 'tags', 'createServer_tags' - A map that contains tag keys and tag values to attach to an AWS OpsWorks
-- for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/ \@@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/ \@@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
--     server.
--
-- 'engine', 'createServer_engine' - The configuration management engine to use. Valid values include
-- @ChefAutomate@ and @Puppet@.
--
-- 'serverName', 'createServer_serverName' - The name of the server. The server name must be unique within your AWS
-- account, within each region. Server names must start with a letter; then
-- letters, numbers, or hyphens (-) are allowed, up to a maximum of 40
-- characters.
--
-- 'instanceProfileArn', 'createServer_instanceProfileArn' - The ARN of the instance profile that your Amazon EC2 instances use.
-- Although the AWS OpsWorks console typically creates the instance profile
-- for you, if you are using API commands instead, run the
-- service-role-creation.yaml AWS CloudFormation template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the instance
-- profile you need.
--
-- 'instanceType', 'createServer_instanceType' - The Amazon EC2 instance type to use. For example, @m5.large@.
--
-- 'serviceRoleArn', 'createServer_serviceRoleArn' - The service role that the AWS OpsWorks CM service backend uses to work
-- with your account. Although the AWS OpsWorks management console
-- typically creates the service role for you, if you are using the AWS CLI
-- or API commands, run the service-role-creation.yaml AWS CloudFormation
-- template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the service
-- role and instance profile that you need.
newCreateServer ::
  -- | 'engine'
  Prelude.Text ->
  -- | 'serverName'
  Prelude.Text ->
  -- | 'instanceProfileArn'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'serviceRoleArn'
  Prelude.Text ->
  CreateServer
newCreateServer
  pEngine_
  pServerName_
  pInstanceProfileArn_
  pInstanceType_
  pServiceRoleArn_ =
    CreateServer'
      { associatePublicIpAddress =
          Prelude.Nothing,
        backupId = Prelude.Nothing,
        backupRetentionCount = Prelude.Nothing,
        customCertificate = Prelude.Nothing,
        customDomain = Prelude.Nothing,
        customPrivateKey = Prelude.Nothing,
        disableAutomatedBackup = Prelude.Nothing,
        engineAttributes = Prelude.Nothing,
        engineModel = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        keyPair = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        engine = pEngine_,
        serverName = pServerName_,
        instanceProfileArn = pInstanceProfileArn_,
        instanceType = pInstanceType_,
        serviceRoleArn = pServiceRoleArn_
      }

-- | Associate a public IP address with a server that you are launching.
-- Valid values are @true@ or @false@. The default value is @true@.
createServer_associatePublicIpAddress :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Bool)
createServer_associatePublicIpAddress = Lens.lens (\CreateServer' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@CreateServer' {} a -> s {associatePublicIpAddress = a} :: CreateServer)

-- | If you specify this field, AWS OpsWorks CM creates the server by using
-- the backup represented by BackupId.
createServer_backupId :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_backupId = Lens.lens (\CreateServer' {backupId} -> backupId) (\s@CreateServer' {} a -> s {backupId = a} :: CreateServer)

-- | The number of automated backups that you want to keep. Whenever a new
-- backup is created, AWS OpsWorks CM deletes the oldest backups if this
-- number is exceeded. The default value is @1@.
createServer_backupRetentionCount :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Natural)
createServer_backupRetentionCount = Lens.lens (\CreateServer' {backupRetentionCount} -> backupRetentionCount) (\s@CreateServer' {} a -> s {backupRetentionCount = a} :: CreateServer)

-- | A PEM-formatted HTTPS certificate. The value can be be a single,
-- self-signed certificate, or a certificate chain. If you specify a custom
-- certificate, you must also specify values for @CustomDomain@ and
-- @CustomPrivateKey@. The following are requirements for the
-- @CustomCertificate@ value:
--
-- -   You can provide either a self-signed, custom certificate, or the
--     full certificate chain.
--
-- -   The certificate must be a valid X509 certificate, or a certificate
--     chain in PEM format.
--
-- -   The certificate must be valid at the time of upload. A certificate
--     can\'t be used before its validity period begins (the certificate\'s
--     @NotBefore@ date), or after it expires (the certificate\'s
--     @NotAfter@ date).
--
-- -   The certificate’s common name or subject alternative names (SANs),
--     if present, must match the value of @CustomDomain@.
--
-- -   The certificate must match the value of @CustomPrivateKey@.
createServer_customCertificate :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_customCertificate = Lens.lens (\CreateServer' {customCertificate} -> customCertificate) (\s@CreateServer' {} a -> s {customCertificate = a} :: CreateServer)

-- | An optional public endpoint of a server, such as
-- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
-- record in your preferred DNS service that points the custom domain to
-- the endpoint that is generated when the server is created (the value of
-- the CreateServer Endpoint attribute). You cannot access the server by
-- using the generated @Endpoint@ value if the server is using a custom
-- domain. If you specify a custom domain, you must also specify values for
-- @CustomCertificate@ and @CustomPrivateKey@.
createServer_customDomain :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_customDomain = Lens.lens (\CreateServer' {customDomain} -> customDomain) (\s@CreateServer' {} a -> s {customDomain = a} :: CreateServer)

-- | A private key in PEM format for connecting to the server by using HTTPS.
-- The private key must not be encrypted; it cannot be protected by a
-- password or passphrase. If you specify a custom private key, you must
-- also specify values for @CustomDomain@ and @CustomCertificate@.
createServer_customPrivateKey :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_customPrivateKey = Lens.lens (\CreateServer' {customPrivateKey} -> customPrivateKey) (\s@CreateServer' {} a -> s {customPrivateKey = a} :: CreateServer) Prelude.. Lens.mapping Data._Sensitive

-- | Enable or disable scheduled backups. Valid values are @true@ or @false@.
-- The default value is @true@.
createServer_disableAutomatedBackup :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Bool)
createServer_disableAutomatedBackup = Lens.lens (\CreateServer' {disableAutomatedBackup} -> disableAutomatedBackup) (\s@CreateServer' {} a -> s {disableAutomatedBackup = a} :: CreateServer)

-- | Optional engine attributes on a specified server.
--
-- __Attributes accepted in a Chef createServer request:__
--
-- -   @CHEF_AUTOMATE_PIVOTAL_KEY@: A base64-encoded RSA public key. The
--     corresponding private key is required to access the Chef API. When
--     no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and
--     returned in the response.
--
-- -   @CHEF_AUTOMATE_ADMIN_PASSWORD@: The password for the administrative
--     user in the Chef Automate web-based dashboard. The password length
--     is a minimum of eight characters, and a maximum of 32. The password
--     can contain letters, numbers, and special characters
--     (!\/\@#$%^&+=_). The password must contain at least one lower case
--     letter, one upper case letter, one number, and one special
--     character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is
--     generated and returned in the response.
--
-- __Attributes accepted in a Puppet createServer request:__
--
-- -   @PUPPET_ADMIN_PASSWORD@: To work with the Puppet Enterprise console,
--     a password must use ASCII characters.
--
-- -   @PUPPET_R10K_REMOTE@: The r10k remote is the URL of your control
--     repository (for example,
--     ssh:\/\/git\@your.git-repo.com:user\/control-repo.git). Specifying
--     an r10k remote opens TCP port 8170.
--
-- -   @PUPPET_R10K_PRIVATE_KEY@: If you are using a private Git
--     repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded
--     private SSH key.
createServer_engineAttributes :: Lens.Lens' CreateServer (Prelude.Maybe [EngineAttribute])
createServer_engineAttributes = Lens.lens (\CreateServer' {engineAttributes} -> engineAttributes) (\s@CreateServer' {} a -> s {engineAttributes = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | The engine model of the server. Valid values in this release include
-- @Monolithic@ for Puppet and @Single@ for Chef.
createServer_engineModel :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_engineModel = Lens.lens (\CreateServer' {engineModel} -> engineModel) (\s@CreateServer' {} a -> s {engineModel = a} :: CreateServer)

-- | The major release version of the engine that you want to use. For a Chef
-- server, the valid value for EngineVersion is currently @2@. For a Puppet
-- server, valid values are @2019@ or @2017@.
createServer_engineVersion :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_engineVersion = Lens.lens (\CreateServer' {engineVersion} -> engineVersion) (\s@CreateServer' {} a -> s {engineVersion = a} :: CreateServer)

-- | The Amazon EC2 key pair to set for the instance. This parameter is
-- optional; if desired, you may specify this parameter to connect to your
-- instances by using SSH.
createServer_keyPair :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_keyPair = Lens.lens (\CreateServer' {keyPair} -> keyPair) (\s@CreateServer' {} a -> s {keyPair = a} :: CreateServer)

-- | The start time for a one-hour period during which AWS OpsWorks CM backs
-- up application-level data on your server if automated backups are
-- enabled. Valid values must be specified in one of the following formats:
--
-- -   @HH:MM@ for daily backups
--
-- -   @DDD:HH:MM@ for weekly backups
--
-- @MM@ must be specified as @00@. The specified time is in coordinated
-- universal time (UTC). The default value is a random, daily start time.
--
-- __Example:__ @08:00@, which represents a daily start time of 08:00 UTC.
--
-- __Example:__ @Mon:08:00@, which represents a start time of every Monday
-- at 08:00 UTC. (8:00 a.m.)
createServer_preferredBackupWindow :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_preferredBackupWindow = Lens.lens (\CreateServer' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateServer' {} a -> s {preferredBackupWindow = a} :: CreateServer)

-- | The start time for a one-hour period each week during which AWS OpsWorks
-- CM performs maintenance on the instance. Valid values must be specified
-- in the following format: @DDD:HH:MM@. @MM@ must be specified as @00@.
-- The specified time is in coordinated universal time (UTC). The default
-- value is a random one-hour period on Tuesday, Wednesday, or Friday. See
-- @TimeWindowDefinition@ for more information.
--
-- __Example:__ @Mon:08:00@, which represents a start time of every Monday
-- at 08:00 UTC. (8:00 a.m.)
createServer_preferredMaintenanceWindow :: Lens.Lens' CreateServer (Prelude.Maybe Prelude.Text)
createServer_preferredMaintenanceWindow = Lens.lens (\CreateServer' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateServer' {} a -> s {preferredMaintenanceWindow = a} :: CreateServer)

-- | A list of security group IDs to attach to the Amazon EC2 instance. If
-- you add this parameter, the specified security groups must be within the
-- VPC that is specified by @SubnetIds@.
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new
-- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
-- (everyone).
createServer_securityGroupIds :: Lens.Lens' CreateServer (Prelude.Maybe [Prelude.Text])
createServer_securityGroupIds = Lens.lens (\CreateServer' {securityGroupIds} -> securityGroupIds) (\s@CreateServer' {} a -> s {securityGroupIds = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of subnets in which to launch the server EC2 instance.
--
-- Amazon EC2-Classic customers: This field is required. All servers must
-- run within a VPC. The VPC must have \"Auto Assign Public IP\" enabled.
--
-- EC2-VPC customers: This field is optional. If you do not specify subnet
-- IDs, your EC2 instances are created in a default subnet that is selected
-- by Amazon EC2. If you specify subnet IDs, the VPC must have \"Auto
-- Assign Public IP\" enabled.
--
-- For more information about supported Amazon EC2 platforms, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
createServer_subnetIds :: Lens.Lens' CreateServer (Prelude.Maybe [Prelude.Text])
createServer_subnetIds = Lens.lens (\CreateServer' {subnetIds} -> subnetIds) (\s@CreateServer' {} a -> s {subnetIds = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | A map that contains tag keys and tag values to attach to an AWS OpsWorks
-- for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/ \@@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/ \@@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
--     server.
createServer_tags :: Lens.Lens' CreateServer (Prelude.Maybe [Tag])
createServer_tags = Lens.lens (\CreateServer' {tags} -> tags) (\s@CreateServer' {} a -> s {tags = a} :: CreateServer) Prelude.. Lens.mapping Lens.coerced

-- | The configuration management engine to use. Valid values include
-- @ChefAutomate@ and @Puppet@.
createServer_engine :: Lens.Lens' CreateServer Prelude.Text
createServer_engine = Lens.lens (\CreateServer' {engine} -> engine) (\s@CreateServer' {} a -> s {engine = a} :: CreateServer)

-- | The name of the server. The server name must be unique within your AWS
-- account, within each region. Server names must start with a letter; then
-- letters, numbers, or hyphens (-) are allowed, up to a maximum of 40
-- characters.
createServer_serverName :: Lens.Lens' CreateServer Prelude.Text
createServer_serverName = Lens.lens (\CreateServer' {serverName} -> serverName) (\s@CreateServer' {} a -> s {serverName = a} :: CreateServer)

-- | The ARN of the instance profile that your Amazon EC2 instances use.
-- Although the AWS OpsWorks console typically creates the instance profile
-- for you, if you are using API commands instead, run the
-- service-role-creation.yaml AWS CloudFormation template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the instance
-- profile you need.
createServer_instanceProfileArn :: Lens.Lens' CreateServer Prelude.Text
createServer_instanceProfileArn = Lens.lens (\CreateServer' {instanceProfileArn} -> instanceProfileArn) (\s@CreateServer' {} a -> s {instanceProfileArn = a} :: CreateServer)

-- | The Amazon EC2 instance type to use. For example, @m5.large@.
createServer_instanceType :: Lens.Lens' CreateServer Prelude.Text
createServer_instanceType = Lens.lens (\CreateServer' {instanceType} -> instanceType) (\s@CreateServer' {} a -> s {instanceType = a} :: CreateServer)

-- | The service role that the AWS OpsWorks CM service backend uses to work
-- with your account. Although the AWS OpsWorks management console
-- typically creates the service role for you, if you are using the AWS CLI
-- or API commands, run the service-role-creation.yaml AWS CloudFormation
-- template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the service
-- role and instance profile that you need.
createServer_serviceRoleArn :: Lens.Lens' CreateServer Prelude.Text
createServer_serviceRoleArn = Lens.lens (\CreateServer' {serviceRoleArn} -> serviceRoleArn) (\s@CreateServer' {} a -> s {serviceRoleArn = a} :: CreateServer)

instance Core.AWSRequest CreateServer where
  type AWSResponse CreateServer = CreateServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServerResponse'
            Prelude.<$> (x Data..?> "Server")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServer where
  hashWithSalt _salt CreateServer' {..} =
    _salt
      `Prelude.hashWithSalt` associatePublicIpAddress
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` backupRetentionCount
      `Prelude.hashWithSalt` customCertificate
      `Prelude.hashWithSalt` customDomain
      `Prelude.hashWithSalt` customPrivateKey
      `Prelude.hashWithSalt` disableAutomatedBackup
      `Prelude.hashWithSalt` engineAttributes
      `Prelude.hashWithSalt` engineModel
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` keyPair
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` instanceProfileArn
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` serviceRoleArn

instance Prelude.NFData CreateServer where
  rnf CreateServer' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf backupRetentionCount
      `Prelude.seq` Prelude.rnf customCertificate
      `Prelude.seq` Prelude.rnf customDomain
      `Prelude.seq` Prelude.rnf customPrivateKey
      `Prelude.seq` Prelude.rnf disableAutomatedBackup
      `Prelude.seq` Prelude.rnf engineAttributes
      `Prelude.seq` Prelude.rnf engineModel
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf instanceProfileArn
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf serviceRoleArn

instance Data.ToHeaders CreateServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.CreateServer" ::
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
          [ ("AssociatePublicIpAddress" Data..=)
              Prelude.<$> associatePublicIpAddress,
            ("BackupId" Data..=) Prelude.<$> backupId,
            ("BackupRetentionCount" Data..=)
              Prelude.<$> backupRetentionCount,
            ("CustomCertificate" Data..=)
              Prelude.<$> customCertificate,
            ("CustomDomain" Data..=) Prelude.<$> customDomain,
            ("CustomPrivateKey" Data..=)
              Prelude.<$> customPrivateKey,
            ("DisableAutomatedBackup" Data..=)
              Prelude.<$> disableAutomatedBackup,
            ("EngineAttributes" Data..=)
              Prelude.<$> engineAttributes,
            ("EngineModel" Data..=) Prelude.<$> engineModel,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("KeyPair" Data..=) Prelude.<$> keyPair,
            ("PreferredBackupWindow" Data..=)
              Prelude.<$> preferredBackupWindow,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Engine" Data..= engine),
            Prelude.Just ("ServerName" Data..= serverName),
            Prelude.Just
              ("InstanceProfileArn" Data..= instanceProfileArn),
            Prelude.Just ("InstanceType" Data..= instanceType),
            Prelude.Just
              ("ServiceRoleArn" Data..= serviceRoleArn)
          ]
      )

instance Data.ToPath CreateServer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { -- | The server that is created by the request.
    server :: Prelude.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'createServerResponse_server' - The server that is created by the request.
--
-- 'httpStatus', 'createServerResponse_httpStatus' - The response's http status code.
newCreateServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServerResponse
newCreateServerResponse pHttpStatus_ =
  CreateServerResponse'
    { server = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The server that is created by the request.
createServerResponse_server :: Lens.Lens' CreateServerResponse (Prelude.Maybe Server)
createServerResponse_server = Lens.lens (\CreateServerResponse' {server} -> server) (\s@CreateServerResponse' {} a -> s {server = a} :: CreateServerResponse)

-- | The response's http status code.
createServerResponse_httpStatus :: Lens.Lens' CreateServerResponse Prelude.Int
createServerResponse_httpStatus = Lens.lens (\CreateServerResponse' {httpStatus} -> httpStatus) (\s@CreateServerResponse' {} a -> s {httpStatus = a} :: CreateServerResponse)

instance Prelude.NFData CreateServerResponse where
  rnf CreateServerResponse' {..} =
    Prelude.rnf server
      `Prelude.seq` Prelude.rnf httpStatus
