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
-- Module      : Network.AWS.OpsWorksCM.CreateServer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorksCM.CreateServer
  ( -- * Creating a Request
    CreateServer (..),
    newCreateServer,

    -- * Request Lenses
    createServer_securityGroupIds,
    createServer_preferredBackupWindow,
    createServer_disableAutomatedBackup,
    createServer_customPrivateKey,
    createServer_engineAttributes,
    createServer_customDomain,
    createServer_backupId,
    createServer_subnetIds,
    createServer_keyPair,
    createServer_associatePublicIpAddress,
    createServer_engineVersion,
    createServer_preferredMaintenanceWindow,
    createServer_tags,
    createServer_backupRetentionCount,
    createServer_engineModel,
    createServer_customCertificate,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateServer' smart constructor.
data CreateServer = CreateServer'
  { -- | A list of security group IDs to attach to the Amazon EC2 instance. If
    -- you add this parameter, the specified security groups must be within the
    -- VPC that is specified by @SubnetIds@.
    --
    -- If you do not specify this parameter, AWS OpsWorks CM creates one new
    -- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
    -- (everyone).
    securityGroupIds :: Core.Maybe [Core.Text],
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
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | Enable or disable scheduled backups. Valid values are @true@ or @false@.
    -- The default value is @true@.
    disableAutomatedBackup :: Core.Maybe Core.Bool,
    -- | A private key in PEM format for connecting to the server by using HTTPS.
    -- The private key must not be encrypted; it cannot be protected by a
    -- password or passphrase. If you specify a custom private key, you must
    -- also specify values for @CustomDomain@ and @CustomCertificate@.
    customPrivateKey :: Core.Maybe (Core.Sensitive Core.Text),
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
    engineAttributes :: Core.Maybe [EngineAttribute],
    -- | An optional public endpoint of a server, such as
    -- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
    -- record in your preferred DNS service that points the custom domain to
    -- the endpoint that is generated when the server is created (the value of
    -- the CreateServer Endpoint attribute). You cannot access the server by
    -- using the generated @Endpoint@ value if the server is using a custom
    -- domain. If you specify a custom domain, you must also specify values for
    -- @CustomCertificate@ and @CustomPrivateKey@.
    customDomain :: Core.Maybe Core.Text,
    -- | If you specify this field, AWS OpsWorks CM creates the server by using
    -- the backup represented by BackupId.
    backupId :: Core.Maybe Core.Text,
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
    subnetIds :: Core.Maybe [Core.Text],
    -- | The Amazon EC2 key pair to set for the instance. This parameter is
    -- optional; if desired, you may specify this parameter to connect to your
    -- instances by using SSH.
    keyPair :: Core.Maybe Core.Text,
    -- | Associate a public IP address with a server that you are launching.
    -- Valid values are @true@ or @false@. The default value is @true@.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | The major release version of the engine that you want to use. For a Chef
    -- server, the valid value for EngineVersion is currently @2@. For a Puppet
    -- server, the valid value is @2017@.
    engineVersion :: Core.Maybe Core.Text,
    -- | The start time for a one-hour period each week during which AWS OpsWorks
    -- CM performs maintenance on the instance. Valid values must be specified
    -- in the following format: @DDD:HH:MM@. @MM@ must be specified as @00@.
    -- The specified time is in coordinated universal time (UTC). The default
    -- value is a random one-hour period on Tuesday, Wednesday, or Friday. See
    -- @TimeWindowDefinition@ for more information.
    --
    -- __Example:__ @Mon:08:00@, which represents a start time of every Monday
    -- at 08:00 UTC. (8:00 a.m.)
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
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
    tags :: Core.Maybe [Tag],
    -- | The number of automated backups that you want to keep. Whenever a new
    -- backup is created, AWS OpsWorks CM deletes the oldest backups if this
    -- number is exceeded. The default value is @1@.
    backupRetentionCount :: Core.Maybe Core.Natural,
    -- | The engine model of the server. Valid values in this release include
    -- @Monolithic@ for Puppet and @Single@ for Chef.
    engineModel :: Core.Maybe Core.Text,
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
    customCertificate :: Core.Maybe Core.Text,
    -- | The configuration management engine to use. Valid values include
    -- @ChefAutomate@ and @Puppet@.
    engine :: Core.Text,
    -- | The name of the server. The server name must be unique within your AWS
    -- account, within each region. Server names must start with a letter; then
    -- letters, numbers, or hyphens (-) are allowed, up to a maximum of 40
    -- characters.
    serverName :: Core.Text,
    -- | The ARN of the instance profile that your Amazon EC2 instances use.
    -- Although the AWS OpsWorks console typically creates the instance profile
    -- for you, if you are using API commands instead, run the
    -- service-role-creation.yaml AWS CloudFormation template, located at
    -- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
    -- This template creates a CloudFormation stack that includes the instance
    -- profile you need.
    instanceProfileArn :: Core.Text,
    -- | The Amazon EC2 instance type to use. For example, @m5.large@.
    instanceType :: Core.Text,
    -- | The service role that the AWS OpsWorks CM service backend uses to work
    -- with your account. Although the AWS OpsWorks management console
    -- typically creates the service role for you, if you are using the AWS CLI
    -- or API commands, run the service-role-creation.yaml AWS CloudFormation
    -- template, located at
    -- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
    -- This template creates a CloudFormation stack that includes the service
    -- role and instance profile that you need.
    serviceRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createServer_securityGroupIds' - A list of security group IDs to attach to the Amazon EC2 instance. If
-- you add this parameter, the specified security groups must be within the
-- VPC that is specified by @SubnetIds@.
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new
-- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
-- (everyone).
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
-- 'disableAutomatedBackup', 'createServer_disableAutomatedBackup' - Enable or disable scheduled backups. Valid values are @true@ or @false@.
-- The default value is @true@.
--
-- 'customPrivateKey', 'createServer_customPrivateKey' - A private key in PEM format for connecting to the server by using HTTPS.
-- The private key must not be encrypted; it cannot be protected by a
-- password or passphrase. If you specify a custom private key, you must
-- also specify values for @CustomDomain@ and @CustomCertificate@.
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
-- 'customDomain', 'createServer_customDomain' - An optional public endpoint of a server, such as
-- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
-- record in your preferred DNS service that points the custom domain to
-- the endpoint that is generated when the server is created (the value of
-- the CreateServer Endpoint attribute). You cannot access the server by
-- using the generated @Endpoint@ value if the server is using a custom
-- domain. If you specify a custom domain, you must also specify values for
-- @CustomCertificate@ and @CustomPrivateKey@.
--
-- 'backupId', 'createServer_backupId' - If you specify this field, AWS OpsWorks CM creates the server by using
-- the backup represented by BackupId.
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
-- 'keyPair', 'createServer_keyPair' - The Amazon EC2 key pair to set for the instance. This parameter is
-- optional; if desired, you may specify this parameter to connect to your
-- instances by using SSH.
--
-- 'associatePublicIpAddress', 'createServer_associatePublicIpAddress' - Associate a public IP address with a server that you are launching.
-- Valid values are @true@ or @false@. The default value is @true@.
--
-- 'engineVersion', 'createServer_engineVersion' - The major release version of the engine that you want to use. For a Chef
-- server, the valid value for EngineVersion is currently @2@. For a Puppet
-- server, the valid value is @2017@.
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
-- 'backupRetentionCount', 'createServer_backupRetentionCount' - The number of automated backups that you want to keep. Whenever a new
-- backup is created, AWS OpsWorks CM deletes the oldest backups if this
-- number is exceeded. The default value is @1@.
--
-- 'engineModel', 'createServer_engineModel' - The engine model of the server. Valid values in this release include
-- @Monolithic@ for Puppet and @Single@ for Chef.
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
  Core.Text ->
  -- | 'serverName'
  Core.Text ->
  -- | 'instanceProfileArn'
  Core.Text ->
  -- | 'instanceType'
  Core.Text ->
  -- | 'serviceRoleArn'
  Core.Text ->
  CreateServer
newCreateServer
  pEngine_
  pServerName_
  pInstanceProfileArn_
  pInstanceType_
  pServiceRoleArn_ =
    CreateServer'
      { securityGroupIds = Core.Nothing,
        preferredBackupWindow = Core.Nothing,
        disableAutomatedBackup = Core.Nothing,
        customPrivateKey = Core.Nothing,
        engineAttributes = Core.Nothing,
        customDomain = Core.Nothing,
        backupId = Core.Nothing,
        subnetIds = Core.Nothing,
        keyPair = Core.Nothing,
        associatePublicIpAddress = Core.Nothing,
        engineVersion = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        tags = Core.Nothing,
        backupRetentionCount = Core.Nothing,
        engineModel = Core.Nothing,
        customCertificate = Core.Nothing,
        engine = pEngine_,
        serverName = pServerName_,
        instanceProfileArn = pInstanceProfileArn_,
        instanceType = pInstanceType_,
        serviceRoleArn = pServiceRoleArn_
      }

-- | A list of security group IDs to attach to the Amazon EC2 instance. If
-- you add this parameter, the specified security groups must be within the
-- VPC that is specified by @SubnetIds@.
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new
-- security group that uses TCP ports 22 and 443, open to 0.0.0.0\/0
-- (everyone).
createServer_securityGroupIds :: Lens.Lens' CreateServer (Core.Maybe [Core.Text])
createServer_securityGroupIds = Lens.lens (\CreateServer' {securityGroupIds} -> securityGroupIds) (\s@CreateServer' {} a -> s {securityGroupIds = a} :: CreateServer) Core.. Lens.mapping Lens._Coerce

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
createServer_preferredBackupWindow :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_preferredBackupWindow = Lens.lens (\CreateServer' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateServer' {} a -> s {preferredBackupWindow = a} :: CreateServer)

-- | Enable or disable scheduled backups. Valid values are @true@ or @false@.
-- The default value is @true@.
createServer_disableAutomatedBackup :: Lens.Lens' CreateServer (Core.Maybe Core.Bool)
createServer_disableAutomatedBackup = Lens.lens (\CreateServer' {disableAutomatedBackup} -> disableAutomatedBackup) (\s@CreateServer' {} a -> s {disableAutomatedBackup = a} :: CreateServer)

-- | A private key in PEM format for connecting to the server by using HTTPS.
-- The private key must not be encrypted; it cannot be protected by a
-- password or passphrase. If you specify a custom private key, you must
-- also specify values for @CustomDomain@ and @CustomCertificate@.
createServer_customPrivateKey :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_customPrivateKey = Lens.lens (\CreateServer' {customPrivateKey} -> customPrivateKey) (\s@CreateServer' {} a -> s {customPrivateKey = a} :: CreateServer) Core.. Lens.mapping Core._Sensitive

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
createServer_engineAttributes :: Lens.Lens' CreateServer (Core.Maybe [EngineAttribute])
createServer_engineAttributes = Lens.lens (\CreateServer' {engineAttributes} -> engineAttributes) (\s@CreateServer' {} a -> s {engineAttributes = a} :: CreateServer) Core.. Lens.mapping Lens._Coerce

-- | An optional public endpoint of a server, such as
-- @https:\/\/aws.my-company.com@. To access the server, create a CNAME DNS
-- record in your preferred DNS service that points the custom domain to
-- the endpoint that is generated when the server is created (the value of
-- the CreateServer Endpoint attribute). You cannot access the server by
-- using the generated @Endpoint@ value if the server is using a custom
-- domain. If you specify a custom domain, you must also specify values for
-- @CustomCertificate@ and @CustomPrivateKey@.
createServer_customDomain :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_customDomain = Lens.lens (\CreateServer' {customDomain} -> customDomain) (\s@CreateServer' {} a -> s {customDomain = a} :: CreateServer)

-- | If you specify this field, AWS OpsWorks CM creates the server by using
-- the backup represented by BackupId.
createServer_backupId :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_backupId = Lens.lens (\CreateServer' {backupId} -> backupId) (\s@CreateServer' {} a -> s {backupId = a} :: CreateServer)

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
createServer_subnetIds :: Lens.Lens' CreateServer (Core.Maybe [Core.Text])
createServer_subnetIds = Lens.lens (\CreateServer' {subnetIds} -> subnetIds) (\s@CreateServer' {} a -> s {subnetIds = a} :: CreateServer) Core.. Lens.mapping Lens._Coerce

-- | The Amazon EC2 key pair to set for the instance. This parameter is
-- optional; if desired, you may specify this parameter to connect to your
-- instances by using SSH.
createServer_keyPair :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_keyPair = Lens.lens (\CreateServer' {keyPair} -> keyPair) (\s@CreateServer' {} a -> s {keyPair = a} :: CreateServer)

-- | Associate a public IP address with a server that you are launching.
-- Valid values are @true@ or @false@. The default value is @true@.
createServer_associatePublicIpAddress :: Lens.Lens' CreateServer (Core.Maybe Core.Bool)
createServer_associatePublicIpAddress = Lens.lens (\CreateServer' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@CreateServer' {} a -> s {associatePublicIpAddress = a} :: CreateServer)

-- | The major release version of the engine that you want to use. For a Chef
-- server, the valid value for EngineVersion is currently @2@. For a Puppet
-- server, the valid value is @2017@.
createServer_engineVersion :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_engineVersion = Lens.lens (\CreateServer' {engineVersion} -> engineVersion) (\s@CreateServer' {} a -> s {engineVersion = a} :: CreateServer)

-- | The start time for a one-hour period each week during which AWS OpsWorks
-- CM performs maintenance on the instance. Valid values must be specified
-- in the following format: @DDD:HH:MM@. @MM@ must be specified as @00@.
-- The specified time is in coordinated universal time (UTC). The default
-- value is a random one-hour period on Tuesday, Wednesday, or Friday. See
-- @TimeWindowDefinition@ for more information.
--
-- __Example:__ @Mon:08:00@, which represents a start time of every Monday
-- at 08:00 UTC. (8:00 a.m.)
createServer_preferredMaintenanceWindow :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_preferredMaintenanceWindow = Lens.lens (\CreateServer' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateServer' {} a -> s {preferredMaintenanceWindow = a} :: CreateServer)

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
createServer_tags :: Lens.Lens' CreateServer (Core.Maybe [Tag])
createServer_tags = Lens.lens (\CreateServer' {tags} -> tags) (\s@CreateServer' {} a -> s {tags = a} :: CreateServer) Core.. Lens.mapping Lens._Coerce

-- | The number of automated backups that you want to keep. Whenever a new
-- backup is created, AWS OpsWorks CM deletes the oldest backups if this
-- number is exceeded. The default value is @1@.
createServer_backupRetentionCount :: Lens.Lens' CreateServer (Core.Maybe Core.Natural)
createServer_backupRetentionCount = Lens.lens (\CreateServer' {backupRetentionCount} -> backupRetentionCount) (\s@CreateServer' {} a -> s {backupRetentionCount = a} :: CreateServer)

-- | The engine model of the server. Valid values in this release include
-- @Monolithic@ for Puppet and @Single@ for Chef.
createServer_engineModel :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_engineModel = Lens.lens (\CreateServer' {engineModel} -> engineModel) (\s@CreateServer' {} a -> s {engineModel = a} :: CreateServer)

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
createServer_customCertificate :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
createServer_customCertificate = Lens.lens (\CreateServer' {customCertificate} -> customCertificate) (\s@CreateServer' {} a -> s {customCertificate = a} :: CreateServer)

-- | The configuration management engine to use. Valid values include
-- @ChefAutomate@ and @Puppet@.
createServer_engine :: Lens.Lens' CreateServer Core.Text
createServer_engine = Lens.lens (\CreateServer' {engine} -> engine) (\s@CreateServer' {} a -> s {engine = a} :: CreateServer)

-- | The name of the server. The server name must be unique within your AWS
-- account, within each region. Server names must start with a letter; then
-- letters, numbers, or hyphens (-) are allowed, up to a maximum of 40
-- characters.
createServer_serverName :: Lens.Lens' CreateServer Core.Text
createServer_serverName = Lens.lens (\CreateServer' {serverName} -> serverName) (\s@CreateServer' {} a -> s {serverName = a} :: CreateServer)

-- | The ARN of the instance profile that your Amazon EC2 instances use.
-- Although the AWS OpsWorks console typically creates the instance profile
-- for you, if you are using API commands instead, run the
-- service-role-creation.yaml AWS CloudFormation template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the instance
-- profile you need.
createServer_instanceProfileArn :: Lens.Lens' CreateServer Core.Text
createServer_instanceProfileArn = Lens.lens (\CreateServer' {instanceProfileArn} -> instanceProfileArn) (\s@CreateServer' {} a -> s {instanceProfileArn = a} :: CreateServer)

-- | The Amazon EC2 instance type to use. For example, @m5.large@.
createServer_instanceType :: Lens.Lens' CreateServer Core.Text
createServer_instanceType = Lens.lens (\CreateServer' {instanceType} -> instanceType) (\s@CreateServer' {} a -> s {instanceType = a} :: CreateServer)

-- | The service role that the AWS OpsWorks CM service backend uses to work
-- with your account. Although the AWS OpsWorks management console
-- typically creates the service role for you, if you are using the AWS CLI
-- or API commands, run the service-role-creation.yaml AWS CloudFormation
-- template, located at
-- https:\/\/s3.amazonaws.com\/opsworks-cm-us-east-1-prod-default-assets\/misc\/opsworks-cm-roles.yaml.
-- This template creates a CloudFormation stack that includes the service
-- role and instance profile that you need.
createServer_serviceRoleArn :: Lens.Lens' CreateServer Core.Text
createServer_serviceRoleArn = Lens.lens (\CreateServer' {serviceRoleArn} -> serviceRoleArn) (\s@CreateServer' {} a -> s {serviceRoleArn = a} :: CreateServer)

instance Core.AWSRequest CreateServer where
  type AWSResponse CreateServer = CreateServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServerResponse'
            Core.<$> (x Core..?> "Server")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateServer

instance Core.NFData CreateServer

instance Core.ToHeaders CreateServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.CreateServer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateServer where
  toJSON CreateServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("PreferredBackupWindow" Core..=)
              Core.<$> preferredBackupWindow,
            ("DisableAutomatedBackup" Core..=)
              Core.<$> disableAutomatedBackup,
            ("CustomPrivateKey" Core..=)
              Core.<$> customPrivateKey,
            ("EngineAttributes" Core..=)
              Core.<$> engineAttributes,
            ("CustomDomain" Core..=) Core.<$> customDomain,
            ("BackupId" Core..=) Core.<$> backupId,
            ("SubnetIds" Core..=) Core.<$> subnetIds,
            ("KeyPair" Core..=) Core.<$> keyPair,
            ("AssociatePublicIpAddress" Core..=)
              Core.<$> associatePublicIpAddress,
            ("EngineVersion" Core..=) Core.<$> engineVersion,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("Tags" Core..=) Core.<$> tags,
            ("BackupRetentionCount" Core..=)
              Core.<$> backupRetentionCount,
            ("EngineModel" Core..=) Core.<$> engineModel,
            ("CustomCertificate" Core..=)
              Core.<$> customCertificate,
            Core.Just ("Engine" Core..= engine),
            Core.Just ("ServerName" Core..= serverName),
            Core.Just
              ("InstanceProfileArn" Core..= instanceProfileArn),
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("ServiceRoleArn" Core..= serviceRoleArn)
          ]
      )

instance Core.ToPath CreateServer where
  toPath = Core.const "/"

instance Core.ToQuery CreateServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { -- | The server that is created by the request.
    server :: Core.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateServerResponse
newCreateServerResponse pHttpStatus_ =
  CreateServerResponse'
    { server = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The server that is created by the request.
createServerResponse_server :: Lens.Lens' CreateServerResponse (Core.Maybe Server)
createServerResponse_server = Lens.lens (\CreateServerResponse' {server} -> server) (\s@CreateServerResponse' {} a -> s {server = a} :: CreateServerResponse)

-- | The response's http status code.
createServerResponse_httpStatus :: Lens.Lens' CreateServerResponse Core.Int
createServerResponse_httpStatus = Lens.lens (\CreateServerResponse' {httpStatus} -> httpStatus) (\s@CreateServerResponse' {} a -> s {httpStatus = a} :: CreateServerResponse)

instance Core.NFData CreateServerResponse
