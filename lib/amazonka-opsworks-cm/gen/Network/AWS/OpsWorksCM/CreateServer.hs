{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.CreateServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and immedately starts a new server. The server is ready to use when it is in the @HEALTHY@ state. By default, you can create a maximum of 10 servers.
--
-- This operation is asynchronous.
-- A @LimitExceededException@ is thrown when you have created the maximum number of servers (10). A @ResourceAlreadyExistsException@ is thrown when a server with the same name already exists in the account. A @ResourceNotFoundException@ is thrown when you specify a backup ID that is not valid or is for a backup that does not exist. A @ValidationException@ is thrown when parameters of the request are not valid.
-- If you do not specify a security group by adding the @SecurityGroupIds@ parameter, AWS OpsWorks creates a new security group.
-- /Chef Automate:/ The default security group opens the Chef server to the world on TCP port 443. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22.
-- /Puppet Enterprise:/ The default security group opens TCP ports 22, 443, 4433, 8140, 8142, 8143, and 8170. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22.
-- By default, your server is accessible from any IP address. We recommend that you update your security group rules to allow access from known IP addresses and address ranges only. To edit security group rules, open Security Groups in the navigation pane of the EC2 management console.
-- To specify your own domain for a server, and provide your own self-signed or CA-signed certificate and private key, specify values for @CustomDomain@ , @CustomCertificate@ , and @CustomPrivateKey@ .
module Network.AWS.OpsWorksCM.CreateServer
  ( -- * Creating a request
    CreateServer (..),
    mkCreateServer,

    -- ** Request lenses
    csEngineVersion,
    csServiceRoleARN,
    csDisableAutomatedBackup,
    csInstanceProfileARN,
    csSecurityGroupIds,
    csAssociatePublicIPAddress,
    csServerName,
    csSubnetIds,
    csKeyPair,
    csBackupId,
    csCustomDomain,
    csEngine,
    csInstanceType,
    csCustomPrivateKey,
    csEngineModel,
    csEngineAttributes,
    csPreferredMaintenanceWindow,
    csPreferredBackupWindow,
    csCustomCertificate,
    csTags,
    csBackupRetentionCount,

    -- * Destructuring the response
    CreateServerResponse (..),
    mkCreateServerResponse,

    -- ** Response lenses
    csrsServer,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateServer' smart constructor.
data CreateServer = CreateServer'
  { -- | The major release version of the engine that you want to use. For a Chef server, the valid value for EngineVersion is currently @2@ . For a Puppet server, the valid value is @2017@ .
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The service role that the AWS OpsWorks CM service backend uses to work with your account. Although the AWS OpsWorks management console typically creates the service role for you, if you are using the AWS CLI or API commands, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the service role and instance profile that you need.
    serviceRoleARN :: Lude.Text,
    -- | Enable or disable scheduled backups. Valid values are @true@ or @false@ . The default value is @true@ .
    disableAutomatedBackup :: Lude.Maybe Lude.Bool,
    -- | The ARN of the instance profile that your Amazon EC2 instances use. Although the AWS OpsWorks console typically creates the instance profile for you, if you are using API commands instead, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the instance profile you need.
    instanceProfileARN :: Lude.Text,
    -- | A list of security group IDs to attach to the Amazon EC2 instance. If you add this parameter, the specified security groups must be within the VPC that is specified by @SubnetIds@ .
    --
    -- If you do not specify this parameter, AWS OpsWorks CM creates one new security group that uses TCP ports 22 and 443, open to 0.0.0.0/0 (everyone).
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | Associate a public IP address with a server that you are launching. Valid values are @true@ or @false@ . The default value is @true@ .
    associatePublicIPAddress :: Lude.Maybe Lude.Bool,
    -- | The name of the server. The server name must be unique within your AWS account, within each region. Server names must start with a letter; then letters, numbers, or hyphens (-) are allowed, up to a maximum of 40 characters.
    serverName :: Lude.Text,
    -- | The IDs of subnets in which to launch the server EC2 instance.
    --
    -- Amazon EC2-Classic customers: This field is required. All servers must run within a VPC. The VPC must have "Auto Assign Public IP" enabled.
    -- EC2-VPC customers: This field is optional. If you do not specify subnet IDs, your EC2 instances are created in a default subnet that is selected by Amazon EC2. If you specify subnet IDs, the VPC must have "Auto Assign Public IP" enabled.
    -- For more information about supported Amazon EC2 platforms, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The Amazon EC2 key pair to set for the instance. This parameter is optional; if desired, you may specify this parameter to connect to your instances by using SSH.
    keyPair :: Lude.Maybe Lude.Text,
    -- | If you specify this field, AWS OpsWorks CM creates the server by using the backup represented by BackupId.
    backupId :: Lude.Maybe Lude.Text,
    -- | An optional public endpoint of a server, such as @https://aws.my-company.com@ . To access the server, create a CNAME DNS record in your preferred DNS service that points the custom domain to the endpoint that is generated when the server is created (the value of the CreateServer Endpoint attribute). You cannot access the server by using the generated @Endpoint@ value if the server is using a custom domain. If you specify a custom domain, you must also specify values for @CustomCertificate@ and @CustomPrivateKey@ .
    customDomain :: Lude.Maybe Lude.Text,
    -- | The configuration management engine to use. Valid values include @ChefAutomate@ and @Puppet@ .
    engine :: Lude.Text,
    -- | The Amazon EC2 instance type to use. For example, @m5.large@ .
    instanceType :: Lude.Text,
    -- | A private key in PEM format for connecting to the server by using HTTPS. The private key must not be encrypted; it cannot be protected by a password or passphrase. If you specify a custom private key, you must also specify values for @CustomDomain@ and @CustomCertificate@ .
    customPrivateKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef.
    engineModel :: Lude.Maybe Lude.Text,
    -- | Optional engine attributes on a specified server.
    --
    -- __Attributes accepted in a Chef createServer request:__
    --
    --     * @CHEF_AUTOMATE_PIVOTAL_KEY@ : A base64-encoded RSA public key. The corresponding private key is required to access the Chef API. When no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and returned in the response.
    --
    --
    --     * @CHEF_AUTOMATE_ADMIN_PASSWORD@ : The password for the administrative user in the Chef Automate web-based dashboard. The password length is a minimum of eight characters, and a maximum of 32. The password can contain letters, numbers, and special characters (!/@#$%^&+=_). The password must contain at least one lower case letter, one upper case letter, one number, and one special character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is generated and returned in the response.
    --
    --
    -- __Attributes accepted in a Puppet createServer request:__
    --
    --     * @PUPPET_ADMIN_PASSWORD@ : To work with the Puppet Enterprise console, a password must use ASCII characters.
    --
    --
    --     * @PUPPET_R10K_REMOTE@ : The r10k remote is the URL of your control repository (for example, ssh://git@your.git-repo.com:user/control-repo.git). Specifying an r10k remote opens TCP port 8170.
    --
    --
    --     * @PUPPET_R10K_PRIVATE_KEY@ : If you are using a private Git repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded private SSH key.
    engineAttributes :: Lude.Maybe [EngineAttribute],
    -- | The start time for a one-hour period each week during which AWS OpsWorks CM performs maintenance on the instance. Valid values must be specified in the following format: @DDD:HH:MM@ . @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random one-hour period on Tuesday, Wednesday, or Friday. See @TimeWindowDefinition@ for more information.
    --
    -- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | The start time for a one-hour period during which AWS OpsWorks CM backs up application-level data on your server if automated backups are enabled. Valid values must be specified in one of the following formats:
    --
    --
    --     * @HH:MM@ for daily backups
    --
    --
    --     * @DDD:HH:MM@ for weekly backups
    --
    --
    -- @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random, daily start time.
    -- __Example:__ @08:00@ , which represents a daily start time of 08:00 UTC.
    -- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    -- | A PEM-formatted HTTPS certificate. The value can be be a single, self-signed certificate, or a certificate chain. If you specify a custom certificate, you must also specify values for @CustomDomain@ and @CustomPrivateKey@ . The following are requirements for the @CustomCertificate@ value:
    --
    --
    --     * You can provide either a self-signed, custom certificate, or the full certificate chain.
    --
    --
    --     * The certificate must be a valid X509 certificate, or a certificate chain in PEM format.
    --
    --
    --     * The certificate must be valid at the time of upload. A certificate can't be used before its validity period begins (the certificate's @NotBefore@ date), or after it expires (the certificate's @NotAfter@ date).
    --
    --
    --     * The certificate’s common name or subject alternative names (SANs), if present, must match the value of @CustomDomain@ .
    --
    --
    --     * The certificate must match the value of @CustomPrivateKey@ .
    customCertificate :: Lude.Maybe Lude.Text,
    -- | A map that contains tag keys and tag values to attach to an AWS OpsWorks for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
    --
    --
    --     * The key cannot be empty.
    --
    --
    --     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
    --
    --
    --     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
    --
    --
    --     * Leading and trailing white spaces are trimmed from both the key and value.
    --
    --
    --     * A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM server.
    tags :: Lude.Maybe [Tag],
    -- | The number of automated backups that you want to keep. Whenever a new backup is created, AWS OpsWorks CM deletes the oldest backups if this number is exceeded. The default value is @1@ .
    backupRetentionCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServer' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The major release version of the engine that you want to use. For a Chef server, the valid value for EngineVersion is currently @2@ . For a Puppet server, the valid value is @2017@ .
-- * 'serviceRoleARN' - The service role that the AWS OpsWorks CM service backend uses to work with your account. Although the AWS OpsWorks management console typically creates the service role for you, if you are using the AWS CLI or API commands, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the service role and instance profile that you need.
-- * 'disableAutomatedBackup' - Enable or disable scheduled backups. Valid values are @true@ or @false@ . The default value is @true@ .
-- * 'instanceProfileARN' - The ARN of the instance profile that your Amazon EC2 instances use. Although the AWS OpsWorks console typically creates the instance profile for you, if you are using API commands instead, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the instance profile you need.
-- * 'securityGroupIds' - A list of security group IDs to attach to the Amazon EC2 instance. If you add this parameter, the specified security groups must be within the VPC that is specified by @SubnetIds@ .
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new security group that uses TCP ports 22 and 443, open to 0.0.0.0/0 (everyone).
-- * 'associatePublicIPAddress' - Associate a public IP address with a server that you are launching. Valid values are @true@ or @false@ . The default value is @true@ .
-- * 'serverName' - The name of the server. The server name must be unique within your AWS account, within each region. Server names must start with a letter; then letters, numbers, or hyphens (-) are allowed, up to a maximum of 40 characters.
-- * 'subnetIds' - The IDs of subnets in which to launch the server EC2 instance.
--
-- Amazon EC2-Classic customers: This field is required. All servers must run within a VPC. The VPC must have "Auto Assign Public IP" enabled.
-- EC2-VPC customers: This field is optional. If you do not specify subnet IDs, your EC2 instances are created in a default subnet that is selected by Amazon EC2. If you specify subnet IDs, the VPC must have "Auto Assign Public IP" enabled.
-- For more information about supported Amazon EC2 platforms, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
-- * 'keyPair' - The Amazon EC2 key pair to set for the instance. This parameter is optional; if desired, you may specify this parameter to connect to your instances by using SSH.
-- * 'backupId' - If you specify this field, AWS OpsWorks CM creates the server by using the backup represented by BackupId.
-- * 'customDomain' - An optional public endpoint of a server, such as @https://aws.my-company.com@ . To access the server, create a CNAME DNS record in your preferred DNS service that points the custom domain to the endpoint that is generated when the server is created (the value of the CreateServer Endpoint attribute). You cannot access the server by using the generated @Endpoint@ value if the server is using a custom domain. If you specify a custom domain, you must also specify values for @CustomCertificate@ and @CustomPrivateKey@ .
-- * 'engine' - The configuration management engine to use. Valid values include @ChefAutomate@ and @Puppet@ .
-- * 'instanceType' - The Amazon EC2 instance type to use. For example, @m5.large@ .
-- * 'customPrivateKey' - A private key in PEM format for connecting to the server by using HTTPS. The private key must not be encrypted; it cannot be protected by a password or passphrase. If you specify a custom private key, you must also specify values for @CustomDomain@ and @CustomCertificate@ .
-- * 'engineModel' - The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef.
-- * 'engineAttributes' - Optional engine attributes on a specified server.
--
-- __Attributes accepted in a Chef createServer request:__
--
--     * @CHEF_AUTOMATE_PIVOTAL_KEY@ : A base64-encoded RSA public key. The corresponding private key is required to access the Chef API. When no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and returned in the response.
--
--
--     * @CHEF_AUTOMATE_ADMIN_PASSWORD@ : The password for the administrative user in the Chef Automate web-based dashboard. The password length is a minimum of eight characters, and a maximum of 32. The password can contain letters, numbers, and special characters (!/@#$%^&+=_). The password must contain at least one lower case letter, one upper case letter, one number, and one special character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is generated and returned in the response.
--
--
-- __Attributes accepted in a Puppet createServer request:__
--
--     * @PUPPET_ADMIN_PASSWORD@ : To work with the Puppet Enterprise console, a password must use ASCII characters.
--
--
--     * @PUPPET_R10K_REMOTE@ : The r10k remote is the URL of your control repository (for example, ssh://git@your.git-repo.com:user/control-repo.git). Specifying an r10k remote opens TCP port 8170.
--
--
--     * @PUPPET_R10K_PRIVATE_KEY@ : If you are using a private Git repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded private SSH key.
--
--
-- * 'preferredMaintenanceWindow' - The start time for a one-hour period each week during which AWS OpsWorks CM performs maintenance on the instance. Valid values must be specified in the following format: @DDD:HH:MM@ . @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random one-hour period on Tuesday, Wednesday, or Friday. See @TimeWindowDefinition@ for more information.
--
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
-- * 'preferredBackupWindow' - The start time for a one-hour period during which AWS OpsWorks CM backs up application-level data on your server if automated backups are enabled. Valid values must be specified in one of the following formats:
--
--
--     * @HH:MM@ for daily backups
--
--
--     * @DDD:HH:MM@ for weekly backups
--
--
-- @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random, daily start time.
-- __Example:__ @08:00@ , which represents a daily start time of 08:00 UTC.
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
-- * 'customCertificate' - A PEM-formatted HTTPS certificate. The value can be be a single, self-signed certificate, or a certificate chain. If you specify a custom certificate, you must also specify values for @CustomDomain@ and @CustomPrivateKey@ . The following are requirements for the @CustomCertificate@ value:
--
--
--     * You can provide either a self-signed, custom certificate, or the full certificate chain.
--
--
--     * The certificate must be a valid X509 certificate, or a certificate chain in PEM format.
--
--
--     * The certificate must be valid at the time of upload. A certificate can't be used before its validity period begins (the certificate's @NotBefore@ date), or after it expires (the certificate's @NotAfter@ date).
--
--
--     * The certificate’s common name or subject alternative names (SANs), if present, must match the value of @CustomDomain@ .
--
--
--     * The certificate must match the value of @CustomPrivateKey@ .
--
--
-- * 'tags' - A map that contains tag keys and tag values to attach to an AWS OpsWorks for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM server.
--
--
-- * 'backupRetentionCount' - The number of automated backups that you want to keep. Whenever a new backup is created, AWS OpsWorks CM deletes the oldest backups if this number is exceeded. The default value is @1@ .
mkCreateServer ::
  -- | 'serviceRoleARN'
  Lude.Text ->
  -- | 'instanceProfileARN'
  Lude.Text ->
  -- | 'serverName'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'instanceType'
  Lude.Text ->
  CreateServer
mkCreateServer
  pServiceRoleARN_
  pInstanceProfileARN_
  pServerName_
  pEngine_
  pInstanceType_ =
    CreateServer'
      { engineVersion = Lude.Nothing,
        serviceRoleARN = pServiceRoleARN_,
        disableAutomatedBackup = Lude.Nothing,
        instanceProfileARN = pInstanceProfileARN_,
        securityGroupIds = Lude.Nothing,
        associatePublicIPAddress = Lude.Nothing,
        serverName = pServerName_,
        subnetIds = Lude.Nothing,
        keyPair = Lude.Nothing,
        backupId = Lude.Nothing,
        customDomain = Lude.Nothing,
        engine = pEngine_,
        instanceType = pInstanceType_,
        customPrivateKey = Lude.Nothing,
        engineModel = Lude.Nothing,
        engineAttributes = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        preferredBackupWindow = Lude.Nothing,
        customCertificate = Lude.Nothing,
        tags = Lude.Nothing,
        backupRetentionCount = Lude.Nothing
      }

-- | The major release version of the engine that you want to use. For a Chef server, the valid value for EngineVersion is currently @2@ . For a Puppet server, the valid value is @2017@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineVersion :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csEngineVersion = Lens.lens (engineVersion :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateServer)
{-# DEPRECATED csEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The service role that the AWS OpsWorks CM service backend uses to work with your account. Although the AWS OpsWorks management console typically creates the service role for you, if you are using the AWS CLI or API commands, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the service role and instance profile that you need.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRoleARN :: Lens.Lens' CreateServer Lude.Text
csServiceRoleARN = Lens.lens (serviceRoleARN :: CreateServer -> Lude.Text) (\s a -> s {serviceRoleARN = a} :: CreateServer)
{-# DEPRECATED csServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Enable or disable scheduled backups. Valid values are @true@ or @false@ . The default value is @true@ .
--
-- /Note:/ Consider using 'disableAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisableAutomatedBackup :: Lens.Lens' CreateServer (Lude.Maybe Lude.Bool)
csDisableAutomatedBackup = Lens.lens (disableAutomatedBackup :: CreateServer -> Lude.Maybe Lude.Bool) (\s a -> s {disableAutomatedBackup = a} :: CreateServer)
{-# DEPRECATED csDisableAutomatedBackup "Use generic-lens or generic-optics with 'disableAutomatedBackup' instead." #-}

-- | The ARN of the instance profile that your Amazon EC2 instances use. Although the AWS OpsWorks console typically creates the instance profile for you, if you are using API commands instead, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the instance profile you need.
--
-- /Note:/ Consider using 'instanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceProfileARN :: Lens.Lens' CreateServer Lude.Text
csInstanceProfileARN = Lens.lens (instanceProfileARN :: CreateServer -> Lude.Text) (\s a -> s {instanceProfileARN = a} :: CreateServer)
{-# DEPRECATED csInstanceProfileARN "Use generic-lens or generic-optics with 'instanceProfileARN' instead." #-}

-- | A list of security group IDs to attach to the Amazon EC2 instance. If you add this parameter, the specified security groups must be within the VPC that is specified by @SubnetIds@ .
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new security group that uses TCP ports 22 and 443, open to 0.0.0.0/0 (everyone).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSecurityGroupIds :: Lens.Lens' CreateServer (Lude.Maybe [Lude.Text])
csSecurityGroupIds = Lens.lens (securityGroupIds :: CreateServer -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateServer)
{-# DEPRECATED csSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Associate a public IP address with a server that you are launching. Valid values are @true@ or @false@ . The default value is @true@ .
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAssociatePublicIPAddress :: Lens.Lens' CreateServer (Lude.Maybe Lude.Bool)
csAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: CreateServer -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: CreateServer)
{-# DEPRECATED csAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The name of the server. The server name must be unique within your AWS account, within each region. Server names must start with a letter; then letters, numbers, or hyphens (-) are allowed, up to a maximum of 40 characters.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServerName :: Lens.Lens' CreateServer Lude.Text
csServerName = Lens.lens (serverName :: CreateServer -> Lude.Text) (\s a -> s {serverName = a} :: CreateServer)
{-# DEPRECATED csServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The IDs of subnets in which to launch the server EC2 instance.
--
-- Amazon EC2-Classic customers: This field is required. All servers must run within a VPC. The VPC must have "Auto Assign Public IP" enabled.
-- EC2-VPC customers: This field is optional. If you do not specify subnet IDs, your EC2 instances are created in a default subnet that is selected by Amazon EC2. If you specify subnet IDs, the VPC must have "Auto Assign Public IP" enabled.
-- For more information about supported Amazon EC2 platforms, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubnetIds :: Lens.Lens' CreateServer (Lude.Maybe [Lude.Text])
csSubnetIds = Lens.lens (subnetIds :: CreateServer -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateServer)
{-# DEPRECATED csSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The Amazon EC2 key pair to set for the instance. This parameter is optional; if desired, you may specify this parameter to connect to your instances by using SSH.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKeyPair :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csKeyPair = Lens.lens (keyPair :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {keyPair = a} :: CreateServer)
{-# DEPRECATED csKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | If you specify this field, AWS OpsWorks CM creates the server by using the backup represented by BackupId.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBackupId :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csBackupId = Lens.lens (backupId :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {backupId = a} :: CreateServer)
{-# DEPRECATED csBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | An optional public endpoint of a server, such as @https://aws.my-company.com@ . To access the server, create a CNAME DNS record in your preferred DNS service that points the custom domain to the endpoint that is generated when the server is created (the value of the CreateServer Endpoint attribute). You cannot access the server by using the generated @Endpoint@ value if the server is using a custom domain. If you specify a custom domain, you must also specify values for @CustomCertificate@ and @CustomPrivateKey@ .
--
-- /Note:/ Consider using 'customDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomDomain :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csCustomDomain = Lens.lens (customDomain :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {customDomain = a} :: CreateServer)
{-# DEPRECATED csCustomDomain "Use generic-lens or generic-optics with 'customDomain' instead." #-}

-- | The configuration management engine to use. Valid values include @ChefAutomate@ and @Puppet@ .
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngine :: Lens.Lens' CreateServer Lude.Text
csEngine = Lens.lens (engine :: CreateServer -> Lude.Text) (\s a -> s {engine = a} :: CreateServer)
{-# DEPRECATED csEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The Amazon EC2 instance type to use. For example, @m5.large@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceType :: Lens.Lens' CreateServer Lude.Text
csInstanceType = Lens.lens (instanceType :: CreateServer -> Lude.Text) (\s a -> s {instanceType = a} :: CreateServer)
{-# DEPRECATED csInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A private key in PEM format for connecting to the server by using HTTPS. The private key must not be encrypted; it cannot be protected by a password or passphrase. If you specify a custom private key, you must also specify values for @CustomDomain@ and @CustomCertificate@ .
--
-- /Note:/ Consider using 'customPrivateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomPrivateKey :: Lens.Lens' CreateServer (Lude.Maybe (Lude.Sensitive Lude.Text))
csCustomPrivateKey = Lens.lens (customPrivateKey :: CreateServer -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {customPrivateKey = a} :: CreateServer)
{-# DEPRECATED csCustomPrivateKey "Use generic-lens or generic-optics with 'customPrivateKey' instead." #-}

-- | The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef.
--
-- /Note:/ Consider using 'engineModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineModel :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csEngineModel = Lens.lens (engineModel :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {engineModel = a} :: CreateServer)
{-# DEPRECATED csEngineModel "Use generic-lens or generic-optics with 'engineModel' instead." #-}

-- | Optional engine attributes on a specified server.
--
-- __Attributes accepted in a Chef createServer request:__
--
--     * @CHEF_AUTOMATE_PIVOTAL_KEY@ : A base64-encoded RSA public key. The corresponding private key is required to access the Chef API. When no CHEF_AUTOMATE_PIVOTAL_KEY is set, a private key is generated and returned in the response.
--
--
--     * @CHEF_AUTOMATE_ADMIN_PASSWORD@ : The password for the administrative user in the Chef Automate web-based dashboard. The password length is a minimum of eight characters, and a maximum of 32. The password can contain letters, numbers, and special characters (!/@#$%^&+=_). The password must contain at least one lower case letter, one upper case letter, one number, and one special character. When no CHEF_AUTOMATE_ADMIN_PASSWORD is set, one is generated and returned in the response.
--
--
-- __Attributes accepted in a Puppet createServer request:__
--
--     * @PUPPET_ADMIN_PASSWORD@ : To work with the Puppet Enterprise console, a password must use ASCII characters.
--
--
--     * @PUPPET_R10K_REMOTE@ : The r10k remote is the URL of your control repository (for example, ssh://git@your.git-repo.com:user/control-repo.git). Specifying an r10k remote opens TCP port 8170.
--
--
--     * @PUPPET_R10K_PRIVATE_KEY@ : If you are using a private Git repository, add PUPPET_R10K_PRIVATE_KEY to specify a PEM-encoded private SSH key.
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineAttributes :: Lens.Lens' CreateServer (Lude.Maybe [EngineAttribute])
csEngineAttributes = Lens.lens (engineAttributes :: CreateServer -> Lude.Maybe [EngineAttribute]) (\s a -> s {engineAttributes = a} :: CreateServer)
{-# DEPRECATED csEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

-- | The start time for a one-hour period each week during which AWS OpsWorks CM performs maintenance on the instance. Valid values must be specified in the following format: @DDD:HH:MM@ . @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random one-hour period on Tuesday, Wednesday, or Friday. See @TimeWindowDefinition@ for more information.
--
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredMaintenanceWindow :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateServer)
{-# DEPRECATED csPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The start time for a one-hour period during which AWS OpsWorks CM backs up application-level data on your server if automated backups are enabled. Valid values must be specified in one of the following formats:
--
--
--     * @HH:MM@ for daily backups
--
--
--     * @DDD:HH:MM@ for weekly backups
--
--
-- @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random, daily start time.
-- __Example:__ @08:00@ , which represents a daily start time of 08:00 UTC.
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.)
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredBackupWindow :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csPreferredBackupWindow = Lens.lens (preferredBackupWindow :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: CreateServer)
{-# DEPRECATED csPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | A PEM-formatted HTTPS certificate. The value can be be a single, self-signed certificate, or a certificate chain. If you specify a custom certificate, you must also specify values for @CustomDomain@ and @CustomPrivateKey@ . The following are requirements for the @CustomCertificate@ value:
--
--
--     * You can provide either a self-signed, custom certificate, or the full certificate chain.
--
--
--     * The certificate must be a valid X509 certificate, or a certificate chain in PEM format.
--
--
--     * The certificate must be valid at the time of upload. A certificate can't be used before its validity period begins (the certificate's @NotBefore@ date), or after it expires (the certificate's @NotAfter@ date).
--
--
--     * The certificate’s common name or subject alternative names (SANs), if present, must match the value of @CustomDomain@ .
--
--
--     * The certificate must match the value of @CustomPrivateKey@ .
--
--
--
-- /Note:/ Consider using 'customCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomCertificate :: Lens.Lens' CreateServer (Lude.Maybe Lude.Text)
csCustomCertificate = Lens.lens (customCertificate :: CreateServer -> Lude.Maybe Lude.Text) (\s a -> s {customCertificate = a} :: CreateServer)
{-# DEPRECATED csCustomCertificate "Use generic-lens or generic-optics with 'customCertificate' instead." #-}

-- | A map that contains tag keys and tag values to attach to an AWS OpsWorks for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : / @@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM server.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateServer (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateServer -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateServer)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of automated backups that you want to keep. Whenever a new backup is created, AWS OpsWorks CM deletes the oldest backups if this number is exceeded. The default value is @1@ .
--
-- /Note:/ Consider using 'backupRetentionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBackupRetentionCount :: Lens.Lens' CreateServer (Lude.Maybe Lude.Natural)
csBackupRetentionCount = Lens.lens (backupRetentionCount :: CreateServer -> Lude.Maybe Lude.Natural) (\s a -> s {backupRetentionCount = a} :: CreateServer)
{-# DEPRECATED csBackupRetentionCount "Use generic-lens or generic-optics with 'backupRetentionCount' instead." #-}

instance Lude.AWSRequest CreateServer where
  type Rs CreateServer = CreateServerResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateServerResponse'
            Lude.<$> (x Lude..?> "Server") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.CreateServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateServer where
  toJSON CreateServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EngineVersion" Lude..=) Lude.<$> engineVersion,
            Lude.Just ("ServiceRoleArn" Lude..= serviceRoleARN),
            ("DisableAutomatedBackup" Lude..=) Lude.<$> disableAutomatedBackup,
            Lude.Just ("InstanceProfileArn" Lude..= instanceProfileARN),
            ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("AssociatePublicIpAddress" Lude..=)
              Lude.<$> associatePublicIPAddress,
            Lude.Just ("ServerName" Lude..= serverName),
            ("SubnetIds" Lude..=) Lude.<$> subnetIds,
            ("KeyPair" Lude..=) Lude.<$> keyPair,
            ("BackupId" Lude..=) Lude.<$> backupId,
            ("CustomDomain" Lude..=) Lude.<$> customDomain,
            Lude.Just ("Engine" Lude..= engine),
            Lude.Just ("InstanceType" Lude..= instanceType),
            ("CustomPrivateKey" Lude..=) Lude.<$> customPrivateKey,
            ("EngineModel" Lude..=) Lude.<$> engineModel,
            ("EngineAttributes" Lude..=) Lude.<$> engineAttributes,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("PreferredBackupWindow" Lude..=) Lude.<$> preferredBackupWindow,
            ("CustomCertificate" Lude..=) Lude.<$> customCertificate,
            ("Tags" Lude..=) Lude.<$> tags,
            ("BackupRetentionCount" Lude..=) Lude.<$> backupRetentionCount
          ]
      )

instance Lude.ToPath CreateServer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { -- | The server that is created by the request.
    server :: Lude.Maybe Server,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServerResponse' with the minimum fields required to make a request.
--
-- * 'server' - The server that is created by the request.
-- * 'responseStatus' - The response status code.
mkCreateServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateServerResponse
mkCreateServerResponse pResponseStatus_ =
  CreateServerResponse'
    { server = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The server that is created by the request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsServer :: Lens.Lens' CreateServerResponse (Lude.Maybe Server)
csrsServer = Lens.lens (server :: CreateServerResponse -> Lude.Maybe Server) (\s a -> s {server = a} :: CreateServerResponse)
{-# DEPRECATED csrsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateServerResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateServerResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
