{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateServer (..)
    , mkCreateServer
    -- ** Request lenses
    , csEngine
    , csServerName
    , csInstanceProfileArn
    , csInstanceType
    , csServiceRoleArn
    , csAssociatePublicIpAddress
    , csBackupId
    , csBackupRetentionCount
    , csCustomCertificate
    , csCustomDomain
    , csCustomPrivateKey
    , csDisableAutomatedBackup
    , csEngineAttributes
    , csEngineModel
    , csEngineVersion
    , csKeyPair
    , csPreferredBackupWindow
    , csPreferredMaintenanceWindow
    , csSecurityGroupIds
    , csSubnetIds
    , csTags

    -- * Destructuring the response
    , CreateServerResponse (..)
    , mkCreateServerResponse
    -- ** Response lenses
    , csrrsServer
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateServer' smart constructor.
data CreateServer = CreateServer'
  { engine :: Core.Text
    -- ^ The configuration management engine to use. Valid values include @ChefAutomate@ and @Puppet@ . 
  , serverName :: Types.ServerName
    -- ^ The name of the server. The server name must be unique within your AWS account, within each region. Server names must start with a letter; then letters, numbers, or hyphens (-) are allowed, up to a maximum of 40 characters. 
  , instanceProfileArn :: Types.InstanceProfileArn
    -- ^ The ARN of the instance profile that your Amazon EC2 instances use. Although the AWS OpsWorks console typically creates the instance profile for you, if you are using API commands instead, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the instance profile you need. 
  , instanceType :: Core.Text
    -- ^ The Amazon EC2 instance type to use. For example, @m5.large@ . 
  , serviceRoleArn :: Types.ServiceRoleArn
    -- ^ The service role that the AWS OpsWorks CM service backend uses to work with your account. Although the AWS OpsWorks management console typically creates the service role for you, if you are using the AWS CLI or API commands, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the service role and instance profile that you need. 
  , associatePublicIpAddress :: Core.Maybe Core.Bool
    -- ^ Associate a public IP address with a server that you are launching. Valid values are @true@ or @false@ . The default value is @true@ . 
  , backupId :: Core.Maybe Types.BackupId
    -- ^ If you specify this field, AWS OpsWorks CM creates the server by using the backup represented by BackupId. 
  , backupRetentionCount :: Core.Maybe Core.Natural
    -- ^ The number of automated backups that you want to keep. Whenever a new backup is created, AWS OpsWorks CM deletes the oldest backups if this number is exceeded. The default value is @1@ . 
  , customCertificate :: Core.Maybe Types.CustomCertificate
    -- ^ A PEM-formatted HTTPS certificate. The value can be be a single, self-signed certificate, or a certificate chain. If you specify a custom certificate, you must also specify values for @CustomDomain@ and @CustomPrivateKey@ . The following are requirements for the @CustomCertificate@ value:
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
  , customDomain :: Core.Maybe Types.CustomDomain
    -- ^ An optional public endpoint of a server, such as @https://aws.my-company.com@ . To access the server, create a CNAME DNS record in your preferred DNS service that points the custom domain to the endpoint that is generated when the server is created (the value of the CreateServer Endpoint attribute). You cannot access the server by using the generated @Endpoint@ value if the server is using a custom domain. If you specify a custom domain, you must also specify values for @CustomCertificate@ and @CustomPrivateKey@ .
  , customPrivateKey :: Core.Maybe Types.CustomPrivateKey
    -- ^ A private key in PEM format for connecting to the server by using HTTPS. The private key must not be encrypted; it cannot be protected by a password or passphrase. If you specify a custom private key, you must also specify values for @CustomDomain@ and @CustomCertificate@ .
  , disableAutomatedBackup :: Core.Maybe Core.Bool
    -- ^ Enable or disable scheduled backups. Valid values are @true@ or @false@ . The default value is @true@ . 
  , engineAttributes :: Core.Maybe [Types.EngineAttribute]
    -- ^ Optional engine attributes on a specified server. 
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
  , engineModel :: Core.Maybe Core.Text
    -- ^ The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef. 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The major release version of the engine that you want to use. For a Chef server, the valid value for EngineVersion is currently @2@ . For a Puppet server, the valid value is @2017@ . 
  , keyPair :: Core.Maybe Types.KeyPair
    -- ^ The Amazon EC2 key pair to set for the instance. This parameter is optional; if desired, you may specify this parameter to connect to your instances by using SSH. 
  , preferredBackupWindow :: Core.Maybe Types.TimeWindowDefinition
    -- ^ The start time for a one-hour period during which AWS OpsWorks CM backs up application-level data on your server if automated backups are enabled. Valid values must be specified in one of the following formats: 
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
  , preferredMaintenanceWindow :: Core.Maybe Types.TimeWindowDefinition
    -- ^ The start time for a one-hour period each week during which AWS OpsWorks CM performs maintenance on the instance. Valid values must be specified in the following format: @DDD:HH:MM@ . @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random one-hour period on Tuesday, Wednesday, or Friday. See @TimeWindowDefinition@ for more information. 
--
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.) 
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of security group IDs to attach to the Amazon EC2 instance. If you add this parameter, the specified security groups must be within the VPC that is specified by @SubnetIds@ . 
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new security group that uses TCP ports 22 and 443, open to 0.0.0.0/0 (everyone). 
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of subnets in which to launch the server EC2 instance. 
--
-- Amazon EC2-Classic customers: This field is required. All servers must run within a VPC. The VPC must have "Auto Assign Public IP" enabled. 
-- EC2-VPC customers: This field is optional. If you do not specify subnet IDs, your EC2 instances are created in a default subnet that is selected by Amazon EC2. If you specify subnet IDs, the VPC must have "Auto Assign Public IP" enabled. 
-- For more information about supported Amazon EC2 platforms, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A map that contains tag keys and tag values to attach to an AWS OpsWorks for Chef Automate or AWS OpsWorks for Puppet Enterprise server.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateServer' value with any optional fields omitted.
mkCreateServer
    :: Core.Text -- ^ 'engine'
    -> Types.ServerName -- ^ 'serverName'
    -> Types.InstanceProfileArn -- ^ 'instanceProfileArn'
    -> Core.Text -- ^ 'instanceType'
    -> Types.ServiceRoleArn -- ^ 'serviceRoleArn'
    -> CreateServer
mkCreateServer engine serverName instanceProfileArn instanceType
  serviceRoleArn
  = CreateServer'{engine, serverName, instanceProfileArn,
                  instanceType, serviceRoleArn,
                  associatePublicIpAddress = Core.Nothing, backupId = Core.Nothing,
                  backupRetentionCount = Core.Nothing,
                  customCertificate = Core.Nothing, customDomain = Core.Nothing,
                  customPrivateKey = Core.Nothing,
                  disableAutomatedBackup = Core.Nothing,
                  engineAttributes = Core.Nothing, engineModel = Core.Nothing,
                  engineVersion = Core.Nothing, keyPair = Core.Nothing,
                  preferredBackupWindow = Core.Nothing,
                  preferredMaintenanceWindow = Core.Nothing,
                  securityGroupIds = Core.Nothing, subnetIds = Core.Nothing,
                  tags = Core.Nothing}

-- | The configuration management engine to use. Valid values include @ChefAutomate@ and @Puppet@ . 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngine :: Lens.Lens' CreateServer Core.Text
csEngine = Lens.field @"engine"
{-# INLINEABLE csEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The name of the server. The server name must be unique within your AWS account, within each region. Server names must start with a letter; then letters, numbers, or hyphens (-) are allowed, up to a maximum of 40 characters. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServerName :: Lens.Lens' CreateServer Types.ServerName
csServerName = Lens.field @"serverName"
{-# INLINEABLE csServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The ARN of the instance profile that your Amazon EC2 instances use. Although the AWS OpsWorks console typically creates the instance profile for you, if you are using API commands instead, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the instance profile you need. 
--
-- /Note:/ Consider using 'instanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceProfileArn :: Lens.Lens' CreateServer Types.InstanceProfileArn
csInstanceProfileArn = Lens.field @"instanceProfileArn"
{-# INLINEABLE csInstanceProfileArn #-}
{-# DEPRECATED instanceProfileArn "Use generic-lens or generic-optics with 'instanceProfileArn' instead"  #-}

-- | The Amazon EC2 instance type to use. For example, @m5.large@ . 
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceType :: Lens.Lens' CreateServer Core.Text
csInstanceType = Lens.field @"instanceType"
{-# INLINEABLE csInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The service role that the AWS OpsWorks CM service backend uses to work with your account. Although the AWS OpsWorks management console typically creates the service role for you, if you are using the AWS CLI or API commands, run the service-role-creation.yaml AWS CloudFormation template, located at https://s3.amazonaws.com/opsworks-cm-us-east-1-prod-default-assets/misc/opsworks-cm-roles.yaml. This template creates a CloudFormation stack that includes the service role and instance profile that you need. 
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRoleArn :: Lens.Lens' CreateServer Types.ServiceRoleArn
csServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE csServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | Associate a public IP address with a server that you are launching. Valid values are @true@ or @false@ . The default value is @true@ . 
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAssociatePublicIpAddress :: Lens.Lens' CreateServer (Core.Maybe Core.Bool)
csAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# INLINEABLE csAssociatePublicIpAddress #-}
{-# DEPRECATED associatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead"  #-}

-- | If you specify this field, AWS OpsWorks CM creates the server by using the backup represented by BackupId. 
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBackupId :: Lens.Lens' CreateServer (Core.Maybe Types.BackupId)
csBackupId = Lens.field @"backupId"
{-# INLINEABLE csBackupId #-}
{-# DEPRECATED backupId "Use generic-lens or generic-optics with 'backupId' instead"  #-}

-- | The number of automated backups that you want to keep. Whenever a new backup is created, AWS OpsWorks CM deletes the oldest backups if this number is exceeded. The default value is @1@ . 
--
-- /Note:/ Consider using 'backupRetentionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBackupRetentionCount :: Lens.Lens' CreateServer (Core.Maybe Core.Natural)
csBackupRetentionCount = Lens.field @"backupRetentionCount"
{-# INLINEABLE csBackupRetentionCount #-}
{-# DEPRECATED backupRetentionCount "Use generic-lens or generic-optics with 'backupRetentionCount' instead"  #-}

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
csCustomCertificate :: Lens.Lens' CreateServer (Core.Maybe Types.CustomCertificate)
csCustomCertificate = Lens.field @"customCertificate"
{-# INLINEABLE csCustomCertificate #-}
{-# DEPRECATED customCertificate "Use generic-lens or generic-optics with 'customCertificate' instead"  #-}

-- | An optional public endpoint of a server, such as @https://aws.my-company.com@ . To access the server, create a CNAME DNS record in your preferred DNS service that points the custom domain to the endpoint that is generated when the server is created (the value of the CreateServer Endpoint attribute). You cannot access the server by using the generated @Endpoint@ value if the server is using a custom domain. If you specify a custom domain, you must also specify values for @CustomCertificate@ and @CustomPrivateKey@ .
--
-- /Note:/ Consider using 'customDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomDomain :: Lens.Lens' CreateServer (Core.Maybe Types.CustomDomain)
csCustomDomain = Lens.field @"customDomain"
{-# INLINEABLE csCustomDomain #-}
{-# DEPRECATED customDomain "Use generic-lens or generic-optics with 'customDomain' instead"  #-}

-- | A private key in PEM format for connecting to the server by using HTTPS. The private key must not be encrypted; it cannot be protected by a password or passphrase. If you specify a custom private key, you must also specify values for @CustomDomain@ and @CustomCertificate@ .
--
-- /Note:/ Consider using 'customPrivateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomPrivateKey :: Lens.Lens' CreateServer (Core.Maybe Types.CustomPrivateKey)
csCustomPrivateKey = Lens.field @"customPrivateKey"
{-# INLINEABLE csCustomPrivateKey #-}
{-# DEPRECATED customPrivateKey "Use generic-lens or generic-optics with 'customPrivateKey' instead"  #-}

-- | Enable or disable scheduled backups. Valid values are @true@ or @false@ . The default value is @true@ . 
--
-- /Note:/ Consider using 'disableAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisableAutomatedBackup :: Lens.Lens' CreateServer (Core.Maybe Core.Bool)
csDisableAutomatedBackup = Lens.field @"disableAutomatedBackup"
{-# INLINEABLE csDisableAutomatedBackup #-}
{-# DEPRECATED disableAutomatedBackup "Use generic-lens or generic-optics with 'disableAutomatedBackup' instead"  #-}

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
csEngineAttributes :: Lens.Lens' CreateServer (Core.Maybe [Types.EngineAttribute])
csEngineAttributes = Lens.field @"engineAttributes"
{-# INLINEABLE csEngineAttributes #-}
{-# DEPRECATED engineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead"  #-}

-- | The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef. 
--
-- /Note:/ Consider using 'engineModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineModel :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
csEngineModel = Lens.field @"engineModel"
{-# INLINEABLE csEngineModel #-}
{-# DEPRECATED engineModel "Use generic-lens or generic-optics with 'engineModel' instead"  #-}

-- | The major release version of the engine that you want to use. For a Chef server, the valid value for EngineVersion is currently @2@ . For a Puppet server, the valid value is @2017@ . 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineVersion :: Lens.Lens' CreateServer (Core.Maybe Core.Text)
csEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE csEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The Amazon EC2 key pair to set for the instance. This parameter is optional; if desired, you may specify this parameter to connect to your instances by using SSH. 
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKeyPair :: Lens.Lens' CreateServer (Core.Maybe Types.KeyPair)
csKeyPair = Lens.field @"keyPair"
{-# INLINEABLE csKeyPair #-}
{-# DEPRECATED keyPair "Use generic-lens or generic-optics with 'keyPair' instead"  #-}

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
csPreferredBackupWindow :: Lens.Lens' CreateServer (Core.Maybe Types.TimeWindowDefinition)
csPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE csPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

-- | The start time for a one-hour period each week during which AWS OpsWorks CM performs maintenance on the instance. Valid values must be specified in the following format: @DDD:HH:MM@ . @MM@ must be specified as @00@ . The specified time is in coordinated universal time (UTC). The default value is a random one-hour period on Tuesday, Wednesday, or Friday. See @TimeWindowDefinition@ for more information. 
--
-- __Example:__ @Mon:08:00@ , which represents a start time of every Monday at 08:00 UTC. (8:00 a.m.) 
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredMaintenanceWindow :: Lens.Lens' CreateServer (Core.Maybe Types.TimeWindowDefinition)
csPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE csPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | A list of security group IDs to attach to the Amazon EC2 instance. If you add this parameter, the specified security groups must be within the VPC that is specified by @SubnetIds@ . 
--
-- If you do not specify this parameter, AWS OpsWorks CM creates one new security group that uses TCP ports 22 and 443, open to 0.0.0.0/0 (everyone). 
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSecurityGroupIds :: Lens.Lens' CreateServer (Core.Maybe [Core.Text])
csSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE csSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The IDs of subnets in which to launch the server EC2 instance. 
--
-- Amazon EC2-Classic customers: This field is required. All servers must run within a VPC. The VPC must have "Auto Assign Public IP" enabled. 
-- EC2-VPC customers: This field is optional. If you do not specify subnet IDs, your EC2 instances are created in a default subnet that is selected by Amazon EC2. If you specify subnet IDs, the VPC must have "Auto Assign Public IP" enabled. 
-- For more information about supported Amazon EC2 platforms, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubnetIds :: Lens.Lens' CreateServer (Core.Maybe [Core.Text])
csSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE csSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

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
csTags :: Lens.Lens' CreateServer (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# INLINEABLE csTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateServer where
        toHeaders CreateServer{..}
          = Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.CreateServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateServer where
        toJSON CreateServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Engine" Core..= engine),
                  Core.Just ("ServerName" Core..= serverName),
                  Core.Just ("InstanceProfileArn" Core..= instanceProfileArn),
                  Core.Just ("InstanceType" Core..= instanceType),
                  Core.Just ("ServiceRoleArn" Core..= serviceRoleArn),
                  ("AssociatePublicIpAddress" Core..=) Core.<$>
                    associatePublicIpAddress,
                  ("BackupId" Core..=) Core.<$> backupId,
                  ("BackupRetentionCount" Core..=) Core.<$> backupRetentionCount,
                  ("CustomCertificate" Core..=) Core.<$> customCertificate,
                  ("CustomDomain" Core..=) Core.<$> customDomain,
                  ("CustomPrivateKey" Core..=) Core.<$> customPrivateKey,
                  ("DisableAutomatedBackup" Core..=) Core.<$> disableAutomatedBackup,
                  ("EngineAttributes" Core..=) Core.<$> engineAttributes,
                  ("EngineModel" Core..=) Core.<$> engineModel,
                  ("EngineVersion" Core..=) Core.<$> engineVersion,
                  ("KeyPair" Core..=) Core.<$> keyPair,
                  ("PreferredBackupWindow" Core..=) Core.<$> preferredBackupWindow,
                  ("PreferredMaintenanceWindow" Core..=) Core.<$>
                    preferredMaintenanceWindow,
                  ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("SubnetIds" Core..=) Core.<$> subnetIds,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateServer where
        type Rs CreateServer = CreateServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateServerResponse' Core.<$>
                   (x Core..:? "Server") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateServerResponse' smart constructor.
data CreateServerResponse = CreateServerResponse'
  { server :: Core.Maybe Types.Server
    -- ^ The server that is created by the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateServerResponse' value with any optional fields omitted.
mkCreateServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateServerResponse
mkCreateServerResponse responseStatus
  = CreateServerResponse'{server = Core.Nothing, responseStatus}

-- | The server that is created by the request. 
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsServer :: Lens.Lens' CreateServerResponse (Core.Maybe Types.Server)
csrrsServer = Lens.field @"server"
{-# INLINEABLE csrrsServer #-}
{-# DEPRECATED server "Use generic-lens or generic-optics with 'server' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateServerResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
