{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerService
  ( ContainerService (..),

    -- * Smart constructor
    mkContainerService,

    -- * Lenses
    csArn,
    csContainerServiceName,
    csCreatedAt,
    csCurrentDeployment,
    csIsDisabled,
    csLocation,
    csNextDeployment,
    csPower,
    csPowerId,
    csPrincipalArn,
    csPrivateDomainName,
    csPublicDomainNames,
    csResourceType,
    csScale,
    csState,
    csTags,
    csUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.ContainerServiceDeployment as Types
import qualified Network.AWS.Lightsail.Types.ContainerServiceName as Types
import qualified Network.AWS.Lightsail.Types.ContainerServicePowerName as Types
import qualified Network.AWS.Lightsail.Types.ContainerServiceState as Types
import qualified Network.AWS.Lightsail.Types.PowerId as Types
import qualified Network.AWS.Lightsail.Types.PrincipalArn as Types
import qualified Network.AWS.Lightsail.Types.PrivateDomainName as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Lightsail.Types.Url as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerService' smart constructor.
data ContainerService = ContainerService'
  { -- | The Amazon Resource Name (ARN) of the container service.
    arn :: Core.Maybe Types.Arn,
    -- | The name of the container service.
    containerServiceName :: Core.Maybe Types.ContainerServiceName,
    -- | The timestamp when the container service was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | An object that describes the current container deployment of the container service.
    currentDeployment :: Core.Maybe Types.ContainerServiceDeployment,
    -- | A Boolean value indicating whether the container service is disabled.
    isDisabled :: Core.Maybe Core.Bool,
    -- | An object that describes the location of the container service, such as the AWS Region and Availability Zone.
    location :: Core.Maybe Types.ResourceLocation,
    -- | An object that describes the next deployment of the container service.
    --
    -- This value is @null@ when there is no deployment in a @pending@ state.
    nextDeployment :: Core.Maybe Types.ContainerServiceDeployment,
    -- | The power specification of the container service.
    --
    -- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
    power :: Core.Maybe Types.ContainerServicePowerName,
    -- | The ID of the power of the container service.
    powerId :: Core.Maybe Types.PowerId,
    -- | The principal ARN of the container service.
    --
    -- The principal ARN can be used to create a trust relationship between your standard AWS account and your Lightsail container service. This allows you to give your service permission to access resources in your standard AWS account.
    principalArn :: Core.Maybe Types.PrincipalArn,
    -- | The private domain name of the container service.
    --
    -- The private domain name is accessible only by other resources within the default virtual private cloud (VPC) of your Lightsail account.
    privateDomainName :: Core.Maybe Types.PrivateDomainName,
    -- | The public domain name of the container service, such as @example.com@ and @www.example.com@ .
    --
    -- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
    -- If you don't specify public domain names, then you can use the default domain of the container service.
    -- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
    -- See @CreateContainerService@ or @UpdateContainerService@ for information about how to specify public domain names for your Lightsail container service.
    publicDomainNames :: Core.Maybe (Core.HashMap Types.String [Types.String]),
    -- | The Lightsail resource type of the container service (i.e., @ContainerService@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The scale specification of the container service.
    --
    -- The scale specifies the allocated compute nodes of the container service.
    scale :: Core.Maybe Core.Natural,
    -- | The current state of the container service.
    --
    -- The state can be:
    --
    --     * @Pending@ - The container service is being created.
    --
    --
    --     * @Ready@ - The container service is created but does not have a container deployment.
    --
    --
    --     * @Disabled@ - The container service is disabled.
    --
    --
    --     * @Updating@ - The container service capacity or other setting is being updated.
    --
    --
    --     * @Deploying@ - The container service is launching a container deployment.
    --
    --
    --     * @Running@ - The container service is created and it has a container deployment.
    state :: Core.Maybe Types.ContainerServiceState,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag],
    -- | The publicly accessible URL of the container service.
    --
    -- If no public endpoint is specified in the @currentDeployment@ , this URL returns a 404 response.
    url :: Core.Maybe Types.Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ContainerService' value with any optional fields omitted.
mkContainerService ::
  ContainerService
mkContainerService =
  ContainerService'
    { arn = Core.Nothing,
      containerServiceName = Core.Nothing,
      createdAt = Core.Nothing,
      currentDeployment = Core.Nothing,
      isDisabled = Core.Nothing,
      location = Core.Nothing,
      nextDeployment = Core.Nothing,
      power = Core.Nothing,
      powerId = Core.Nothing,
      principalArn = Core.Nothing,
      privateDomainName = Core.Nothing,
      publicDomainNames = Core.Nothing,
      resourceType = Core.Nothing,
      scale = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      url = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container service.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csArn :: Lens.Lens' ContainerService (Core.Maybe Types.Arn)
csArn = Lens.field @"arn"
{-# DEPRECATED csArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the container service.
--
-- /Note:/ Consider using 'containerServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csContainerServiceName :: Lens.Lens' ContainerService (Core.Maybe Types.ContainerServiceName)
csContainerServiceName = Lens.field @"containerServiceName"
{-# DEPRECATED csContainerServiceName "Use generic-lens or generic-optics with 'containerServiceName' instead." #-}

-- | The timestamp when the container service was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatedAt :: Lens.Lens' ContainerService (Core.Maybe Core.NominalDiffTime)
csCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED csCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes the current container deployment of the container service.
--
-- /Note:/ Consider using 'currentDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCurrentDeployment :: Lens.Lens' ContainerService (Core.Maybe Types.ContainerServiceDeployment)
csCurrentDeployment = Lens.field @"currentDeployment"
{-# DEPRECATED csCurrentDeployment "Use generic-lens or generic-optics with 'currentDeployment' instead." #-}

-- | A Boolean value indicating whether the container service is disabled.
--
-- /Note:/ Consider using 'isDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIsDisabled :: Lens.Lens' ContainerService (Core.Maybe Core.Bool)
csIsDisabled = Lens.field @"isDisabled"
{-# DEPRECATED csIsDisabled "Use generic-lens or generic-optics with 'isDisabled' instead." #-}

-- | An object that describes the location of the container service, such as the AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLocation :: Lens.Lens' ContainerService (Core.Maybe Types.ResourceLocation)
csLocation = Lens.field @"location"
{-# DEPRECATED csLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | An object that describes the next deployment of the container service.
--
-- This value is @null@ when there is no deployment in a @pending@ state.
--
-- /Note:/ Consider using 'nextDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNextDeployment :: Lens.Lens' ContainerService (Core.Maybe Types.ContainerServiceDeployment)
csNextDeployment = Lens.field @"nextDeployment"
{-# DEPRECATED csNextDeployment "Use generic-lens or generic-optics with 'nextDeployment' instead." #-}

-- | The power specification of the container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPower :: Lens.Lens' ContainerService (Core.Maybe Types.ContainerServicePowerName)
csPower = Lens.field @"power"
{-# DEPRECATED csPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | The ID of the power of the container service.
--
-- /Note:/ Consider using 'powerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPowerId :: Lens.Lens' ContainerService (Core.Maybe Types.PowerId)
csPowerId = Lens.field @"powerId"
{-# DEPRECATED csPowerId "Use generic-lens or generic-optics with 'powerId' instead." #-}

-- | The principal ARN of the container service.
--
-- The principal ARN can be used to create a trust relationship between your standard AWS account and your Lightsail container service. This allows you to give your service permission to access resources in your standard AWS account.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPrincipalArn :: Lens.Lens' ContainerService (Core.Maybe Types.PrincipalArn)
csPrincipalArn = Lens.field @"principalArn"
{-# DEPRECATED csPrincipalArn "Use generic-lens or generic-optics with 'principalArn' instead." #-}

-- | The private domain name of the container service.
--
-- The private domain name is accessible only by other resources within the default virtual private cloud (VPC) of your Lightsail account.
--
-- /Note:/ Consider using 'privateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPrivateDomainName :: Lens.Lens' ContainerService (Core.Maybe Types.PrivateDomainName)
csPrivateDomainName = Lens.field @"privateDomainName"
{-# DEPRECATED csPrivateDomainName "Use generic-lens or generic-optics with 'privateDomainName' instead." #-}

-- | The public domain name of the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- See @CreateContainerService@ or @UpdateContainerService@ for information about how to specify public domain names for your Lightsail container service.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPublicDomainNames :: Lens.Lens' ContainerService (Core.Maybe (Core.HashMap Types.String [Types.String]))
csPublicDomainNames = Lens.field @"publicDomainNames"
{-# DEPRECATED csPublicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead." #-}

-- | The Lightsail resource type of the container service (i.e., @ContainerService@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csResourceType :: Lens.Lens' ContainerService (Core.Maybe Types.ResourceType)
csResourceType = Lens.field @"resourceType"
{-# DEPRECATED csResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The scale specification of the container service.
--
-- The scale specifies the allocated compute nodes of the container service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScale :: Lens.Lens' ContainerService (Core.Maybe Core.Natural)
csScale = Lens.field @"scale"
{-# DEPRECATED csScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The current state of the container service.
--
-- The state can be:
--
--     * @Pending@ - The container service is being created.
--
--
--     * @Ready@ - The container service is created but does not have a container deployment.
--
--
--     * @Disabled@ - The container service is disabled.
--
--
--     * @Updating@ - The container service capacity or other setting is being updated.
--
--
--     * @Deploying@ - The container service is launching a container deployment.
--
--
--     * @Running@ - The container service is created and it has a container deployment.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csState :: Lens.Lens' ContainerService (Core.Maybe Types.ContainerServiceState)
csState = Lens.field @"state"
{-# DEPRECATED csState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' ContainerService (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The publicly accessible URL of the container service.
--
-- If no public endpoint is specified in the @currentDeployment@ , this URL returns a 404 response.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUrl :: Lens.Lens' ContainerService (Core.Maybe Types.Url)
csUrl = Lens.field @"url"
{-# DEPRECATED csUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON ContainerService where
  parseJSON =
    Core.withObject "ContainerService" Core.$
      \x ->
        ContainerService'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "containerServiceName")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "currentDeployment")
          Core.<*> (x Core..:? "isDisabled")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "nextDeployment")
          Core.<*> (x Core..:? "power")
          Core.<*> (x Core..:? "powerId")
          Core.<*> (x Core..:? "principalArn")
          Core.<*> (x Core..:? "privateDomainName")
          Core.<*> (x Core..:? "publicDomainNames")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "scale")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "url")
