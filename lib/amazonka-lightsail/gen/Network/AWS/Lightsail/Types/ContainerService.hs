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
    csState,
    csPowerId,
    csResourceType,
    csArn,
    csCreatedAt,
    csLocation,
    csScale,
    csUrl,
    csNextDeployment,
    csPrincipalARN,
    csPower,
    csPrivateDomainName,
    csIsDisabled,
    csPublicDomainNames,
    csContainerServiceName,
    csCurrentDeployment,
    csTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceDeployment
import Network.AWS.Lightsail.Types.ContainerServicePowerName
import Network.AWS.Lightsail.Types.ContainerServiceState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerService' smart constructor.
data ContainerService = ContainerService'
  { state ::
      Lude.Maybe ContainerServiceState,
    powerId :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    scale :: Lude.Maybe Lude.Natural,
    url :: Lude.Maybe Lude.Text,
    nextDeployment :: Lude.Maybe ContainerServiceDeployment,
    principalARN :: Lude.Maybe Lude.Text,
    power :: Lude.Maybe ContainerServicePowerName,
    privateDomainName :: Lude.Maybe Lude.Text,
    isDisabled :: Lude.Maybe Lude.Bool,
    publicDomainNames ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    containerServiceName :: Lude.Maybe Lude.Text,
    currentDeployment ::
      Lude.Maybe ContainerServiceDeployment,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerService' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the container service.
-- * 'containerServiceName' - The name of the container service.
-- * 'createdAt' - The timestamp when the container service was created.
-- * 'currentDeployment' - An object that describes the current container deployment of the container service.
-- * 'isDisabled' - A Boolean value indicating whether the container service is disabled.
-- * 'location' - An object that describes the location of the container service, such as the AWS Region and Availability Zone.
-- * 'nextDeployment' - An object that describes the next deployment of the container service.
--
-- This value is @null@ when there is no deployment in a @pending@ state.
-- * 'power' - The power specification of the container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
-- * 'powerId' - The ID of the power of the container service.
-- * 'principalARN' - The principal ARN of the container service.
--
-- The principal ARN can be used to create a trust relationship between your standard AWS account and your Lightsail container service. This allows you to give your service permission to access resources in your standard AWS account.
-- * 'privateDomainName' - The private domain name of the container service.
--
-- The private domain name is accessible only by other resources within the default virtual private cloud (VPC) of your Lightsail account.
-- * 'publicDomainNames' - The public domain name of the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- See @CreateContainerService@ or @UpdateContainerService@ for information about how to specify public domain names for your Lightsail container service.
-- * 'resourceType' - The Lightsail resource type of the container service (i.e., @ContainerService@ ).
-- * 'scale' - The scale specification of the container service.
--
-- The scale specifies the allocated compute nodes of the container service.
-- * 'state' - The current state of the container service.
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
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
-- * 'url' - The publicly accessible URL of the container service.
--
-- If no public endpoint is specified in the @currentDeployment@ , this URL returns a 404 response.
mkContainerService ::
  ContainerService
mkContainerService =
  ContainerService'
    { state = Lude.Nothing,
      powerId = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      scale = Lude.Nothing,
      url = Lude.Nothing,
      nextDeployment = Lude.Nothing,
      principalARN = Lude.Nothing,
      power = Lude.Nothing,
      privateDomainName = Lude.Nothing,
      isDisabled = Lude.Nothing,
      publicDomainNames = Lude.Nothing,
      containerServiceName = Lude.Nothing,
      currentDeployment = Lude.Nothing,
      tags = Lude.Nothing
    }

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
csState :: Lens.Lens' ContainerService (Lude.Maybe ContainerServiceState)
csState = Lens.lens (state :: ContainerService -> Lude.Maybe ContainerServiceState) (\s a -> s {state = a} :: ContainerService)
{-# DEPRECATED csState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the power of the container service.
--
-- /Note:/ Consider using 'powerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPowerId :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csPowerId = Lens.lens (powerId :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {powerId = a} :: ContainerService)
{-# DEPRECATED csPowerId "Use generic-lens or generic-optics with 'powerId' instead." #-}

-- | The Lightsail resource type of the container service (i.e., @ContainerService@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csResourceType :: Lens.Lens' ContainerService (Lude.Maybe ResourceType)
csResourceType = Lens.lens (resourceType :: ContainerService -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ContainerService)
{-# DEPRECATED csResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the container service.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csArn :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csArn = Lens.lens (arn :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ContainerService)
{-# DEPRECATED csArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the container service was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatedAt :: Lens.Lens' ContainerService (Lude.Maybe Lude.Timestamp)
csCreatedAt = Lens.lens (createdAt :: ContainerService -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContainerService)
{-# DEPRECATED csCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes the location of the container service, such as the AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLocation :: Lens.Lens' ContainerService (Lude.Maybe ResourceLocation)
csLocation = Lens.lens (location :: ContainerService -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: ContainerService)
{-# DEPRECATED csLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The scale specification of the container service.
--
-- The scale specifies the allocated compute nodes of the container service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScale :: Lens.Lens' ContainerService (Lude.Maybe Lude.Natural)
csScale = Lens.lens (scale :: ContainerService -> Lude.Maybe Lude.Natural) (\s a -> s {scale = a} :: ContainerService)
{-# DEPRECATED csScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The publicly accessible URL of the container service.
--
-- If no public endpoint is specified in the @currentDeployment@ , this URL returns a 404 response.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUrl :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csUrl = Lens.lens (url :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: ContainerService)
{-# DEPRECATED csUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | An object that describes the next deployment of the container service.
--
-- This value is @null@ when there is no deployment in a @pending@ state.
--
-- /Note:/ Consider using 'nextDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNextDeployment :: Lens.Lens' ContainerService (Lude.Maybe ContainerServiceDeployment)
csNextDeployment = Lens.lens (nextDeployment :: ContainerService -> Lude.Maybe ContainerServiceDeployment) (\s a -> s {nextDeployment = a} :: ContainerService)
{-# DEPRECATED csNextDeployment "Use generic-lens or generic-optics with 'nextDeployment' instead." #-}

-- | The principal ARN of the container service.
--
-- The principal ARN can be used to create a trust relationship between your standard AWS account and your Lightsail container service. This allows you to give your service permission to access resources in your standard AWS account.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPrincipalARN :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csPrincipalARN = Lens.lens (principalARN :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: ContainerService)
{-# DEPRECATED csPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The power specification of the container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPower :: Lens.Lens' ContainerService (Lude.Maybe ContainerServicePowerName)
csPower = Lens.lens (power :: ContainerService -> Lude.Maybe ContainerServicePowerName) (\s a -> s {power = a} :: ContainerService)
{-# DEPRECATED csPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | The private domain name of the container service.
--
-- The private domain name is accessible only by other resources within the default virtual private cloud (VPC) of your Lightsail account.
--
-- /Note:/ Consider using 'privateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPrivateDomainName :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csPrivateDomainName = Lens.lens (privateDomainName :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {privateDomainName = a} :: ContainerService)
{-# DEPRECATED csPrivateDomainName "Use generic-lens or generic-optics with 'privateDomainName' instead." #-}

-- | A Boolean value indicating whether the container service is disabled.
--
-- /Note:/ Consider using 'isDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIsDisabled :: Lens.Lens' ContainerService (Lude.Maybe Lude.Bool)
csIsDisabled = Lens.lens (isDisabled :: ContainerService -> Lude.Maybe Lude.Bool) (\s a -> s {isDisabled = a} :: ContainerService)
{-# DEPRECATED csIsDisabled "Use generic-lens or generic-optics with 'isDisabled' instead." #-}

-- | The public domain name of the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- See @CreateContainerService@ or @UpdateContainerService@ for information about how to specify public domain names for your Lightsail container service.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPublicDomainNames :: Lens.Lens' ContainerService (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
csPublicDomainNames = Lens.lens (publicDomainNames :: ContainerService -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {publicDomainNames = a} :: ContainerService)
{-# DEPRECATED csPublicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead." #-}

-- | The name of the container service.
--
-- /Note:/ Consider using 'containerServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csContainerServiceName :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csContainerServiceName = Lens.lens (containerServiceName :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {containerServiceName = a} :: ContainerService)
{-# DEPRECATED csContainerServiceName "Use generic-lens or generic-optics with 'containerServiceName' instead." #-}

-- | An object that describes the current container deployment of the container service.
--
-- /Note:/ Consider using 'currentDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCurrentDeployment :: Lens.Lens' ContainerService (Lude.Maybe ContainerServiceDeployment)
csCurrentDeployment = Lens.lens (currentDeployment :: ContainerService -> Lude.Maybe ContainerServiceDeployment) (\s a -> s {currentDeployment = a} :: ContainerService)
{-# DEPRECATED csCurrentDeployment "Use generic-lens or generic-optics with 'currentDeployment' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' ContainerService (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: ContainerService -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ContainerService)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ContainerService where
  parseJSON =
    Lude.withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "powerId")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "scale")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "nextDeployment")
            Lude.<*> (x Lude..:? "principalArn")
            Lude.<*> (x Lude..:? "power")
            Lude.<*> (x Lude..:? "privateDomainName")
            Lude.<*> (x Lude..:? "isDisabled")
            Lude.<*> (x Lude..:? "publicDomainNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "containerServiceName")
            Lude.<*> (x Lude..:? "currentDeployment")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
