{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Domain@ used by Amazon SageMaker Studio. A domain consists of an associated Amazon Elastic File System (EFS) volume, a list of authorized users, and a variety of security, application, policy, and Amazon Virtual Private Cloud (VPC) configurations. An AWS account is limited to one domain per region. Users within a domain can share notebook files and other artifacts with each other.
--
-- When a domain is created, an EFS volume is created for use by all of the users within the domain. Each user receives a private home directory within the EFS volume for notebooks, Git repositories, and data files.
-- __VPC configuration__
-- All SageMaker Studio traffic between the domain and the EFS volume is through the specified VPC and subnets. For other Studio traffic, you can specify the @AppNetworkAccessType@ parameter. @AppNetworkAccessType@ corresponds to the network access type that you choose when you onboard to Studio. The following options are available:
--
--     * @PublicInternetOnly@ - Non-EFS traffic goes through a VPC managed by Amazon SageMaker, which allows internet access. This is the default value.
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets. Internet access is disabled by default. To allow internet access, you must specify a NAT gateway.
-- When internet access is disabled, you won't be able to run a Studio notebook or to train or host models unless your VPC has an interface endpoint to the SageMaker API and runtime or a NAT gateway and your security groups allow outbound connections.
--
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-notebooks-and-internet-access.html Connect SageMaker Studio Notebooks to Resources in a VPC> .
module Network.AWS.SageMaker.CreateDomain
  ( -- * Creating a request
    CreateDomain (..),
    mkCreateDomain,

    -- ** Request lenses
    cdDefaultUserSettings,
    cdSubnetIds,
    cdVPCId,
    cdAuthMode,
    cdHomeEfsFileSystemKMSKeyId,
    cdKMSKeyId,
    cdDomainName,
    cdAppNetworkAccessType,
    cdTags,

    -- * Destructuring the response
    CreateDomainResponse (..),
    mkCreateDomainResponse,

    -- ** Response lenses
    cdrsDomainARN,
    cdrsURL,
    cdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The default user settings.
    defaultUserSettings :: UserSettings,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Lude.NonEmpty Lude.Text,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
    vpcId :: Lude.Text,
    -- | The mode of authentication that members use to access the domain.
    authMode :: AuthMode,
    -- | This member is deprecated and replaced with @KmsKeyId@ .
    homeEfsFileSystemKMSKeyId :: Lude.Maybe Lude.Text,
    -- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A name for the domain.
    domainName :: Lude.Text,
    -- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
    --
    --
    --     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
    --
    --
    --     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
    appNetworkAccessType :: Lude.Maybe AppNetworkAccessType,
    -- | Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- * 'defaultUserSettings' - The default user settings.
-- * 'subnetIds' - The VPC subnets that Studio uses for communication.
-- * 'vpcId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
-- * 'authMode' - The mode of authentication that members use to access the domain.
-- * 'homeEfsFileSystemKMSKeyId' - This member is deprecated and replaced with @KmsKeyId@ .
-- * 'kmsKeyId' - SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
-- * 'domainName' - A name for the domain.
-- * 'appNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
-- * 'tags' - Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
mkCreateDomain ::
  -- | 'defaultUserSettings'
  UserSettings ->
  -- | 'subnetIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'domainName'
  Lude.Text ->
  CreateDomain
mkCreateDomain
  pDefaultUserSettings_
  pSubnetIds_
  pVPCId_
  pAuthMode_
  pDomainName_ =
    CreateDomain'
      { defaultUserSettings = pDefaultUserSettings_,
        subnetIds = pSubnetIds_,
        vpcId = pVPCId_,
        authMode = pAuthMode_,
        homeEfsFileSystemKMSKeyId = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        domainName = pDomainName_,
        appNetworkAccessType = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The default user settings.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDefaultUserSettings :: Lens.Lens' CreateDomain UserSettings
cdDefaultUserSettings = Lens.lens (defaultUserSettings :: CreateDomain -> UserSettings) (\s a -> s {defaultUserSettings = a} :: CreateDomain)
{-# DEPRECATED cdDefaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead." #-}

-- | The VPC subnets that Studio uses for communication.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubnetIds :: Lens.Lens' CreateDomain (Lude.NonEmpty Lude.Text)
cdSubnetIds = Lens.lens (subnetIds :: CreateDomain -> Lude.NonEmpty Lude.Text) (\s a -> s {subnetIds = a} :: CreateDomain)
{-# DEPRECATED cdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVPCId :: Lens.Lens' CreateDomain Lude.Text
cdVPCId = Lens.lens (vpcId :: CreateDomain -> Lude.Text) (\s a -> s {vpcId = a} :: CreateDomain)
{-# DEPRECATED cdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The mode of authentication that members use to access the domain.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAuthMode :: Lens.Lens' CreateDomain AuthMode
cdAuthMode = Lens.lens (authMode :: CreateDomain -> AuthMode) (\s a -> s {authMode = a} :: CreateDomain)
{-# DEPRECATED cdAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | This member is deprecated and replaced with @KmsKeyId@ .
--
-- /Note:/ Consider using 'homeEfsFileSystemKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdHomeEfsFileSystemKMSKeyId :: Lens.Lens' CreateDomain (Lude.Maybe Lude.Text)
cdHomeEfsFileSystemKMSKeyId = Lens.lens (homeEfsFileSystemKMSKeyId :: CreateDomain -> Lude.Maybe Lude.Text) (\s a -> s {homeEfsFileSystemKMSKeyId = a} :: CreateDomain)
{-# DEPRECATED cdHomeEfsFileSystemKMSKeyId "Use generic-lens or generic-optics with 'homeEfsFileSystemKMSKeyId' instead." #-}

-- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKMSKeyId :: Lens.Lens' CreateDomain (Lude.Maybe Lude.Text)
cdKMSKeyId = Lens.lens (kmsKeyId :: CreateDomain -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateDomain)
{-# DEPRECATED cdKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A name for the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Lude.Text
cdDomainName = Lens.lens (domainName :: CreateDomain -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomain)
{-# DEPRECATED cdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
--
-- /Note:/ Consider using 'appNetworkAccessType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAppNetworkAccessType :: Lens.Lens' CreateDomain (Lude.Maybe AppNetworkAccessType)
cdAppNetworkAccessType = Lens.lens (appNetworkAccessType :: CreateDomain -> Lude.Maybe AppNetworkAccessType) (\s a -> s {appNetworkAccessType = a} :: CreateDomain)
{-# DEPRECATED cdAppNetworkAccessType "Use generic-lens or generic-optics with 'appNetworkAccessType' instead." #-}

-- | Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDomain (Lude.Maybe [Tag])
cdTags = Lens.lens (tags :: CreateDomain -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDomain)
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Lude.<$> (x Lude..?> "DomainArn")
            Lude.<*> (x Lude..?> "Url")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DefaultUserSettings" Lude..= defaultUserSettings),
            Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just ("VpcId" Lude..= vpcId),
            Lude.Just ("AuthMode" Lude..= authMode),
            ("HomeEfsFileSystemKmsKeyId" Lude..=)
              Lude.<$> homeEfsFileSystemKMSKeyId,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            Lude.Just ("DomainName" Lude..= domainName),
            ("AppNetworkAccessType" Lude..=) Lude.<$> appNetworkAccessType,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the created domain.
    domainARN :: Lude.Maybe Lude.Text,
    -- | The URL to the created domain.
    url :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainARN' - The Amazon Resource Name (ARN) of the created domain.
-- * 'url' - The URL to the created domain.
-- * 'responseStatus' - The response status code.
mkCreateDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDomainResponse
mkCreateDomainResponse pResponseStatus_ =
  CreateDomainResponse'
    { domainARN = Lude.Nothing,
      url = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the created domain.
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDomainARN :: Lens.Lens' CreateDomainResponse (Lude.Maybe Lude.Text)
cdrsDomainARN = Lens.lens (domainARN :: CreateDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The URL to the created domain.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsURL :: Lens.Lens' CreateDomainResponse (Lude.Maybe Lude.Text)
cdrsURL = Lens.lens (url :: CreateDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDomainResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
