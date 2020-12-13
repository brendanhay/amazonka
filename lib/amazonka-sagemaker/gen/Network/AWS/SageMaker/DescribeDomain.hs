{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The description of the domain.
module Network.AWS.SageMaker.DescribeDomain
  ( -- * Creating a request
    DescribeDomain (..),
    mkDescribeDomain,

    -- ** Request lenses
    dDomainId,

    -- * Destructuring the response
    DescribeDomainResponse (..),
    mkDescribeDomainResponse,

    -- ** Response lenses
    ddrsCreationTime,
    ddrsStatus,
    ddrsFailureReason,
    ddrsDefaultUserSettings,
    ddrsSubnetIds,
    ddrsDomainARN,
    ddrsVPCId,
    ddrsURL,
    ddrsAuthMode,
    ddrsHomeEfsFileSystemKMSKeyId,
    ddrsSingleSignOnManagedApplicationInstanceId,
    ddrsLastModifiedTime,
    ddrsHomeEfsFileSystemId,
    ddrsKMSKeyId,
    ddrsDomainName,
    ddrsDomainId,
    ddrsAppNetworkAccessType,
    ddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain'
  { -- | The domain ID.
    domainId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomain' with the minimum fields required to make a request.
--
-- * 'domainId' - The domain ID.
mkDescribeDomain ::
  -- | 'domainId'
  Lude.Text ->
  DescribeDomain
mkDescribeDomain pDomainId_ =
  DescribeDomain' {domainId = pDomainId_}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainId :: Lens.Lens' DescribeDomain Lude.Text
dDomainId = Lens.lens (domainId :: DescribeDomain -> Lude.Text) (\s a -> s {domainId = a} :: DescribeDomain)
{-# DEPRECATED dDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.AWSRequest DescribeDomain where
  type Rs DescribeDomain = DescribeDomainResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "DefaultUserSettings")
            Lude.<*> (x Lude..?> "SubnetIds")
            Lude.<*> (x Lude..?> "DomainArn")
            Lude.<*> (x Lude..?> "VpcId")
            Lude.<*> (x Lude..?> "Url")
            Lude.<*> (x Lude..?> "AuthMode")
            Lude.<*> (x Lude..?> "HomeEfsFileSystemKmsKeyId")
            Lude.<*> (x Lude..?> "SingleSignOnManagedApplicationInstanceId")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "HomeEfsFileSystemId")
            Lude.<*> (x Lude..?> "KmsKeyId")
            Lude.<*> (x Lude..?> "DomainName")
            Lude.<*> (x Lude..?> "DomainId")
            Lude.<*> (x Lude..?> "AppNetworkAccessType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainId" Lude..= domainId)])

instance Lude.ToPath DescribeDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The creation time.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status.
    status :: Lude.Maybe DomainStatus,
    -- | The failure reason.
    failureReason :: Lude.Maybe Lude.Text,
    -- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
    defaultUserSettings :: Lude.Maybe UserSettings,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The domain's Amazon Resource Name (ARN).
    domainARN :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The domain's URL.
    url :: Lude.Maybe Lude.Text,
    -- | The domain's authentication mode.
    authMode :: Lude.Maybe AuthMode,
    -- | This member is deprecated and replaced with @KmsKeyId@ .
    homeEfsFileSystemKMSKeyId :: Lude.Maybe Lude.Text,
    -- | The SSO managed application instance ID.
    singleSignOnManagedApplicationInstanceId :: Lude.Maybe Lude.Text,
    -- | The last modified time.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
    homeEfsFileSystemId :: Lude.Maybe Lude.Text,
    -- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The domain name.
    domainName :: Lude.Maybe Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Maybe Lude.Text,
    -- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
    --
    --
    --     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
    --
    --
    --     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
    appNetworkAccessType :: Lude.Maybe AppNetworkAccessType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'status' - The status.
-- * 'failureReason' - The failure reason.
-- * 'defaultUserSettings' - Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
-- * 'subnetIds' - The VPC subnets that Studio uses for communication.
-- * 'domainARN' - The domain's Amazon Resource Name (ARN).
-- * 'vpcId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
-- * 'url' - The domain's URL.
-- * 'authMode' - The domain's authentication mode.
-- * 'homeEfsFileSystemKMSKeyId' - This member is deprecated and replaced with @KmsKeyId@ .
-- * 'singleSignOnManagedApplicationInstanceId' - The SSO managed application instance ID.
-- * 'lastModifiedTime' - The last modified time.
-- * 'homeEfsFileSystemId' - The ID of the Amazon Elastic File System (EFS) managed by this Domain.
-- * 'kmsKeyId' - The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
-- * 'domainName' - The domain name.
-- * 'domainId' - The domain ID.
-- * 'appNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
-- * 'responseStatus' - The response status code.
mkDescribeDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDomainResponse
mkDescribeDomainResponse pResponseStatus_ =
  DescribeDomainResponse'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      failureReason = Lude.Nothing,
      defaultUserSettings = Lude.Nothing,
      subnetIds = Lude.Nothing,
      domainARN = Lude.Nothing,
      vpcId = Lude.Nothing,
      url = Lude.Nothing,
      authMode = Lude.Nothing,
      homeEfsFileSystemKMSKeyId = Lude.Nothing,
      singleSignOnManagedApplicationInstanceId = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      homeEfsFileSystemId = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      domainName = Lude.Nothing,
      domainId = Lude.Nothing,
      appNetworkAccessType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsCreationTime :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Timestamp)
ddrsCreationTime = Lens.lens (creationTime :: DescribeDomainResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsStatus :: Lens.Lens' DescribeDomainResponse (Lude.Maybe DomainStatus)
ddrsStatus = Lens.lens (status :: DescribeDomainResponse -> Lude.Maybe DomainStatus) (\s a -> s {status = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsFailureReason :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsFailureReason = Lens.lens (failureReason :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDefaultUserSettings :: Lens.Lens' DescribeDomainResponse (Lude.Maybe UserSettings)
ddrsDefaultUserSettings = Lens.lens (defaultUserSettings :: DescribeDomainResponse -> Lude.Maybe UserSettings) (\s a -> s {defaultUserSettings = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsDefaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead." #-}

-- | The VPC subnets that Studio uses for communication.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsSubnetIds :: Lens.Lens' DescribeDomainResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
ddrsSubnetIds = Lens.lens (subnetIds :: DescribeDomainResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {subnetIds = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The domain's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDomainARN :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsDomainARN = Lens.lens (domainARN :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsVPCId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsVPCId = Lens.lens (vpcId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The domain's URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsURL :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsURL = Lens.lens (url :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The domain's authentication mode.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsAuthMode :: Lens.Lens' DescribeDomainResponse (Lude.Maybe AuthMode)
ddrsAuthMode = Lens.lens (authMode :: DescribeDomainResponse -> Lude.Maybe AuthMode) (\s a -> s {authMode = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | This member is deprecated and replaced with @KmsKeyId@ .
--
-- /Note:/ Consider using 'homeEfsFileSystemKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsHomeEfsFileSystemKMSKeyId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsHomeEfsFileSystemKMSKeyId = Lens.lens (homeEfsFileSystemKMSKeyId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {homeEfsFileSystemKMSKeyId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsHomeEfsFileSystemKMSKeyId "Use generic-lens or generic-optics with 'homeEfsFileSystemKMSKeyId' instead." #-}

-- | The SSO managed application instance ID.
--
-- /Note:/ Consider using 'singleSignOnManagedApplicationInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsSingleSignOnManagedApplicationInstanceId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsSingleSignOnManagedApplicationInstanceId = Lens.lens (singleSignOnManagedApplicationInstanceId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {singleSignOnManagedApplicationInstanceId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsSingleSignOnManagedApplicationInstanceId "Use generic-lens or generic-optics with 'singleSignOnManagedApplicationInstanceId' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsLastModifiedTime :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Timestamp)
ddrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeDomainResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- /Note:/ Consider using 'homeEfsFileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsHomeEfsFileSystemId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsHomeEfsFileSystemId = Lens.lens (homeEfsFileSystemId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {homeEfsFileSystemId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsHomeEfsFileSystemId "Use generic-lens or generic-optics with 'homeEfsFileSystemId' instead." #-}

-- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsKMSKeyId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsKMSKeyId = Lens.lens (kmsKeyId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDomainName :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsDomainName = Lens.lens (domainName :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDomainId :: Lens.Lens' DescribeDomainResponse (Lude.Maybe Lude.Text)
ddrsDomainId = Lens.lens (domainId :: DescribeDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

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
ddrsAppNetworkAccessType :: Lens.Lens' DescribeDomainResponse (Lude.Maybe AppNetworkAccessType)
ddrsAppNetworkAccessType = Lens.lens (appNetworkAccessType :: DescribeDomainResponse -> Lude.Maybe AppNetworkAccessType) (\s a -> s {appNetworkAccessType = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsAppNetworkAccessType "Use generic-lens or generic-optics with 'appNetworkAccessType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDomainResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
