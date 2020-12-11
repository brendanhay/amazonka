-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.BaseConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.BaseConfigurationItem
  ( BaseConfigurationItem (..),

    -- * Smart constructor
    mkBaseConfigurationItem,

    -- * Lenses
    bciResourceId,
    bciResourceType,
    bciConfigurationStateId,
    bciArn,
    bciResourceName,
    bciResourceCreationTime,
    bciConfigurationItemStatus,
    bciConfigurationItemCaptureTime,
    bciAccountId,
    bciSupplementaryConfiguration,
    bciAvailabilityZone,
    bciVersion,
    bciAwsRegion,
    bciConfiguration,
  )
where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed configuration of a specified resource.
--
-- /See:/ 'mkBaseConfigurationItem' smart constructor.
data BaseConfigurationItem = BaseConfigurationItem'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    configurationStateId :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    resourceName :: Lude.Maybe Lude.Text,
    resourceCreationTime ::
      Lude.Maybe Lude.Timestamp,
    configurationItemStatus ::
      Lude.Maybe ConfigurationItemStatus,
    configurationItemCaptureTime ::
      Lude.Maybe Lude.Timestamp,
    accountId :: Lude.Maybe Lude.Text,
    supplementaryConfiguration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    availabilityZone :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    awsRegion :: Lude.Maybe Lude.Text,
    configuration :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BaseConfigurationItem' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit AWS account ID associated with the resource.
-- * 'arn' - The Amazon Resource Name (ARN) of the resource.
-- * 'availabilityZone' - The Availability Zone associated with the resource.
-- * 'awsRegion' - The region where the resource resides.
-- * 'configuration' - The description of the resource configuration.
-- * 'configurationItemCaptureTime' - The time when the configuration recording was initiated.
-- * 'configurationItemStatus' - The configuration item status. The valid values are:
--
--
--     * OK – The resource configuration has been updated
--
--
--     * ResourceDiscovered – The resource was newly discovered
--
--
--     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--     * ResourceDeleted – The resource was deleted
--
--
--     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
-- * 'configurationStateId' - An identifier that indicates the ordering of the configuration items of a resource.
-- * 'resourceCreationTime' - The time stamp when the resource was created.
-- * 'resourceId' - The ID of the resource (for example., sg-xxxxxx).
-- * 'resourceName' - The custom name of the resource, if available.
-- * 'resourceType' - The type of AWS resource.
-- * 'supplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the configuration parameter.
-- * 'version' - The version number of the resource configuration.
mkBaseConfigurationItem ::
  BaseConfigurationItem
mkBaseConfigurationItem =
  BaseConfigurationItem'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      configurationStateId = Lude.Nothing,
      arn = Lude.Nothing,
      resourceName = Lude.Nothing,
      resourceCreationTime = Lude.Nothing,
      configurationItemStatus = Lude.Nothing,
      configurationItemCaptureTime = Lude.Nothing,
      accountId = Lude.Nothing,
      supplementaryConfiguration = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      version = Lude.Nothing,
      awsRegion = Lude.Nothing,
      configuration = Lude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceId :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciResourceId = Lens.lens (resourceId :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: BaseConfigurationItem)
{-# DEPRECATED bciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceType :: Lens.Lens' BaseConfigurationItem (Lude.Maybe ResourceType)
bciResourceType = Lens.lens (resourceType :: BaseConfigurationItem -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: BaseConfigurationItem)
{-# DEPRECATED bciResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An identifier that indicates the ordering of the configuration items of a resource.
--
-- /Note:/ Consider using 'configurationStateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfigurationStateId :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciConfigurationStateId = Lens.lens (configurationStateId :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {configurationStateId = a} :: BaseConfigurationItem)
{-# DEPRECATED bciConfigurationStateId "Use generic-lens or generic-optics with 'configurationStateId' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciArn :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciArn = Lens.lens (arn :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: BaseConfigurationItem)
{-# DEPRECATED bciArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The custom name of the resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceName :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciResourceName = Lens.lens (resourceName :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: BaseConfigurationItem)
{-# DEPRECATED bciResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The time stamp when the resource was created.
--
-- /Note:/ Consider using 'resourceCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceCreationTime :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Timestamp)
bciResourceCreationTime = Lens.lens (resourceCreationTime :: BaseConfigurationItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {resourceCreationTime = a} :: BaseConfigurationItem)
{-# DEPRECATED bciResourceCreationTime "Use generic-lens or generic-optics with 'resourceCreationTime' instead." #-}

-- | The configuration item status. The valid values are:
--
--
--     * OK – The resource configuration has been updated
--
--
--     * ResourceDiscovered – The resource was newly discovered
--
--
--     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--     * ResourceDeleted – The resource was deleted
--
--
--     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--
-- /Note:/ Consider using 'configurationItemStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfigurationItemStatus :: Lens.Lens' BaseConfigurationItem (Lude.Maybe ConfigurationItemStatus)
bciConfigurationItemStatus = Lens.lens (configurationItemStatus :: BaseConfigurationItem -> Lude.Maybe ConfigurationItemStatus) (\s a -> s {configurationItemStatus = a} :: BaseConfigurationItem)
{-# DEPRECATED bciConfigurationItemStatus "Use generic-lens or generic-optics with 'configurationItemStatus' instead." #-}

-- | The time when the configuration recording was initiated.
--
-- /Note:/ Consider using 'configurationItemCaptureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfigurationItemCaptureTime :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Timestamp)
bciConfigurationItemCaptureTime = Lens.lens (configurationItemCaptureTime :: BaseConfigurationItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {configurationItemCaptureTime = a} :: BaseConfigurationItem)
{-# DEPRECATED bciConfigurationItemCaptureTime "Use generic-lens or generic-optics with 'configurationItemCaptureTime' instead." #-}

-- | The 12-digit AWS account ID associated with the resource.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAccountId :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciAccountId = Lens.lens (accountId :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: BaseConfigurationItem)
{-# DEPRECATED bciAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the configuration parameter.
--
-- /Note:/ Consider using 'supplementaryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciSupplementaryConfiguration :: Lens.Lens' BaseConfigurationItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
bciSupplementaryConfiguration = Lens.lens (supplementaryConfiguration :: BaseConfigurationItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {supplementaryConfiguration = a} :: BaseConfigurationItem)
{-# DEPRECATED bciSupplementaryConfiguration "Use generic-lens or generic-optics with 'supplementaryConfiguration' instead." #-}

-- | The Availability Zone associated with the resource.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAvailabilityZone :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciAvailabilityZone = Lens.lens (availabilityZone :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: BaseConfigurationItem)
{-# DEPRECATED bciAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The version number of the resource configuration.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciVersion :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciVersion = Lens.lens (version :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: BaseConfigurationItem)
{-# DEPRECATED bciVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The region where the resource resides.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAwsRegion :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciAwsRegion = Lens.lens (awsRegion :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: BaseConfigurationItem)
{-# DEPRECATED bciAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The description of the resource configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfiguration :: Lens.Lens' BaseConfigurationItem (Lude.Maybe Lude.Text)
bciConfiguration = Lens.lens (configuration :: BaseConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: BaseConfigurationItem)
{-# DEPRECATED bciConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

instance Lude.FromJSON BaseConfigurationItem where
  parseJSON =
    Lude.withObject
      "BaseConfigurationItem"
      ( \x ->
          BaseConfigurationItem'
            Lude.<$> (x Lude..:? "resourceId")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "configurationStateId")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "resourceName")
            Lude.<*> (x Lude..:? "resourceCreationTime")
            Lude.<*> (x Lude..:? "configurationItemStatus")
            Lude.<*> (x Lude..:? "configurationItemCaptureTime")
            Lude.<*> (x Lude..:? "accountId")
            Lude.<*> (x Lude..:? "supplementaryConfiguration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "availabilityZone")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "awsRegion")
            Lude.<*> (x Lude..:? "configuration")
      )
