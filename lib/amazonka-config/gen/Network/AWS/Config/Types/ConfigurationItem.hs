{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItem
  ( ConfigurationItem (..),

    -- * Smart constructor
    mkConfigurationItem,

    -- * Lenses
    ciResourceId,
    ciResourceType,
    ciConfigurationStateId,
    ciArn,
    ciResourceName,
    ciResourceCreationTime,
    ciConfigurationItemStatus,
    ciConfigurationItemCaptureTime,
    ciAccountId,
    ciSupplementaryConfiguration,
    ciAvailabilityZone,
    ciRelationships,
    ciVersion,
    ciAwsRegion,
    ciRelatedEvents,
    ciConfiguration,
    ciConfigurationItemMD5Hash,
    ciTags,
  )
where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list that contains detailed configurations of a specified resource.
--
-- /See:/ 'mkConfigurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    configurationStateId :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    resourceName :: Lude.Maybe Lude.Text,
    resourceCreationTime :: Lude.Maybe Lude.Timestamp,
    configurationItemStatus ::
      Lude.Maybe ConfigurationItemStatus,
    configurationItemCaptureTime ::
      Lude.Maybe Lude.Timestamp,
    accountId :: Lude.Maybe Lude.Text,
    supplementaryConfiguration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    availabilityZone :: Lude.Maybe Lude.Text,
    relationships :: Lude.Maybe [Relationship],
    version :: Lude.Maybe Lude.Text,
    awsRegion :: Lude.Maybe Lude.Text,
    relatedEvents :: Lude.Maybe [Lude.Text],
    configuration :: Lude.Maybe Lude.Text,
    configurationItemMD5Hash :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationItem' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit AWS account ID associated with the resource.
-- * 'arn' - accoun
-- * 'availabilityZone' - The Availability Zone associated with the resource.
-- * 'awsRegion' - The region where the resource resides.
-- * 'configuration' - The description of the resource configuration.
-- * 'configurationItemCaptureTime' - The time when the configuration recording was initiated.
-- * 'configurationItemMD5Hash' - Unique MD5 hash that represents the configuration item's state.
--
-- You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
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
-- * 'relatedEvents' - A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> .
-- An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
-- * 'relationships' - A list of related AWS resources.
-- * 'resourceCreationTime' - The time stamp when the resource was created.
-- * 'resourceId' - The ID of the resource (for example, @sg-xxxxxx@ ).
-- * 'resourceName' - The custom name of the resource, if available.
-- * 'resourceType' - The type of AWS resource.
-- * 'supplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
-- * 'tags' - A mapping of key value tags associated with the resource.
-- * 'version' - The version number of the resource configuration.
mkConfigurationItem ::
  ConfigurationItem
mkConfigurationItem =
  ConfigurationItem'
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
      relationships = Lude.Nothing,
      version = Lude.Nothing,
      awsRegion = Lude.Nothing,
      relatedEvents = Lude.Nothing,
      configuration = Lude.Nothing,
      configurationItemMD5Hash = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceId :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciResourceId = Lens.lens (resourceId :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ConfigurationItem)
{-# DEPRECATED ciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceType :: Lens.Lens' ConfigurationItem (Lude.Maybe ResourceType)
ciResourceType = Lens.lens (resourceType :: ConfigurationItem -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ConfigurationItem)
{-# DEPRECATED ciResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An identifier that indicates the ordering of the configuration items of a resource.
--
-- /Note:/ Consider using 'configurationStateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationStateId :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciConfigurationStateId = Lens.lens (configurationStateId :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {configurationStateId = a} :: ConfigurationItem)
{-# DEPRECATED ciConfigurationStateId "Use generic-lens or generic-optics with 'configurationStateId' instead." #-}

-- | accoun
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciArn :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciArn = Lens.lens (arn :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ConfigurationItem)
{-# DEPRECATED ciArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The custom name of the resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceName :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciResourceName = Lens.lens (resourceName :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ConfigurationItem)
{-# DEPRECATED ciResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The time stamp when the resource was created.
--
-- /Note:/ Consider using 'resourceCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceCreationTime :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Timestamp)
ciResourceCreationTime = Lens.lens (resourceCreationTime :: ConfigurationItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {resourceCreationTime = a} :: ConfigurationItem)
{-# DEPRECATED ciResourceCreationTime "Use generic-lens or generic-optics with 'resourceCreationTime' instead." #-}

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
ciConfigurationItemStatus :: Lens.Lens' ConfigurationItem (Lude.Maybe ConfigurationItemStatus)
ciConfigurationItemStatus = Lens.lens (configurationItemStatus :: ConfigurationItem -> Lude.Maybe ConfigurationItemStatus) (\s a -> s {configurationItemStatus = a} :: ConfigurationItem)
{-# DEPRECATED ciConfigurationItemStatus "Use generic-lens or generic-optics with 'configurationItemStatus' instead." #-}

-- | The time when the configuration recording was initiated.
--
-- /Note:/ Consider using 'configurationItemCaptureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationItemCaptureTime :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Timestamp)
ciConfigurationItemCaptureTime = Lens.lens (configurationItemCaptureTime :: ConfigurationItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {configurationItemCaptureTime = a} :: ConfigurationItem)
{-# DEPRECATED ciConfigurationItemCaptureTime "Use generic-lens or generic-optics with 'configurationItemCaptureTime' instead." #-}

-- | The 12-digit AWS account ID associated with the resource.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAccountId :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciAccountId = Lens.lens (accountId :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ConfigurationItem)
{-# DEPRECATED ciAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
--
-- /Note:/ Consider using 'supplementaryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSupplementaryConfiguration :: Lens.Lens' ConfigurationItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ciSupplementaryConfiguration = Lens.lens (supplementaryConfiguration :: ConfigurationItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {supplementaryConfiguration = a} :: ConfigurationItem)
{-# DEPRECATED ciSupplementaryConfiguration "Use generic-lens or generic-optics with 'supplementaryConfiguration' instead." #-}

-- | The Availability Zone associated with the resource.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciAvailabilityZone = Lens.lens (availabilityZone :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ConfigurationItem)
{-# DEPRECATED ciAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of related AWS resources.
--
-- /Note:/ Consider using 'relationships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRelationships :: Lens.Lens' ConfigurationItem (Lude.Maybe [Relationship])
ciRelationships = Lens.lens (relationships :: ConfigurationItem -> Lude.Maybe [Relationship]) (\s a -> s {relationships = a} :: ConfigurationItem)
{-# DEPRECATED ciRelationships "Use generic-lens or generic-optics with 'relationships' instead." #-}

-- | The version number of the resource configuration.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersion :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciVersion = Lens.lens (version :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: ConfigurationItem)
{-# DEPRECATED ciVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The region where the resource resides.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAwsRegion :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciAwsRegion = Lens.lens (awsRegion :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: ConfigurationItem)
{-# DEPRECATED ciAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> .
-- An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
--
-- /Note:/ Consider using 'relatedEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRelatedEvents :: Lens.Lens' ConfigurationItem (Lude.Maybe [Lude.Text])
ciRelatedEvents = Lens.lens (relatedEvents :: ConfigurationItem -> Lude.Maybe [Lude.Text]) (\s a -> s {relatedEvents = a} :: ConfigurationItem)
{-# DEPRECATED ciRelatedEvents "Use generic-lens or generic-optics with 'relatedEvents' instead." #-}

-- | The description of the resource configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfiguration :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciConfiguration = Lens.lens (configuration :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: ConfigurationItem)
{-# DEPRECATED ciConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Unique MD5 hash that represents the configuration item's state.
--
-- You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
--
-- /Note:/ Consider using 'configurationItemMD5Hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationItemMD5Hash :: Lens.Lens' ConfigurationItem (Lude.Maybe Lude.Text)
ciConfigurationItemMD5Hash = Lens.lens (configurationItemMD5Hash :: ConfigurationItem -> Lude.Maybe Lude.Text) (\s a -> s {configurationItemMD5Hash = a} :: ConfigurationItem)
{-# DEPRECATED ciConfigurationItemMD5Hash "Use generic-lens or generic-optics with 'configurationItemMD5Hash' instead." #-}

-- | A mapping of key value tags associated with the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' ConfigurationItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ciTags = Lens.lens (tags :: ConfigurationItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ConfigurationItem)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ConfigurationItem where
  parseJSON =
    Lude.withObject
      "ConfigurationItem"
      ( \x ->
          ConfigurationItem'
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
            Lude.<*> (x Lude..:? "relationships" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "awsRegion")
            Lude.<*> (x Lude..:? "relatedEvents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "configuration")
            Lude.<*> (x Lude..:? "configurationItemMD5Hash")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
