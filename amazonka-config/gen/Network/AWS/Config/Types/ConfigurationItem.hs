{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItem where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list that contains detailed configurations of a specified resource.
--
-- /See:/ 'newConfigurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { -- | A list of related AWS resources.
    relationships :: Prelude.Maybe [Relationship],
    -- | The ID of the resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit AWS account ID associated with the resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A list of CloudTrail event IDs.
    --
    -- A populated field indicates that the current configuration was initiated
    -- by the events recorded in the CloudTrail log. For more information about
    -- CloudTrail, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail>.
    --
    -- An empty field indicates that the current configuration was not
    -- initiated by any event. As of Version 1.3, the relatedEvents field is
    -- empty. You can access the
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
    -- in the /AWS CloudTrail API Reference/ to retrieve the events for the
    -- resource.
    relatedEvents :: Prelude.Maybe [Prelude.Text],
    -- | The description of the resource configuration.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the resource configuration.
    version :: Prelude.Maybe Prelude.Text,
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Configuration attributes that AWS Config returns for certain resource
    -- types to supplement the information returned for the @configuration@
    -- parameter.
    supplementaryConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Availability Zone associated with the resource.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration recording was initiated.
    configurationItemCaptureTime :: Prelude.Maybe Prelude.POSIX,
    -- | The configuration item status. The valid values are:
    --
    -- -   OK – The resource configuration has been updated
    --
    -- -   ResourceDiscovered – The resource was newly discovered
    --
    -- -   ResourceNotRecorded – The resource was discovered but its
    --     configuration was not recorded since the recorder excludes the
    --     recording of resources of this type
    --
    -- -   ResourceDeleted – The resource was deleted
    --
    -- -   ResourceDeletedNotRecorded – The resource was deleted but its
    --     configuration was not recorded since the recorder excludes the
    --     recording of resources of this type
    --
    -- The CIs do not incur any cost.
    configurationItemStatus :: Prelude.Maybe ConfigurationItemStatus,
    -- | A mapping of key value tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Unique MD5 hash that represents the configuration item\'s state.
    --
    -- You can use MD5 hash to compare the states of two or more configuration
    -- items that are associated with the same resource.
    configurationItemMD5Hash :: Prelude.Maybe Prelude.Text,
    -- | The region where the resource resides.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationships', 'configurationItem_relationships' - A list of related AWS resources.
--
-- 'resourceId', 'configurationItem_resourceId' - The ID of the resource (for example, @sg-xxxxxx@).
--
-- 'accountId', 'configurationItem_accountId' - The 12-digit AWS account ID associated with the resource.
--
-- 'relatedEvents', 'configurationItem_relatedEvents' - A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated
-- by the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail>.
--
-- An empty field indicates that the current configuration was not
-- initiated by any event. As of Version 1.3, the relatedEvents field is
-- empty. You can access the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
-- in the /AWS CloudTrail API Reference/ to retrieve the events for the
-- resource.
--
-- 'configuration', 'configurationItem_configuration' - The description of the resource configuration.
--
-- 'arn', 'configurationItem_arn' - Amazon Resource Name (ARN) associated with the resource.
--
-- 'version', 'configurationItem_version' - The version number of the resource configuration.
--
-- 'configurationStateId', 'configurationItem_configurationStateId' - An identifier that indicates the ordering of the configuration items of
-- a resource.
--
-- 'resourceType', 'configurationItem_resourceType' - The type of AWS resource.
--
-- 'supplementaryConfiguration', 'configurationItem_supplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource
-- types to supplement the information returned for the @configuration@
-- parameter.
--
-- 'availabilityZone', 'configurationItem_availabilityZone' - The Availability Zone associated with the resource.
--
-- 'configurationItemCaptureTime', 'configurationItem_configurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- 'configurationItemStatus', 'configurationItem_configurationItemStatus' - The configuration item status. The valid values are:
--
-- -   OK – The resource configuration has been updated
--
-- -   ResourceDiscovered – The resource was newly discovered
--
-- -   ResourceNotRecorded – The resource was discovered but its
--     configuration was not recorded since the recorder excludes the
--     recording of resources of this type
--
-- -   ResourceDeleted – The resource was deleted
--
-- -   ResourceDeletedNotRecorded – The resource was deleted but its
--     configuration was not recorded since the recorder excludes the
--     recording of resources of this type
--
-- The CIs do not incur any cost.
--
-- 'tags', 'configurationItem_tags' - A mapping of key value tags associated with the resource.
--
-- 'resourceCreationTime', 'configurationItem_resourceCreationTime' - The time stamp when the resource was created.
--
-- 'configurationItemMD5Hash', 'configurationItem_configurationItemMD5Hash' - Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
--
-- 'awsRegion', 'configurationItem_awsRegion' - The region where the resource resides.
--
-- 'resourceName', 'configurationItem_resourceName' - The custom name of the resource, if available.
newConfigurationItem ::
  ConfigurationItem
newConfigurationItem =
  ConfigurationItem'
    { relationships = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      relatedEvents = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      version = Prelude.Nothing,
      configurationStateId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supplementaryConfiguration = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      configurationItemCaptureTime = Prelude.Nothing,
      configurationItemStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      resourceCreationTime = Prelude.Nothing,
      configurationItemMD5Hash = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | A list of related AWS resources.
configurationItem_relationships :: Lens.Lens' ConfigurationItem (Prelude.Maybe [Relationship])
configurationItem_relationships = Lens.lens (\ConfigurationItem' {relationships} -> relationships) (\s@ConfigurationItem' {} a -> s {relationships = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the resource (for example, @sg-xxxxxx@).
configurationItem_resourceId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceId = Lens.lens (\ConfigurationItem' {resourceId} -> resourceId) (\s@ConfigurationItem' {} a -> s {resourceId = a} :: ConfigurationItem)

-- | The 12-digit AWS account ID associated with the resource.
configurationItem_accountId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_accountId = Lens.lens (\ConfigurationItem' {accountId} -> accountId) (\s@ConfigurationItem' {} a -> s {accountId = a} :: ConfigurationItem)

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated
-- by the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail>.
--
-- An empty field indicates that the current configuration was not
-- initiated by any event. As of Version 1.3, the relatedEvents field is
-- empty. You can access the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
-- in the /AWS CloudTrail API Reference/ to retrieve the events for the
-- resource.
configurationItem_relatedEvents :: Lens.Lens' ConfigurationItem (Prelude.Maybe [Prelude.Text])
configurationItem_relatedEvents = Lens.lens (\ConfigurationItem' {relatedEvents} -> relatedEvents) (\s@ConfigurationItem' {} a -> s {relatedEvents = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the resource configuration.
configurationItem_configuration :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configuration = Lens.lens (\ConfigurationItem' {configuration} -> configuration) (\s@ConfigurationItem' {} a -> s {configuration = a} :: ConfigurationItem)

-- | Amazon Resource Name (ARN) associated with the resource.
configurationItem_arn :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_arn = Lens.lens (\ConfigurationItem' {arn} -> arn) (\s@ConfigurationItem' {} a -> s {arn = a} :: ConfigurationItem)

-- | The version number of the resource configuration.
configurationItem_version :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_version = Lens.lens (\ConfigurationItem' {version} -> version) (\s@ConfigurationItem' {} a -> s {version = a} :: ConfigurationItem)

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
configurationItem_configurationStateId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationStateId = Lens.lens (\ConfigurationItem' {configurationStateId} -> configurationStateId) (\s@ConfigurationItem' {} a -> s {configurationStateId = a} :: ConfigurationItem)

-- | The type of AWS resource.
configurationItem_resourceType :: Lens.Lens' ConfigurationItem (Prelude.Maybe ResourceType)
configurationItem_resourceType = Lens.lens (\ConfigurationItem' {resourceType} -> resourceType) (\s@ConfigurationItem' {} a -> s {resourceType = a} :: ConfigurationItem)

-- | Configuration attributes that AWS Config returns for certain resource
-- types to supplement the information returned for the @configuration@
-- parameter.
configurationItem_supplementaryConfiguration :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_supplementaryConfiguration = Lens.lens (\ConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@ConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone associated with the resource.
configurationItem_availabilityZone :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_availabilityZone = Lens.lens (\ConfigurationItem' {availabilityZone} -> availabilityZone) (\s@ConfigurationItem' {} a -> s {availabilityZone = a} :: ConfigurationItem)

-- | The time when the configuration recording was initiated.
configurationItem_configurationItemCaptureTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_configurationItemCaptureTime = Lens.lens (\ConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@ConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Time

-- | The configuration item status. The valid values are:
--
-- -   OK – The resource configuration has been updated
--
-- -   ResourceDiscovered – The resource was newly discovered
--
-- -   ResourceNotRecorded – The resource was discovered but its
--     configuration was not recorded since the recorder excludes the
--     recording of resources of this type
--
-- -   ResourceDeleted – The resource was deleted
--
-- -   ResourceDeletedNotRecorded – The resource was deleted but its
--     configuration was not recorded since the recorder excludes the
--     recording of resources of this type
--
-- The CIs do not incur any cost.
configurationItem_configurationItemStatus :: Lens.Lens' ConfigurationItem (Prelude.Maybe ConfigurationItemStatus)
configurationItem_configurationItemStatus = Lens.lens (\ConfigurationItem' {configurationItemStatus} -> configurationItemStatus) (\s@ConfigurationItem' {} a -> s {configurationItemStatus = a} :: ConfigurationItem)

-- | A mapping of key value tags associated with the resource.
configurationItem_tags :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_tags = Lens.lens (\ConfigurationItem' {tags} -> tags) (\s@ConfigurationItem' {} a -> s {tags = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The time stamp when the resource was created.
configurationItem_resourceCreationTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_resourceCreationTime = Lens.lens (\ConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@ConfigurationItem' {} a -> s {resourceCreationTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Prelude._Time

-- | Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
configurationItem_configurationItemMD5Hash :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationItemMD5Hash = Lens.lens (\ConfigurationItem' {configurationItemMD5Hash} -> configurationItemMD5Hash) (\s@ConfigurationItem' {} a -> s {configurationItemMD5Hash = a} :: ConfigurationItem)

-- | The region where the resource resides.
configurationItem_awsRegion :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_awsRegion = Lens.lens (\ConfigurationItem' {awsRegion} -> awsRegion) (\s@ConfigurationItem' {} a -> s {awsRegion = a} :: ConfigurationItem)

-- | The custom name of the resource, if available.
configurationItem_resourceName :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceName = Lens.lens (\ConfigurationItem' {resourceName} -> resourceName) (\s@ConfigurationItem' {} a -> s {resourceName = a} :: ConfigurationItem)

instance Prelude.FromJSON ConfigurationItem where
  parseJSON =
    Prelude.withObject
      "ConfigurationItem"
      ( \x ->
          ConfigurationItem'
            Prelude.<$> ( x Prelude..:? "relationships"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "resourceId")
            Prelude.<*> (x Prelude..:? "accountId")
            Prelude.<*> ( x Prelude..:? "relatedEvents"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "configuration")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "configurationStateId")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> ( x Prelude..:? "supplementaryConfiguration"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "availabilityZone")
            Prelude.<*> (x Prelude..:? "configurationItemCaptureTime")
            Prelude.<*> (x Prelude..:? "configurationItemStatus")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "resourceCreationTime")
            Prelude.<*> (x Prelude..:? "configurationItemMD5Hash")
            Prelude.<*> (x Prelude..:? "awsRegion")
            Prelude.<*> (x Prelude..:? "resourceName")
      )

instance Prelude.Hashable ConfigurationItem

instance Prelude.NFData ConfigurationItem
