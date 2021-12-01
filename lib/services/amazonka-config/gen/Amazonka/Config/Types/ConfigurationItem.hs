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
-- Module      : Amazonka.Config.Types.ConfigurationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationItem where

import Amazonka.Config.Types.ConfigurationItemStatus
import Amazonka.Config.Types.Relationship
import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list that contains detailed configurations of a specified resource.
--
-- /See:/ 'newConfigurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { -- | The ID of the resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Prelude.Maybe Core.POSIX,
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
    -- | The time when the configuration recording was initiated.
    configurationItemCaptureTime :: Prelude.Maybe Core.POSIX,
    -- | The 12-digit Amazon Web Services account ID associated with the
    -- resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Configuration attributes that Config returns for certain resource types
    -- to supplement the information returned for the @configuration@
    -- parameter.
    supplementaryConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Availability Zone associated with the resource.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A list of related Amazon Web Services resources.
    relationships :: Prelude.Maybe [Relationship],
    -- | The version number of the resource configuration.
    version :: Prelude.Maybe Prelude.Text,
    -- | The region where the resource resides.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | A list of CloudTrail event IDs.
    --
    -- A populated field indicates that the current configuration was initiated
    -- by the events recorded in the CloudTrail log. For more information about
    -- CloudTrail, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is CloudTrail>.
    --
    -- An empty field indicates that the current configuration was not
    -- initiated by any event. As of Version 1.3, the relatedEvents field is
    -- empty. You can access the
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
    -- in the /CloudTrail API Reference/ to retrieve the events for the
    -- resource.
    relatedEvents :: Prelude.Maybe [Prelude.Text],
    -- | The description of the resource configuration.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | Unique MD5 hash that represents the configuration item\'s state.
    --
    -- You can use MD5 hash to compare the states of two or more configuration
    -- items that are associated with the same resource.
    configurationItemMD5Hash :: Prelude.Maybe Prelude.Text,
    -- | A mapping of key value tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'configurationItem_resourceId' - The ID of the resource (for example, @sg-xxxxxx@).
--
-- 'resourceType', 'configurationItem_resourceType' - The type of Amazon Web Services resource.
--
-- 'configurationStateId', 'configurationItem_configurationStateId' - An identifier that indicates the ordering of the configuration items of
-- a resource.
--
-- 'arn', 'configurationItem_arn' - Amazon Resource Name (ARN) associated with the resource.
--
-- 'resourceName', 'configurationItem_resourceName' - The custom name of the resource, if available.
--
-- 'resourceCreationTime', 'configurationItem_resourceCreationTime' - The time stamp when the resource was created.
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
-- 'configurationItemCaptureTime', 'configurationItem_configurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- 'accountId', 'configurationItem_accountId' - The 12-digit Amazon Web Services account ID associated with the
-- resource.
--
-- 'supplementaryConfiguration', 'configurationItem_supplementaryConfiguration' - Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the @configuration@
-- parameter.
--
-- 'availabilityZone', 'configurationItem_availabilityZone' - The Availability Zone associated with the resource.
--
-- 'relationships', 'configurationItem_relationships' - A list of related Amazon Web Services resources.
--
-- 'version', 'configurationItem_version' - The version number of the resource configuration.
--
-- 'awsRegion', 'configurationItem_awsRegion' - The region where the resource resides.
--
-- 'relatedEvents', 'configurationItem_relatedEvents' - A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated
-- by the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is CloudTrail>.
--
-- An empty field indicates that the current configuration was not
-- initiated by any event. As of Version 1.3, the relatedEvents field is
-- empty. You can access the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
-- in the /CloudTrail API Reference/ to retrieve the events for the
-- resource.
--
-- 'configuration', 'configurationItem_configuration' - The description of the resource configuration.
--
-- 'configurationItemMD5Hash', 'configurationItem_configurationItemMD5Hash' - Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
--
-- 'tags', 'configurationItem_tags' - A mapping of key value tags associated with the resource.
newConfigurationItem ::
  ConfigurationItem
newConfigurationItem =
  ConfigurationItem'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      configurationStateId = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceCreationTime = Prelude.Nothing,
      configurationItemStatus = Prelude.Nothing,
      configurationItemCaptureTime = Prelude.Nothing,
      accountId = Prelude.Nothing,
      supplementaryConfiguration = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      relationships = Prelude.Nothing,
      version = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      relatedEvents = Prelude.Nothing,
      configuration = Prelude.Nothing,
      configurationItemMD5Hash = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@).
configurationItem_resourceId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceId = Lens.lens (\ConfigurationItem' {resourceId} -> resourceId) (\s@ConfigurationItem' {} a -> s {resourceId = a} :: ConfigurationItem)

-- | The type of Amazon Web Services resource.
configurationItem_resourceType :: Lens.Lens' ConfigurationItem (Prelude.Maybe ResourceType)
configurationItem_resourceType = Lens.lens (\ConfigurationItem' {resourceType} -> resourceType) (\s@ConfigurationItem' {} a -> s {resourceType = a} :: ConfigurationItem)

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
configurationItem_configurationStateId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationStateId = Lens.lens (\ConfigurationItem' {configurationStateId} -> configurationStateId) (\s@ConfigurationItem' {} a -> s {configurationStateId = a} :: ConfigurationItem)

-- | Amazon Resource Name (ARN) associated with the resource.
configurationItem_arn :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_arn = Lens.lens (\ConfigurationItem' {arn} -> arn) (\s@ConfigurationItem' {} a -> s {arn = a} :: ConfigurationItem)

-- | The custom name of the resource, if available.
configurationItem_resourceName :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceName = Lens.lens (\ConfigurationItem' {resourceName} -> resourceName) (\s@ConfigurationItem' {} a -> s {resourceName = a} :: ConfigurationItem)

-- | The time stamp when the resource was created.
configurationItem_resourceCreationTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_resourceCreationTime = Lens.lens (\ConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@ConfigurationItem' {} a -> s {resourceCreationTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Core._Time

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

-- | The time when the configuration recording was initiated.
configurationItem_configurationItemCaptureTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_configurationItemCaptureTime = Lens.lens (\ConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@ConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Core._Time

-- | The 12-digit Amazon Web Services account ID associated with the
-- resource.
configurationItem_accountId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_accountId = Lens.lens (\ConfigurationItem' {accountId} -> accountId) (\s@ConfigurationItem' {} a -> s {accountId = a} :: ConfigurationItem)

-- | Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the @configuration@
-- parameter.
configurationItem_supplementaryConfiguration :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_supplementaryConfiguration = Lens.lens (\ConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@ConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone associated with the resource.
configurationItem_availabilityZone :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_availabilityZone = Lens.lens (\ConfigurationItem' {availabilityZone} -> availabilityZone) (\s@ConfigurationItem' {} a -> s {availabilityZone = a} :: ConfigurationItem)

-- | A list of related Amazon Web Services resources.
configurationItem_relationships :: Lens.Lens' ConfigurationItem (Prelude.Maybe [Relationship])
configurationItem_relationships = Lens.lens (\ConfigurationItem' {relationships} -> relationships) (\s@ConfigurationItem' {} a -> s {relationships = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the resource configuration.
configurationItem_version :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_version = Lens.lens (\ConfigurationItem' {version} -> version) (\s@ConfigurationItem' {} a -> s {version = a} :: ConfigurationItem)

-- | The region where the resource resides.
configurationItem_awsRegion :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_awsRegion = Lens.lens (\ConfigurationItem' {awsRegion} -> awsRegion) (\s@ConfigurationItem' {} a -> s {awsRegion = a} :: ConfigurationItem)

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated
-- by the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is CloudTrail>.
--
-- An empty field indicates that the current configuration was not
-- initiated by any event. As of Version 1.3, the relatedEvents field is
-- empty. You can access the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API>
-- in the /CloudTrail API Reference/ to retrieve the events for the
-- resource.
configurationItem_relatedEvents :: Lens.Lens' ConfigurationItem (Prelude.Maybe [Prelude.Text])
configurationItem_relatedEvents = Lens.lens (\ConfigurationItem' {relatedEvents} -> relatedEvents) (\s@ConfigurationItem' {} a -> s {relatedEvents = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | The description of the resource configuration.
configurationItem_configuration :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configuration = Lens.lens (\ConfigurationItem' {configuration} -> configuration) (\s@ConfigurationItem' {} a -> s {configuration = a} :: ConfigurationItem)

-- | Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
configurationItem_configurationItemMD5Hash :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationItemMD5Hash = Lens.lens (\ConfigurationItem' {configurationItemMD5Hash} -> configurationItemMD5Hash) (\s@ConfigurationItem' {} a -> s {configurationItemMD5Hash = a} :: ConfigurationItem)

-- | A mapping of key value tags associated with the resource.
configurationItem_tags :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_tags = Lens.lens (\ConfigurationItem' {tags} -> tags) (\s@ConfigurationItem' {} a -> s {tags = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConfigurationItem where
  parseJSON =
    Core.withObject
      "ConfigurationItem"
      ( \x ->
          ConfigurationItem'
            Prelude.<$> (x Core..:? "resourceId")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "configurationStateId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "resourceName")
            Prelude.<*> (x Core..:? "resourceCreationTime")
            Prelude.<*> (x Core..:? "configurationItemStatus")
            Prelude.<*> (x Core..:? "configurationItemCaptureTime")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> ( x Core..:? "supplementaryConfiguration"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "availabilityZone")
            Prelude.<*> (x Core..:? "relationships" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "awsRegion")
            Prelude.<*> (x Core..:? "relatedEvents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "configuration")
            Prelude.<*> (x Core..:? "configurationItemMD5Hash")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ConfigurationItem where
  hashWithSalt salt' ConfigurationItem' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configurationItemMD5Hash
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` relatedEvents
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` relationships
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` supplementaryConfiguration
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` configurationItemCaptureTime
      `Prelude.hashWithSalt` configurationItemStatus
      `Prelude.hashWithSalt` resourceCreationTime
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` configurationStateId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ConfigurationItem where
  rnf ConfigurationItem' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configurationItemMD5Hash
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf relatedEvents
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf supplementaryConfiguration
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf configurationItemCaptureTime
      `Prelude.seq` Prelude.rnf configurationItemStatus
      `Prelude.seq` Prelude.rnf resourceCreationTime
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf configurationStateId
      `Prelude.seq` Prelude.rnf resourceType
