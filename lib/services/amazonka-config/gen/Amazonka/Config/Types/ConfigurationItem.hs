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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationItem where

import Amazonka.Config.Types.ConfigurationItemStatus
import Amazonka.Config.Types.Relationship
import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list that contains detailed configurations of a specified resource.
--
-- /See:/ 'newConfigurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { -- | The 12-digit Amazon Web Services account ID associated with the
    -- resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone associated with the resource.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The region where the resource resides.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource configuration.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration recording was initiated.
    configurationItemCaptureTime :: Prelude.Maybe Data.POSIX,
    -- | Unique MD5 hash that represents the configuration item\'s state.
    --
    -- You can use MD5 hash to compare the states of two or more configuration
    -- items that are associated with the same resource.
    configurationItemMD5Hash :: Prelude.Maybe Prelude.Text,
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
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Prelude.Maybe Prelude.Text,
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
    -- | A list of related Amazon Web Services resources.
    relationships :: Prelude.Maybe [Relationship],
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Configuration attributes that Config returns for certain resource types
    -- to supplement the information returned for the @configuration@
    -- parameter.
    supplementaryConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A mapping of key value tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version number of the resource configuration.
    version :: Prelude.Maybe Prelude.Text
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
-- 'accountId', 'configurationItem_accountId' - The 12-digit Amazon Web Services account ID associated with the
-- resource.
--
-- 'arn', 'configurationItem_arn' - Amazon Resource Name (ARN) associated with the resource.
--
-- 'availabilityZone', 'configurationItem_availabilityZone' - The Availability Zone associated with the resource.
--
-- 'awsRegion', 'configurationItem_awsRegion' - The region where the resource resides.
--
-- 'configuration', 'configurationItem_configuration' - The description of the resource configuration.
--
-- 'configurationItemCaptureTime', 'configurationItem_configurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- 'configurationItemMD5Hash', 'configurationItem_configurationItemMD5Hash' - Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
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
-- 'configurationStateId', 'configurationItem_configurationStateId' - An identifier that indicates the ordering of the configuration items of
-- a resource.
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
-- 'relationships', 'configurationItem_relationships' - A list of related Amazon Web Services resources.
--
-- 'resourceCreationTime', 'configurationItem_resourceCreationTime' - The time stamp when the resource was created.
--
-- 'resourceId', 'configurationItem_resourceId' - The ID of the resource (for example, @sg-xxxxxx@).
--
-- 'resourceName', 'configurationItem_resourceName' - The custom name of the resource, if available.
--
-- 'resourceType', 'configurationItem_resourceType' - The type of Amazon Web Services resource.
--
-- 'supplementaryConfiguration', 'configurationItem_supplementaryConfiguration' - Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the @configuration@
-- parameter.
--
-- 'tags', 'configurationItem_tags' - A mapping of key value tags associated with the resource.
--
-- 'version', 'configurationItem_version' - The version number of the resource configuration.
newConfigurationItem ::
  ConfigurationItem
newConfigurationItem =
  ConfigurationItem'
    { accountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      configuration = Prelude.Nothing,
      configurationItemCaptureTime = Prelude.Nothing,
      configurationItemMD5Hash = Prelude.Nothing,
      configurationItemStatus = Prelude.Nothing,
      configurationStateId = Prelude.Nothing,
      relatedEvents = Prelude.Nothing,
      relationships = Prelude.Nothing,
      resourceCreationTime = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supplementaryConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account ID associated with the
-- resource.
configurationItem_accountId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_accountId = Lens.lens (\ConfigurationItem' {accountId} -> accountId) (\s@ConfigurationItem' {} a -> s {accountId = a} :: ConfigurationItem)

-- | Amazon Resource Name (ARN) associated with the resource.
configurationItem_arn :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_arn = Lens.lens (\ConfigurationItem' {arn} -> arn) (\s@ConfigurationItem' {} a -> s {arn = a} :: ConfigurationItem)

-- | The Availability Zone associated with the resource.
configurationItem_availabilityZone :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_availabilityZone = Lens.lens (\ConfigurationItem' {availabilityZone} -> availabilityZone) (\s@ConfigurationItem' {} a -> s {availabilityZone = a} :: ConfigurationItem)

-- | The region where the resource resides.
configurationItem_awsRegion :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_awsRegion = Lens.lens (\ConfigurationItem' {awsRegion} -> awsRegion) (\s@ConfigurationItem' {} a -> s {awsRegion = a} :: ConfigurationItem)

-- | The description of the resource configuration.
configurationItem_configuration :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configuration = Lens.lens (\ConfigurationItem' {configuration} -> configuration) (\s@ConfigurationItem' {} a -> s {configuration = a} :: ConfigurationItem)

-- | The time when the configuration recording was initiated.
configurationItem_configurationItemCaptureTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_configurationItemCaptureTime = Lens.lens (\ConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@ConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Data._Time

-- | Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
configurationItem_configurationItemMD5Hash :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationItemMD5Hash = Lens.lens (\ConfigurationItem' {configurationItemMD5Hash} -> configurationItemMD5Hash) (\s@ConfigurationItem' {} a -> s {configurationItemMD5Hash = a} :: ConfigurationItem)

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

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
configurationItem_configurationStateId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_configurationStateId = Lens.lens (\ConfigurationItem' {configurationStateId} -> configurationStateId) (\s@ConfigurationItem' {} a -> s {configurationStateId = a} :: ConfigurationItem)

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

-- | A list of related Amazon Web Services resources.
configurationItem_relationships :: Lens.Lens' ConfigurationItem (Prelude.Maybe [Relationship])
configurationItem_relationships = Lens.lens (\ConfigurationItem' {relationships} -> relationships) (\s@ConfigurationItem' {} a -> s {relationships = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | The time stamp when the resource was created.
configurationItem_resourceCreationTime :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.UTCTime)
configurationItem_resourceCreationTime = Lens.lens (\ConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@ConfigurationItem' {} a -> s {resourceCreationTime = a} :: ConfigurationItem) Prelude.. Lens.mapping Data._Time

-- | The ID of the resource (for example, @sg-xxxxxx@).
configurationItem_resourceId :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceId = Lens.lens (\ConfigurationItem' {resourceId} -> resourceId) (\s@ConfigurationItem' {} a -> s {resourceId = a} :: ConfigurationItem)

-- | The custom name of the resource, if available.
configurationItem_resourceName :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_resourceName = Lens.lens (\ConfigurationItem' {resourceName} -> resourceName) (\s@ConfigurationItem' {} a -> s {resourceName = a} :: ConfigurationItem)

-- | The type of Amazon Web Services resource.
configurationItem_resourceType :: Lens.Lens' ConfigurationItem (Prelude.Maybe ResourceType)
configurationItem_resourceType = Lens.lens (\ConfigurationItem' {resourceType} -> resourceType) (\s@ConfigurationItem' {} a -> s {resourceType = a} :: ConfigurationItem)

-- | Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the @configuration@
-- parameter.
configurationItem_supplementaryConfiguration :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_supplementaryConfiguration = Lens.lens (\ConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@ConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | A mapping of key value tags associated with the resource.
configurationItem_tags :: Lens.Lens' ConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationItem_tags = Lens.lens (\ConfigurationItem' {tags} -> tags) (\s@ConfigurationItem' {} a -> s {tags = a} :: ConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the resource configuration.
configurationItem_version :: Lens.Lens' ConfigurationItem (Prelude.Maybe Prelude.Text)
configurationItem_version = Lens.lens (\ConfigurationItem' {version} -> version) (\s@ConfigurationItem' {} a -> s {version = a} :: ConfigurationItem)

instance Data.FromJSON ConfigurationItem where
  parseJSON =
    Data.withObject
      "ConfigurationItem"
      ( \x ->
          ConfigurationItem'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "availabilityZone")
            Prelude.<*> (x Data..:? "awsRegion")
            Prelude.<*> (x Data..:? "configuration")
            Prelude.<*> (x Data..:? "configurationItemCaptureTime")
            Prelude.<*> (x Data..:? "configurationItemMD5Hash")
            Prelude.<*> (x Data..:? "configurationItemStatus")
            Prelude.<*> (x Data..:? "configurationStateId")
            Prelude.<*> (x Data..:? "relatedEvents" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "relationships" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceCreationTime")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> ( x
                            Data..:? "supplementaryConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable ConfigurationItem where
  hashWithSalt _salt ConfigurationItem' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` configurationItemCaptureTime
      `Prelude.hashWithSalt` configurationItemMD5Hash
      `Prelude.hashWithSalt` configurationItemStatus
      `Prelude.hashWithSalt` configurationStateId
      `Prelude.hashWithSalt` relatedEvents
      `Prelude.hashWithSalt` relationships
      `Prelude.hashWithSalt` resourceCreationTime
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` supplementaryConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` version

instance Prelude.NFData ConfigurationItem where
  rnf ConfigurationItem' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf configurationItemCaptureTime
      `Prelude.seq` Prelude.rnf configurationItemMD5Hash
      `Prelude.seq` Prelude.rnf configurationItemStatus
      `Prelude.seq` Prelude.rnf configurationStateId
      `Prelude.seq` Prelude.rnf relatedEvents
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf resourceCreationTime
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf supplementaryConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
