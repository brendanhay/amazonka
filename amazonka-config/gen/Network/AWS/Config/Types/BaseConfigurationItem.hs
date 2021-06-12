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
-- Module      : Network.AWS.Config.Types.BaseConfigurationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.BaseConfigurationItem where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The detailed configuration of a specified resource.
--
-- /See:/ 'newBaseConfigurationItem' smart constructor.
data BaseConfigurationItem = BaseConfigurationItem'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Core.Maybe Core.Text,
    -- | The 12-digit AWS account ID associated with the resource.
    accountId :: Core.Maybe Core.Text,
    -- | The description of the resource configuration.
    configuration :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Core.Maybe Core.Text,
    -- | The version number of the resource configuration.
    version :: Core.Maybe Core.Text,
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Core.Maybe Core.Text,
    -- | The type of AWS resource.
    resourceType :: Core.Maybe ResourceType,
    -- | Configuration attributes that AWS Config returns for certain resource
    -- types to supplement the information returned for the configuration
    -- parameter.
    supplementaryConfiguration :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Availability Zone associated with the resource.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The time when the configuration recording was initiated.
    configurationItemCaptureTime :: Core.Maybe Core.POSIX,
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
    configurationItemStatus :: Core.Maybe ConfigurationItemStatus,
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Core.Maybe Core.POSIX,
    -- | The region where the resource resides.
    awsRegion :: Core.Maybe Core.Text,
    -- | The custom name of the resource, if available.
    resourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BaseConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'baseConfigurationItem_resourceId' - The ID of the resource (for example., sg-xxxxxx).
--
-- 'accountId', 'baseConfigurationItem_accountId' - The 12-digit AWS account ID associated with the resource.
--
-- 'configuration', 'baseConfigurationItem_configuration' - The description of the resource configuration.
--
-- 'arn', 'baseConfigurationItem_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'version', 'baseConfigurationItem_version' - The version number of the resource configuration.
--
-- 'configurationStateId', 'baseConfigurationItem_configurationStateId' - An identifier that indicates the ordering of the configuration items of
-- a resource.
--
-- 'resourceType', 'baseConfigurationItem_resourceType' - The type of AWS resource.
--
-- 'supplementaryConfiguration', 'baseConfigurationItem_supplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource
-- types to supplement the information returned for the configuration
-- parameter.
--
-- 'availabilityZone', 'baseConfigurationItem_availabilityZone' - The Availability Zone associated with the resource.
--
-- 'configurationItemCaptureTime', 'baseConfigurationItem_configurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- 'configurationItemStatus', 'baseConfigurationItem_configurationItemStatus' - The configuration item status. The valid values are:
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
-- 'resourceCreationTime', 'baseConfigurationItem_resourceCreationTime' - The time stamp when the resource was created.
--
-- 'awsRegion', 'baseConfigurationItem_awsRegion' - The region where the resource resides.
--
-- 'resourceName', 'baseConfigurationItem_resourceName' - The custom name of the resource, if available.
newBaseConfigurationItem ::
  BaseConfigurationItem
newBaseConfigurationItem =
  BaseConfigurationItem'
    { resourceId = Core.Nothing,
      accountId = Core.Nothing,
      configuration = Core.Nothing,
      arn = Core.Nothing,
      version = Core.Nothing,
      configurationStateId = Core.Nothing,
      resourceType = Core.Nothing,
      supplementaryConfiguration = Core.Nothing,
      availabilityZone = Core.Nothing,
      configurationItemCaptureTime = Core.Nothing,
      configurationItemStatus = Core.Nothing,
      resourceCreationTime = Core.Nothing,
      awsRegion = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
baseConfigurationItem_resourceId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_resourceId = Lens.lens (\BaseConfigurationItem' {resourceId} -> resourceId) (\s@BaseConfigurationItem' {} a -> s {resourceId = a} :: BaseConfigurationItem)

-- | The 12-digit AWS account ID associated with the resource.
baseConfigurationItem_accountId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_accountId = Lens.lens (\BaseConfigurationItem' {accountId} -> accountId) (\s@BaseConfigurationItem' {} a -> s {accountId = a} :: BaseConfigurationItem)

-- | The description of the resource configuration.
baseConfigurationItem_configuration :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_configuration = Lens.lens (\BaseConfigurationItem' {configuration} -> configuration) (\s@BaseConfigurationItem' {} a -> s {configuration = a} :: BaseConfigurationItem)

-- | The Amazon Resource Name (ARN) of the resource.
baseConfigurationItem_arn :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_arn = Lens.lens (\BaseConfigurationItem' {arn} -> arn) (\s@BaseConfigurationItem' {} a -> s {arn = a} :: BaseConfigurationItem)

-- | The version number of the resource configuration.
baseConfigurationItem_version :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_version = Lens.lens (\BaseConfigurationItem' {version} -> version) (\s@BaseConfigurationItem' {} a -> s {version = a} :: BaseConfigurationItem)

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
baseConfigurationItem_configurationStateId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_configurationStateId = Lens.lens (\BaseConfigurationItem' {configurationStateId} -> configurationStateId) (\s@BaseConfigurationItem' {} a -> s {configurationStateId = a} :: BaseConfigurationItem)

-- | The type of AWS resource.
baseConfigurationItem_resourceType :: Lens.Lens' BaseConfigurationItem (Core.Maybe ResourceType)
baseConfigurationItem_resourceType = Lens.lens (\BaseConfigurationItem' {resourceType} -> resourceType) (\s@BaseConfigurationItem' {} a -> s {resourceType = a} :: BaseConfigurationItem)

-- | Configuration attributes that AWS Config returns for certain resource
-- types to supplement the information returned for the configuration
-- parameter.
baseConfigurationItem_supplementaryConfiguration :: Lens.Lens' BaseConfigurationItem (Core.Maybe (Core.HashMap Core.Text Core.Text))
baseConfigurationItem_supplementaryConfiguration = Lens.lens (\BaseConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@BaseConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: BaseConfigurationItem) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone associated with the resource.
baseConfigurationItem_availabilityZone :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_availabilityZone = Lens.lens (\BaseConfigurationItem' {availabilityZone} -> availabilityZone) (\s@BaseConfigurationItem' {} a -> s {availabilityZone = a} :: BaseConfigurationItem)

-- | The time when the configuration recording was initiated.
baseConfigurationItem_configurationItemCaptureTime :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.UTCTime)
baseConfigurationItem_configurationItemCaptureTime = Lens.lens (\BaseConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@BaseConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: BaseConfigurationItem) Core.. Lens.mapping Core._Time

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
baseConfigurationItem_configurationItemStatus :: Lens.Lens' BaseConfigurationItem (Core.Maybe ConfigurationItemStatus)
baseConfigurationItem_configurationItemStatus = Lens.lens (\BaseConfigurationItem' {configurationItemStatus} -> configurationItemStatus) (\s@BaseConfigurationItem' {} a -> s {configurationItemStatus = a} :: BaseConfigurationItem)

-- | The time stamp when the resource was created.
baseConfigurationItem_resourceCreationTime :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.UTCTime)
baseConfigurationItem_resourceCreationTime = Lens.lens (\BaseConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@BaseConfigurationItem' {} a -> s {resourceCreationTime = a} :: BaseConfigurationItem) Core.. Lens.mapping Core._Time

-- | The region where the resource resides.
baseConfigurationItem_awsRegion :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_awsRegion = Lens.lens (\BaseConfigurationItem' {awsRegion} -> awsRegion) (\s@BaseConfigurationItem' {} a -> s {awsRegion = a} :: BaseConfigurationItem)

-- | The custom name of the resource, if available.
baseConfigurationItem_resourceName :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.Text)
baseConfigurationItem_resourceName = Lens.lens (\BaseConfigurationItem' {resourceName} -> resourceName) (\s@BaseConfigurationItem' {} a -> s {resourceName = a} :: BaseConfigurationItem)

instance Core.FromJSON BaseConfigurationItem where
  parseJSON =
    Core.withObject
      "BaseConfigurationItem"
      ( \x ->
          BaseConfigurationItem'
            Core.<$> (x Core..:? "resourceId")
            Core.<*> (x Core..:? "accountId")
            Core.<*> (x Core..:? "configuration")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "configurationStateId")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> ( x Core..:? "supplementaryConfiguration"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "availabilityZone")
            Core.<*> (x Core..:? "configurationItemCaptureTime")
            Core.<*> (x Core..:? "configurationItemStatus")
            Core.<*> (x Core..:? "resourceCreationTime")
            Core.<*> (x Core..:? "awsRegion")
            Core.<*> (x Core..:? "resourceName")
      )

instance Core.Hashable BaseConfigurationItem

instance Core.NFData BaseConfigurationItem
