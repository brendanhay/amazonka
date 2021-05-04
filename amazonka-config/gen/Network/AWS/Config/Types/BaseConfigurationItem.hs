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
-- Module      : Network.AWS.Config.Types.BaseConfigurationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.BaseConfigurationItem where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detailed configuration of a specified resource.
--
-- /See:/ 'newBaseConfigurationItem' smart constructor.
data BaseConfigurationItem = BaseConfigurationItem'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit AWS account ID associated with the resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource configuration.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the resource configuration.
    version :: Prelude.Maybe Prelude.Text,
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Configuration attributes that AWS Config returns for certain resource
    -- types to supplement the information returned for the configuration
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
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The region where the resource resides.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      version = Prelude.Nothing,
      configurationStateId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supplementaryConfiguration = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      configurationItemCaptureTime = Prelude.Nothing,
      configurationItemStatus = Prelude.Nothing,
      resourceCreationTime = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
baseConfigurationItem_resourceId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_resourceId = Lens.lens (\BaseConfigurationItem' {resourceId} -> resourceId) (\s@BaseConfigurationItem' {} a -> s {resourceId = a} :: BaseConfigurationItem)

-- | The 12-digit AWS account ID associated with the resource.
baseConfigurationItem_accountId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_accountId = Lens.lens (\BaseConfigurationItem' {accountId} -> accountId) (\s@BaseConfigurationItem' {} a -> s {accountId = a} :: BaseConfigurationItem)

-- | The description of the resource configuration.
baseConfigurationItem_configuration :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_configuration = Lens.lens (\BaseConfigurationItem' {configuration} -> configuration) (\s@BaseConfigurationItem' {} a -> s {configuration = a} :: BaseConfigurationItem)

-- | The Amazon Resource Name (ARN) of the resource.
baseConfigurationItem_arn :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_arn = Lens.lens (\BaseConfigurationItem' {arn} -> arn) (\s@BaseConfigurationItem' {} a -> s {arn = a} :: BaseConfigurationItem)

-- | The version number of the resource configuration.
baseConfigurationItem_version :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_version = Lens.lens (\BaseConfigurationItem' {version} -> version) (\s@BaseConfigurationItem' {} a -> s {version = a} :: BaseConfigurationItem)

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
baseConfigurationItem_configurationStateId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_configurationStateId = Lens.lens (\BaseConfigurationItem' {configurationStateId} -> configurationStateId) (\s@BaseConfigurationItem' {} a -> s {configurationStateId = a} :: BaseConfigurationItem)

-- | The type of AWS resource.
baseConfigurationItem_resourceType :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe ResourceType)
baseConfigurationItem_resourceType = Lens.lens (\BaseConfigurationItem' {resourceType} -> resourceType) (\s@BaseConfigurationItem' {} a -> s {resourceType = a} :: BaseConfigurationItem)

-- | Configuration attributes that AWS Config returns for certain resource
-- types to supplement the information returned for the configuration
-- parameter.
baseConfigurationItem_supplementaryConfiguration :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
baseConfigurationItem_supplementaryConfiguration = Lens.lens (\BaseConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@BaseConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone associated with the resource.
baseConfigurationItem_availabilityZone :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_availabilityZone = Lens.lens (\BaseConfigurationItem' {availabilityZone} -> availabilityZone) (\s@BaseConfigurationItem' {} a -> s {availabilityZone = a} :: BaseConfigurationItem)

-- | The time when the configuration recording was initiated.
baseConfigurationItem_configurationItemCaptureTime :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.UTCTime)
baseConfigurationItem_configurationItemCaptureTime = Lens.lens (\BaseConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@BaseConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Prelude._Time

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
baseConfigurationItem_configurationItemStatus :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe ConfigurationItemStatus)
baseConfigurationItem_configurationItemStatus = Lens.lens (\BaseConfigurationItem' {configurationItemStatus} -> configurationItemStatus) (\s@BaseConfigurationItem' {} a -> s {configurationItemStatus = a} :: BaseConfigurationItem)

-- | The time stamp when the resource was created.
baseConfigurationItem_resourceCreationTime :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.UTCTime)
baseConfigurationItem_resourceCreationTime = Lens.lens (\BaseConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@BaseConfigurationItem' {} a -> s {resourceCreationTime = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Prelude._Time

-- | The region where the resource resides.
baseConfigurationItem_awsRegion :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_awsRegion = Lens.lens (\BaseConfigurationItem' {awsRegion} -> awsRegion) (\s@BaseConfigurationItem' {} a -> s {awsRegion = a} :: BaseConfigurationItem)

-- | The custom name of the resource, if available.
baseConfigurationItem_resourceName :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_resourceName = Lens.lens (\BaseConfigurationItem' {resourceName} -> resourceName) (\s@BaseConfigurationItem' {} a -> s {resourceName = a} :: BaseConfigurationItem)

instance Prelude.FromJSON BaseConfigurationItem where
  parseJSON =
    Prelude.withObject
      "BaseConfigurationItem"
      ( \x ->
          BaseConfigurationItem'
            Prelude.<$> (x Prelude..:? "resourceId")
            Prelude.<*> (x Prelude..:? "accountId")
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
            Prelude.<*> (x Prelude..:? "resourceCreationTime")
            Prelude.<*> (x Prelude..:? "awsRegion")
            Prelude.<*> (x Prelude..:? "resourceName")
      )

instance Prelude.Hashable BaseConfigurationItem

instance Prelude.NFData BaseConfigurationItem
