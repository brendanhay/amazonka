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
-- Module      : Amazonka.Config.Types.BaseConfigurationItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.BaseConfigurationItem where

import Amazonka.Config.Types.ConfigurationItemStatus
import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detailed configuration of a specified resource.
--
-- /See:/ 'newBaseConfigurationItem' smart constructor.
data BaseConfigurationItem = BaseConfigurationItem'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The custom name of the resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the resource was created.
    resourceCreationTime :: Prelude.Maybe Data.POSIX,
    -- | Configuration attributes that Config returns for certain resource types
    -- to supplement the information returned for the configuration parameter.
    supplementaryConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An identifier that indicates the ordering of the configuration items of
    -- a resource.
    configurationStateId :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource configuration.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
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
    -- | The Availability Zone associated with the resource.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit Amazon Web Services account ID associated with the
    -- resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The region where the resource resides.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The version number of the resource configuration.
    version :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration recording was initiated.
    configurationItemCaptureTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceType', 'baseConfigurationItem_resourceType' - The type of Amazon Web Services resource.
--
-- 'resourceName', 'baseConfigurationItem_resourceName' - The custom name of the resource, if available.
--
-- 'resourceCreationTime', 'baseConfigurationItem_resourceCreationTime' - The time stamp when the resource was created.
--
-- 'supplementaryConfiguration', 'baseConfigurationItem_supplementaryConfiguration' - Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the configuration parameter.
--
-- 'configurationStateId', 'baseConfigurationItem_configurationStateId' - An identifier that indicates the ordering of the configuration items of
-- a resource.
--
-- 'configuration', 'baseConfigurationItem_configuration' - The description of the resource configuration.
--
-- 'arn', 'baseConfigurationItem_arn' - The Amazon Resource Name (ARN) of the resource.
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
-- 'availabilityZone', 'baseConfigurationItem_availabilityZone' - The Availability Zone associated with the resource.
--
-- 'accountId', 'baseConfigurationItem_accountId' - The 12-digit Amazon Web Services account ID associated with the
-- resource.
--
-- 'awsRegion', 'baseConfigurationItem_awsRegion' - The region where the resource resides.
--
-- 'version', 'baseConfigurationItem_version' - The version number of the resource configuration.
--
-- 'configurationItemCaptureTime', 'baseConfigurationItem_configurationItemCaptureTime' - The time when the configuration recording was initiated.
newBaseConfigurationItem ::
  BaseConfigurationItem
newBaseConfigurationItem =
  BaseConfigurationItem'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceCreationTime = Prelude.Nothing,
      supplementaryConfiguration = Prelude.Nothing,
      configurationStateId = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      configurationItemStatus = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      accountId = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      version = Prelude.Nothing,
      configurationItemCaptureTime = Prelude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
baseConfigurationItem_resourceId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_resourceId = Lens.lens (\BaseConfigurationItem' {resourceId} -> resourceId) (\s@BaseConfigurationItem' {} a -> s {resourceId = a} :: BaseConfigurationItem)

-- | The type of Amazon Web Services resource.
baseConfigurationItem_resourceType :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe ResourceType)
baseConfigurationItem_resourceType = Lens.lens (\BaseConfigurationItem' {resourceType} -> resourceType) (\s@BaseConfigurationItem' {} a -> s {resourceType = a} :: BaseConfigurationItem)

-- | The custom name of the resource, if available.
baseConfigurationItem_resourceName :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_resourceName = Lens.lens (\BaseConfigurationItem' {resourceName} -> resourceName) (\s@BaseConfigurationItem' {} a -> s {resourceName = a} :: BaseConfigurationItem)

-- | The time stamp when the resource was created.
baseConfigurationItem_resourceCreationTime :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.UTCTime)
baseConfigurationItem_resourceCreationTime = Lens.lens (\BaseConfigurationItem' {resourceCreationTime} -> resourceCreationTime) (\s@BaseConfigurationItem' {} a -> s {resourceCreationTime = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Data._Time

-- | Configuration attributes that Config returns for certain resource types
-- to supplement the information returned for the configuration parameter.
baseConfigurationItem_supplementaryConfiguration :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
baseConfigurationItem_supplementaryConfiguration = Lens.lens (\BaseConfigurationItem' {supplementaryConfiguration} -> supplementaryConfiguration) (\s@BaseConfigurationItem' {} a -> s {supplementaryConfiguration = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
baseConfigurationItem_configurationStateId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_configurationStateId = Lens.lens (\BaseConfigurationItem' {configurationStateId} -> configurationStateId) (\s@BaseConfigurationItem' {} a -> s {configurationStateId = a} :: BaseConfigurationItem)

-- | The description of the resource configuration.
baseConfigurationItem_configuration :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_configuration = Lens.lens (\BaseConfigurationItem' {configuration} -> configuration) (\s@BaseConfigurationItem' {} a -> s {configuration = a} :: BaseConfigurationItem)

-- | The Amazon Resource Name (ARN) of the resource.
baseConfigurationItem_arn :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_arn = Lens.lens (\BaseConfigurationItem' {arn} -> arn) (\s@BaseConfigurationItem' {} a -> s {arn = a} :: BaseConfigurationItem)

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

-- | The Availability Zone associated with the resource.
baseConfigurationItem_availabilityZone :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_availabilityZone = Lens.lens (\BaseConfigurationItem' {availabilityZone} -> availabilityZone) (\s@BaseConfigurationItem' {} a -> s {availabilityZone = a} :: BaseConfigurationItem)

-- | The 12-digit Amazon Web Services account ID associated with the
-- resource.
baseConfigurationItem_accountId :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_accountId = Lens.lens (\BaseConfigurationItem' {accountId} -> accountId) (\s@BaseConfigurationItem' {} a -> s {accountId = a} :: BaseConfigurationItem)

-- | The region where the resource resides.
baseConfigurationItem_awsRegion :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_awsRegion = Lens.lens (\BaseConfigurationItem' {awsRegion} -> awsRegion) (\s@BaseConfigurationItem' {} a -> s {awsRegion = a} :: BaseConfigurationItem)

-- | The version number of the resource configuration.
baseConfigurationItem_version :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.Text)
baseConfigurationItem_version = Lens.lens (\BaseConfigurationItem' {version} -> version) (\s@BaseConfigurationItem' {} a -> s {version = a} :: BaseConfigurationItem)

-- | The time when the configuration recording was initiated.
baseConfigurationItem_configurationItemCaptureTime :: Lens.Lens' BaseConfigurationItem (Prelude.Maybe Prelude.UTCTime)
baseConfigurationItem_configurationItemCaptureTime = Lens.lens (\BaseConfigurationItem' {configurationItemCaptureTime} -> configurationItemCaptureTime) (\s@BaseConfigurationItem' {} a -> s {configurationItemCaptureTime = a} :: BaseConfigurationItem) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON BaseConfigurationItem where
  parseJSON =
    Data.withObject
      "BaseConfigurationItem"
      ( \x ->
          BaseConfigurationItem'
            Prelude.<$> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceCreationTime")
            Prelude.<*> ( x Data..:? "supplementaryConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "configurationStateId")
            Prelude.<*> (x Data..:? "configuration")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "configurationItemStatus")
            Prelude.<*> (x Data..:? "availabilityZone")
            Prelude.<*> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "awsRegion")
            Prelude.<*> (x Data..:? "version")
            Prelude.<*> (x Data..:? "configurationItemCaptureTime")
      )

instance Prelude.Hashable BaseConfigurationItem where
  hashWithSalt _salt BaseConfigurationItem' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceCreationTime
      `Prelude.hashWithSalt` supplementaryConfiguration
      `Prelude.hashWithSalt` configurationStateId
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` configurationItemStatus
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` configurationItemCaptureTime

instance Prelude.NFData BaseConfigurationItem where
  rnf BaseConfigurationItem' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceCreationTime
      `Prelude.seq` Prelude.rnf supplementaryConfiguration
      `Prelude.seq` Prelude.rnf configurationStateId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf configurationItemStatus
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf configurationItemCaptureTime
