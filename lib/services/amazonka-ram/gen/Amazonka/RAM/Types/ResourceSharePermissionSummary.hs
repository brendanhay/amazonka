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
-- Module      : Amazonka.RAM.Types.ResourceSharePermissionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceSharePermissionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an RAM permission that is associated with a resource
-- share and any of its resources of a specified type.
--
-- /See:/ 'newResourceSharePermissionSummary' smart constructor.
data ResourceSharePermissionSummary = ResourceSharePermissionSummary'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the permission you want information about.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the permission was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the version of the permission represented in this
    -- structure is the default version for this permission.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the version of the permission represented in this
    -- structure is the default version for all resources of this resource
    -- type.
    isResourceTypeDefault :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the permission was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of this permission.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of resource to which this permission applies.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The current status of the permission.
    status :: Prelude.Maybe Prelude.Text,
    -- | The version of the permission represented in this structure.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSharePermissionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceSharePermissionSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission you want information about.
--
-- 'creationTime', 'resourceSharePermissionSummary_creationTime' - The date and time when the permission was created.
--
-- 'defaultVersion', 'resourceSharePermissionSummary_defaultVersion' - Specifies whether the version of the permission represented in this
-- structure is the default version for this permission.
--
-- 'isResourceTypeDefault', 'resourceSharePermissionSummary_isResourceTypeDefault' - Specifies whether the version of the permission represented in this
-- structure is the default version for all resources of this resource
-- type.
--
-- 'lastUpdatedTime', 'resourceSharePermissionSummary_lastUpdatedTime' - The date and time when the permission was last updated.
--
-- 'name', 'resourceSharePermissionSummary_name' - The name of this permission.
--
-- 'resourceType', 'resourceSharePermissionSummary_resourceType' - The type of resource to which this permission applies.
--
-- 'status', 'resourceSharePermissionSummary_status' - The current status of the permission.
--
-- 'version', 'resourceSharePermissionSummary_version' - The version of the permission represented in this structure.
newResourceSharePermissionSummary ::
  ResourceSharePermissionSummary
newResourceSharePermissionSummary =
  ResourceSharePermissionSummary'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      isResourceTypeDefault = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission you want information about.
resourceSharePermissionSummary_arn :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_arn = Lens.lens (\ResourceSharePermissionSummary' {arn} -> arn) (\s@ResourceSharePermissionSummary' {} a -> s {arn = a} :: ResourceSharePermissionSummary)

-- | The date and time when the permission was created.
resourceSharePermissionSummary_creationTime :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionSummary_creationTime = Lens.lens (\ResourceSharePermissionSummary' {creationTime} -> creationTime) (\s@ResourceSharePermissionSummary' {} a -> s {creationTime = a} :: ResourceSharePermissionSummary) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the version of the permission represented in this
-- structure is the default version for this permission.
resourceSharePermissionSummary_defaultVersion :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Bool)
resourceSharePermissionSummary_defaultVersion = Lens.lens (\ResourceSharePermissionSummary' {defaultVersion} -> defaultVersion) (\s@ResourceSharePermissionSummary' {} a -> s {defaultVersion = a} :: ResourceSharePermissionSummary)

-- | Specifies whether the version of the permission represented in this
-- structure is the default version for all resources of this resource
-- type.
resourceSharePermissionSummary_isResourceTypeDefault :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Bool)
resourceSharePermissionSummary_isResourceTypeDefault = Lens.lens (\ResourceSharePermissionSummary' {isResourceTypeDefault} -> isResourceTypeDefault) (\s@ResourceSharePermissionSummary' {} a -> s {isResourceTypeDefault = a} :: ResourceSharePermissionSummary)

-- | The date and time when the permission was last updated.
resourceSharePermissionSummary_lastUpdatedTime :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionSummary_lastUpdatedTime = Lens.lens (\ResourceSharePermissionSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceSharePermissionSummary' {} a -> s {lastUpdatedTime = a} :: ResourceSharePermissionSummary) Prelude.. Lens.mapping Data._Time

-- | The name of this permission.
resourceSharePermissionSummary_name :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_name = Lens.lens (\ResourceSharePermissionSummary' {name} -> name) (\s@ResourceSharePermissionSummary' {} a -> s {name = a} :: ResourceSharePermissionSummary)

-- | The type of resource to which this permission applies.
resourceSharePermissionSummary_resourceType :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_resourceType = Lens.lens (\ResourceSharePermissionSummary' {resourceType} -> resourceType) (\s@ResourceSharePermissionSummary' {} a -> s {resourceType = a} :: ResourceSharePermissionSummary)

-- | The current status of the permission.
resourceSharePermissionSummary_status :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_status = Lens.lens (\ResourceSharePermissionSummary' {status} -> status) (\s@ResourceSharePermissionSummary' {} a -> s {status = a} :: ResourceSharePermissionSummary)

-- | The version of the permission represented in this structure.
resourceSharePermissionSummary_version :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_version = Lens.lens (\ResourceSharePermissionSummary' {version} -> version) (\s@ResourceSharePermissionSummary' {} a -> s {version = a} :: ResourceSharePermissionSummary)

instance Data.FromJSON ResourceSharePermissionSummary where
  parseJSON =
    Data.withObject
      "ResourceSharePermissionSummary"
      ( \x ->
          ResourceSharePermissionSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "defaultVersion")
            Prelude.<*> (x Data..:? "isResourceTypeDefault")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
      )

instance
  Prelude.Hashable
    ResourceSharePermissionSummary
  where
  hashWithSalt
    _salt
    ResourceSharePermissionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` defaultVersion
        `Prelude.hashWithSalt` isResourceTypeDefault
        `Prelude.hashWithSalt` lastUpdatedTime
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    ResourceSharePermissionSummary
  where
  rnf ResourceSharePermissionSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf defaultVersion `Prelude.seq`
          Prelude.rnf isResourceTypeDefault `Prelude.seq`
            Prelude.rnf lastUpdatedTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf resourceType `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf version
