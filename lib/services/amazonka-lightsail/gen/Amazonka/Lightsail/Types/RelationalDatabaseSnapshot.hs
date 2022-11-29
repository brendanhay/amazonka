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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a database snapshot.
--
-- /See:/ 'newRelationalDatabaseSnapshot' smart constructor.
data RelationalDatabaseSnapshot = RelationalDatabaseSnapshot'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The Lightsail resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the database snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The bundle ID of the database from which the database snapshot was
    -- created.
    fromRelationalDatabaseBundleId :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB (for example, @32@) for the database
    -- snapshot.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the database snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the database snapshot.
    state :: Prelude.Maybe Prelude.Text,
    -- | The name of the source database from which the database snapshot was
    -- created.
    fromRelationalDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID of the database from which the database snapshot was
    -- created. A blueprint describes the major engine version of a database.
    fromRelationalDatabaseBlueprintId :: Prelude.Maybe Prelude.Text,
    -- | The Region name and Availability Zone where the database snapshot is
    -- located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The Amazon Resource Name (ARN) of the database from which the database
    -- snapshot was created.
    fromRelationalDatabaseArn :: Prelude.Maybe Prelude.Text,
    -- | The software of the database snapshot (for example, @MySQL@)
    engine :: Prelude.Maybe Prelude.Text,
    -- | The support code for the database snapshot. Include this code in your
    -- email to support when you have questions about a database snapshot in
    -- Lightsail. This code enables our support team to look up your Lightsail
    -- information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the database snapshot was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The database engine version for the database snapshot (for example,
    -- @5.7.23@).
    engineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'relationalDatabaseSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'resourceType', 'relationalDatabaseSnapshot_resourceType' - The Lightsail resource type.
--
-- 'name', 'relationalDatabaseSnapshot_name' - The name of the database snapshot.
--
-- 'fromRelationalDatabaseBundleId', 'relationalDatabaseSnapshot_fromRelationalDatabaseBundleId' - The bundle ID of the database from which the database snapshot was
-- created.
--
-- 'sizeInGb', 'relationalDatabaseSnapshot_sizeInGb' - The size of the disk in GB (for example, @32@) for the database
-- snapshot.
--
-- 'arn', 'relationalDatabaseSnapshot_arn' - The Amazon Resource Name (ARN) of the database snapshot.
--
-- 'state', 'relationalDatabaseSnapshot_state' - The state of the database snapshot.
--
-- 'fromRelationalDatabaseName', 'relationalDatabaseSnapshot_fromRelationalDatabaseName' - The name of the source database from which the database snapshot was
-- created.
--
-- 'fromRelationalDatabaseBlueprintId', 'relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId' - The blueprint ID of the database from which the database snapshot was
-- created. A blueprint describes the major engine version of a database.
--
-- 'location', 'relationalDatabaseSnapshot_location' - The Region name and Availability Zone where the database snapshot is
-- located.
--
-- 'fromRelationalDatabaseArn', 'relationalDatabaseSnapshot_fromRelationalDatabaseArn' - The Amazon Resource Name (ARN) of the database from which the database
-- snapshot was created.
--
-- 'engine', 'relationalDatabaseSnapshot_engine' - The software of the database snapshot (for example, @MySQL@)
--
-- 'supportCode', 'relationalDatabaseSnapshot_supportCode' - The support code for the database snapshot. Include this code in your
-- email to support when you have questions about a database snapshot in
-- Lightsail. This code enables our support team to look up your Lightsail
-- information more easily.
--
-- 'createdAt', 'relationalDatabaseSnapshot_createdAt' - The timestamp when the database snapshot was created.
--
-- 'engineVersion', 'relationalDatabaseSnapshot_engineVersion' - The database engine version for the database snapshot (for example,
-- @5.7.23@).
newRelationalDatabaseSnapshot ::
  RelationalDatabaseSnapshot
newRelationalDatabaseSnapshot =
  RelationalDatabaseSnapshot'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      fromRelationalDatabaseBundleId =
        Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      fromRelationalDatabaseName = Prelude.Nothing,
      fromRelationalDatabaseBlueprintId =
        Prelude.Nothing,
      location = Prelude.Nothing,
      fromRelationalDatabaseArn = Prelude.Nothing,
      engine = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
relationalDatabaseSnapshot_tags :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe [Tag])
relationalDatabaseSnapshot_tags = Lens.lens (\RelationalDatabaseSnapshot' {tags} -> tags) (\s@RelationalDatabaseSnapshot' {} a -> s {tags = a} :: RelationalDatabaseSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The Lightsail resource type.
relationalDatabaseSnapshot_resourceType :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe ResourceType)
relationalDatabaseSnapshot_resourceType = Lens.lens (\RelationalDatabaseSnapshot' {resourceType} -> resourceType) (\s@RelationalDatabaseSnapshot' {} a -> s {resourceType = a} :: RelationalDatabaseSnapshot)

-- | The name of the database snapshot.
relationalDatabaseSnapshot_name :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_name = Lens.lens (\RelationalDatabaseSnapshot' {name} -> name) (\s@RelationalDatabaseSnapshot' {} a -> s {name = a} :: RelationalDatabaseSnapshot)

-- | The bundle ID of the database from which the database snapshot was
-- created.
relationalDatabaseSnapshot_fromRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseBundleId = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseBundleId} -> fromRelationalDatabaseBundleId) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseBundleId = a} :: RelationalDatabaseSnapshot)

-- | The size of the disk in GB (for example, @32@) for the database
-- snapshot.
relationalDatabaseSnapshot_sizeInGb :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Int)
relationalDatabaseSnapshot_sizeInGb = Lens.lens (\RelationalDatabaseSnapshot' {sizeInGb} -> sizeInGb) (\s@RelationalDatabaseSnapshot' {} a -> s {sizeInGb = a} :: RelationalDatabaseSnapshot)

-- | The Amazon Resource Name (ARN) of the database snapshot.
relationalDatabaseSnapshot_arn :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_arn = Lens.lens (\RelationalDatabaseSnapshot' {arn} -> arn) (\s@RelationalDatabaseSnapshot' {} a -> s {arn = a} :: RelationalDatabaseSnapshot)

-- | The state of the database snapshot.
relationalDatabaseSnapshot_state :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_state = Lens.lens (\RelationalDatabaseSnapshot' {state} -> state) (\s@RelationalDatabaseSnapshot' {} a -> s {state = a} :: RelationalDatabaseSnapshot)

-- | The name of the source database from which the database snapshot was
-- created.
relationalDatabaseSnapshot_fromRelationalDatabaseName :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseName = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseName} -> fromRelationalDatabaseName) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseName = a} :: RelationalDatabaseSnapshot)

-- | The blueprint ID of the database from which the database snapshot was
-- created. A blueprint describes the major engine version of a database.
relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseBlueprintId} -> fromRelationalDatabaseBlueprintId) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseBlueprintId = a} :: RelationalDatabaseSnapshot)

-- | The Region name and Availability Zone where the database snapshot is
-- located.
relationalDatabaseSnapshot_location :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe ResourceLocation)
relationalDatabaseSnapshot_location = Lens.lens (\RelationalDatabaseSnapshot' {location} -> location) (\s@RelationalDatabaseSnapshot' {} a -> s {location = a} :: RelationalDatabaseSnapshot)

-- | The Amazon Resource Name (ARN) of the database from which the database
-- snapshot was created.
relationalDatabaseSnapshot_fromRelationalDatabaseArn :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseArn = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseArn} -> fromRelationalDatabaseArn) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseArn = a} :: RelationalDatabaseSnapshot)

-- | The software of the database snapshot (for example, @MySQL@)
relationalDatabaseSnapshot_engine :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_engine = Lens.lens (\RelationalDatabaseSnapshot' {engine} -> engine) (\s@RelationalDatabaseSnapshot' {} a -> s {engine = a} :: RelationalDatabaseSnapshot)

-- | The support code for the database snapshot. Include this code in your
-- email to support when you have questions about a database snapshot in
-- Lightsail. This code enables our support team to look up your Lightsail
-- information more easily.
relationalDatabaseSnapshot_supportCode :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_supportCode = Lens.lens (\RelationalDatabaseSnapshot' {supportCode} -> supportCode) (\s@RelationalDatabaseSnapshot' {} a -> s {supportCode = a} :: RelationalDatabaseSnapshot)

-- | The timestamp when the database snapshot was created.
relationalDatabaseSnapshot_createdAt :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.UTCTime)
relationalDatabaseSnapshot_createdAt = Lens.lens (\RelationalDatabaseSnapshot' {createdAt} -> createdAt) (\s@RelationalDatabaseSnapshot' {} a -> s {createdAt = a} :: RelationalDatabaseSnapshot) Prelude.. Lens.mapping Core._Time

-- | The database engine version for the database snapshot (for example,
-- @5.7.23@).
relationalDatabaseSnapshot_engineVersion :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_engineVersion = Lens.lens (\RelationalDatabaseSnapshot' {engineVersion} -> engineVersion) (\s@RelationalDatabaseSnapshot' {} a -> s {engineVersion = a} :: RelationalDatabaseSnapshot)

instance Core.FromJSON RelationalDatabaseSnapshot where
  parseJSON =
    Core.withObject
      "RelationalDatabaseSnapshot"
      ( \x ->
          RelationalDatabaseSnapshot'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "fromRelationalDatabaseBundleId")
            Prelude.<*> (x Core..:? "sizeInGb")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "fromRelationalDatabaseName")
            Prelude.<*> (x Core..:? "fromRelationalDatabaseBlueprintId")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "fromRelationalDatabaseArn")
            Prelude.<*> (x Core..:? "engine")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "engineVersion")
      )

instance Prelude.Hashable RelationalDatabaseSnapshot where
  hashWithSalt _salt RelationalDatabaseSnapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fromRelationalDatabaseBundleId
      `Prelude.hashWithSalt` sizeInGb
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` fromRelationalDatabaseName
      `Prelude.hashWithSalt` fromRelationalDatabaseBlueprintId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` fromRelationalDatabaseArn
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData RelationalDatabaseSnapshot where
  rnf RelationalDatabaseSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf fromRelationalDatabaseBundleId
      `Prelude.seq` Prelude.rnf sizeInGb
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf fromRelationalDatabaseName
      `Prelude.seq` Prelude.rnf fromRelationalDatabaseBlueprintId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf fromRelationalDatabaseArn
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf engineVersion
