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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a database snapshot.
--
-- /See:/ 'newRelationalDatabaseSnapshot' smart constructor.
data RelationalDatabaseSnapshot = RelationalDatabaseSnapshot'
  { -- | The name of the source database from which the database snapshot was
    -- created.
    fromRelationalDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the database snapshot was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the database snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code for the database snapshot. Include this code in your
    -- email to support when you have questions about a database snapshot in
    -- Lightsail. This code enables our support team to look up your Lightsail
    -- information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB (for example, @32@) for the database
    -- snapshot.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The state of the database snapshot.
    state :: Prelude.Maybe Prelude.Text,
    -- | The name of the database snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The database engine version for the database snapshot (for example,
    -- @5.7.23@).
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The bundle ID of the database from which the database snapshot was
    -- created.
    fromRelationalDatabaseBundleId :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The software of the database snapshot (for example, @MySQL@)
    engine :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the database from which the database
    -- snapshot was created.
    fromRelationalDatabaseArn :: Prelude.Maybe Prelude.Text,
    -- | The Region name and Availability Zone where the database snapshot is
    -- located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The blueprint ID of the database from which the database snapshot was
    -- created. A blueprint describes the major engine version of a database.
    fromRelationalDatabaseBlueprintId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromRelationalDatabaseName', 'relationalDatabaseSnapshot_fromRelationalDatabaseName' - The name of the source database from which the database snapshot was
-- created.
--
-- 'createdAt', 'relationalDatabaseSnapshot_createdAt' - The timestamp when the database snapshot was created.
--
-- 'arn', 'relationalDatabaseSnapshot_arn' - The Amazon Resource Name (ARN) of the database snapshot.
--
-- 'resourceType', 'relationalDatabaseSnapshot_resourceType' - The Lightsail resource type.
--
-- 'supportCode', 'relationalDatabaseSnapshot_supportCode' - The support code for the database snapshot. Include this code in your
-- email to support when you have questions about a database snapshot in
-- Lightsail. This code enables our support team to look up your Lightsail
-- information more easily.
--
-- 'sizeInGb', 'relationalDatabaseSnapshot_sizeInGb' - The size of the disk in GB (for example, @32@) for the database
-- snapshot.
--
-- 'state', 'relationalDatabaseSnapshot_state' - The state of the database snapshot.
--
-- 'name', 'relationalDatabaseSnapshot_name' - The name of the database snapshot.
--
-- 'engineVersion', 'relationalDatabaseSnapshot_engineVersion' - The database engine version for the database snapshot (for example,
-- @5.7.23@).
--
-- 'fromRelationalDatabaseBundleId', 'relationalDatabaseSnapshot_fromRelationalDatabaseBundleId' - The bundle ID of the database from which the database snapshot was
-- created.
--
-- 'tags', 'relationalDatabaseSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'engine', 'relationalDatabaseSnapshot_engine' - The software of the database snapshot (for example, @MySQL@)
--
-- 'fromRelationalDatabaseArn', 'relationalDatabaseSnapshot_fromRelationalDatabaseArn' - The Amazon Resource Name (ARN) of the database from which the database
-- snapshot was created.
--
-- 'location', 'relationalDatabaseSnapshot_location' - The Region name and Availability Zone where the database snapshot is
-- located.
--
-- 'fromRelationalDatabaseBlueprintId', 'relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId' - The blueprint ID of the database from which the database snapshot was
-- created. A blueprint describes the major engine version of a database.
newRelationalDatabaseSnapshot ::
  RelationalDatabaseSnapshot
newRelationalDatabaseSnapshot =
  RelationalDatabaseSnapshot'
    { fromRelationalDatabaseName =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      fromRelationalDatabaseBundleId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      engine = Prelude.Nothing,
      fromRelationalDatabaseArn = Prelude.Nothing,
      location = Prelude.Nothing,
      fromRelationalDatabaseBlueprintId =
        Prelude.Nothing
    }

-- | The name of the source database from which the database snapshot was
-- created.
relationalDatabaseSnapshot_fromRelationalDatabaseName :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseName = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseName} -> fromRelationalDatabaseName) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseName = a} :: RelationalDatabaseSnapshot)

-- | The timestamp when the database snapshot was created.
relationalDatabaseSnapshot_createdAt :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.UTCTime)
relationalDatabaseSnapshot_createdAt = Lens.lens (\RelationalDatabaseSnapshot' {createdAt} -> createdAt) (\s@RelationalDatabaseSnapshot' {} a -> s {createdAt = a} :: RelationalDatabaseSnapshot) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the database snapshot.
relationalDatabaseSnapshot_arn :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_arn = Lens.lens (\RelationalDatabaseSnapshot' {arn} -> arn) (\s@RelationalDatabaseSnapshot' {} a -> s {arn = a} :: RelationalDatabaseSnapshot)

-- | The Lightsail resource type.
relationalDatabaseSnapshot_resourceType :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe ResourceType)
relationalDatabaseSnapshot_resourceType = Lens.lens (\RelationalDatabaseSnapshot' {resourceType} -> resourceType) (\s@RelationalDatabaseSnapshot' {} a -> s {resourceType = a} :: RelationalDatabaseSnapshot)

-- | The support code for the database snapshot. Include this code in your
-- email to support when you have questions about a database snapshot in
-- Lightsail. This code enables our support team to look up your Lightsail
-- information more easily.
relationalDatabaseSnapshot_supportCode :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_supportCode = Lens.lens (\RelationalDatabaseSnapshot' {supportCode} -> supportCode) (\s@RelationalDatabaseSnapshot' {} a -> s {supportCode = a} :: RelationalDatabaseSnapshot)

-- | The size of the disk in GB (for example, @32@) for the database
-- snapshot.
relationalDatabaseSnapshot_sizeInGb :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Int)
relationalDatabaseSnapshot_sizeInGb = Lens.lens (\RelationalDatabaseSnapshot' {sizeInGb} -> sizeInGb) (\s@RelationalDatabaseSnapshot' {} a -> s {sizeInGb = a} :: RelationalDatabaseSnapshot)

-- | The state of the database snapshot.
relationalDatabaseSnapshot_state :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_state = Lens.lens (\RelationalDatabaseSnapshot' {state} -> state) (\s@RelationalDatabaseSnapshot' {} a -> s {state = a} :: RelationalDatabaseSnapshot)

-- | The name of the database snapshot.
relationalDatabaseSnapshot_name :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_name = Lens.lens (\RelationalDatabaseSnapshot' {name} -> name) (\s@RelationalDatabaseSnapshot' {} a -> s {name = a} :: RelationalDatabaseSnapshot)

-- | The database engine version for the database snapshot (for example,
-- @5.7.23@).
relationalDatabaseSnapshot_engineVersion :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_engineVersion = Lens.lens (\RelationalDatabaseSnapshot' {engineVersion} -> engineVersion) (\s@RelationalDatabaseSnapshot' {} a -> s {engineVersion = a} :: RelationalDatabaseSnapshot)

-- | The bundle ID of the database from which the database snapshot was
-- created.
relationalDatabaseSnapshot_fromRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseBundleId = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseBundleId} -> fromRelationalDatabaseBundleId) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseBundleId = a} :: RelationalDatabaseSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
relationalDatabaseSnapshot_tags :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe [Tag])
relationalDatabaseSnapshot_tags = Lens.lens (\RelationalDatabaseSnapshot' {tags} -> tags) (\s@RelationalDatabaseSnapshot' {} a -> s {tags = a} :: RelationalDatabaseSnapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The software of the database snapshot (for example, @MySQL@)
relationalDatabaseSnapshot_engine :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_engine = Lens.lens (\RelationalDatabaseSnapshot' {engine} -> engine) (\s@RelationalDatabaseSnapshot' {} a -> s {engine = a} :: RelationalDatabaseSnapshot)

-- | The Amazon Resource Name (ARN) of the database from which the database
-- snapshot was created.
relationalDatabaseSnapshot_fromRelationalDatabaseArn :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseArn = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseArn} -> fromRelationalDatabaseArn) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseArn = a} :: RelationalDatabaseSnapshot)

-- | The Region name and Availability Zone where the database snapshot is
-- located.
relationalDatabaseSnapshot_location :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe ResourceLocation)
relationalDatabaseSnapshot_location = Lens.lens (\RelationalDatabaseSnapshot' {location} -> location) (\s@RelationalDatabaseSnapshot' {} a -> s {location = a} :: RelationalDatabaseSnapshot)

-- | The blueprint ID of the database from which the database snapshot was
-- created. A blueprint describes the major engine version of a database.
relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabaseSnapshot (Prelude.Maybe Prelude.Text)
relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId = Lens.lens (\RelationalDatabaseSnapshot' {fromRelationalDatabaseBlueprintId} -> fromRelationalDatabaseBlueprintId) (\s@RelationalDatabaseSnapshot' {} a -> s {fromRelationalDatabaseBlueprintId = a} :: RelationalDatabaseSnapshot)

instance Prelude.FromJSON RelationalDatabaseSnapshot where
  parseJSON =
    Prelude.withObject
      "RelationalDatabaseSnapshot"
      ( \x ->
          RelationalDatabaseSnapshot'
            Prelude.<$> (x Prelude..:? "fromRelationalDatabaseName")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "sizeInGb")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "engineVersion")
            Prelude.<*> (x Prelude..:? "fromRelationalDatabaseBundleId")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "engine")
            Prelude.<*> (x Prelude..:? "fromRelationalDatabaseArn")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "fromRelationalDatabaseBlueprintId")
      )

instance Prelude.Hashable RelationalDatabaseSnapshot

instance Prelude.NFData RelationalDatabaseSnapshot
