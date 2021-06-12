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
-- Module      : Network.AWS.CloudFormation.Types.TypeVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeVersionSummary where

import Network.AWS.CloudFormation.Types.RegistryType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a specific version of a
-- CloudFormation type.
--
-- /See:/ 'newTypeVersionSummary' smart constructor.
data TypeVersionSummary = TypeVersionSummary'
  { -- | The name of the type.
    typeName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the type version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of a specific version of the type. The version ID is the value at
    -- the end of the Amazon Resource Name (ARN) assigned to the type version
    -- when it is registered.
    versionId :: Core.Maybe Core.Text,
    -- | The description of the type version.
    description :: Core.Maybe Core.Text,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The kind of type.
    type' :: Core.Maybe RegistryType,
    -- | When the version was registered.
    timeCreated :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TypeVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'typeVersionSummary_typeName' - The name of the type.
--
-- 'arn', 'typeVersionSummary_arn' - The Amazon Resource Name (ARN) of the type version.
--
-- 'versionId', 'typeVersionSummary_versionId' - The ID of a specific version of the type. The version ID is the value at
-- the end of the Amazon Resource Name (ARN) assigned to the type version
-- when it is registered.
--
-- 'description', 'typeVersionSummary_description' - The description of the type version.
--
-- 'isDefaultVersion', 'typeVersionSummary_isDefaultVersion' - Whether the specified type version is set as the default version.
--
-- 'type'', 'typeVersionSummary_type' - The kind of type.
--
-- 'timeCreated', 'typeVersionSummary_timeCreated' - When the version was registered.
newTypeVersionSummary ::
  TypeVersionSummary
newTypeVersionSummary =
  TypeVersionSummary'
    { typeName = Core.Nothing,
      arn = Core.Nothing,
      versionId = Core.Nothing,
      description = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      type' = Core.Nothing,
      timeCreated = Core.Nothing
    }

-- | The name of the type.
typeVersionSummary_typeName :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Text)
typeVersionSummary_typeName = Lens.lens (\TypeVersionSummary' {typeName} -> typeName) (\s@TypeVersionSummary' {} a -> s {typeName = a} :: TypeVersionSummary)

-- | The Amazon Resource Name (ARN) of the type version.
typeVersionSummary_arn :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Text)
typeVersionSummary_arn = Lens.lens (\TypeVersionSummary' {arn} -> arn) (\s@TypeVersionSummary' {} a -> s {arn = a} :: TypeVersionSummary)

-- | The ID of a specific version of the type. The version ID is the value at
-- the end of the Amazon Resource Name (ARN) assigned to the type version
-- when it is registered.
typeVersionSummary_versionId :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Text)
typeVersionSummary_versionId = Lens.lens (\TypeVersionSummary' {versionId} -> versionId) (\s@TypeVersionSummary' {} a -> s {versionId = a} :: TypeVersionSummary)

-- | The description of the type version.
typeVersionSummary_description :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Text)
typeVersionSummary_description = Lens.lens (\TypeVersionSummary' {description} -> description) (\s@TypeVersionSummary' {} a -> s {description = a} :: TypeVersionSummary)

-- | Whether the specified type version is set as the default version.
typeVersionSummary_isDefaultVersion :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Bool)
typeVersionSummary_isDefaultVersion = Lens.lens (\TypeVersionSummary' {isDefaultVersion} -> isDefaultVersion) (\s@TypeVersionSummary' {} a -> s {isDefaultVersion = a} :: TypeVersionSummary)

-- | The kind of type.
typeVersionSummary_type :: Lens.Lens' TypeVersionSummary (Core.Maybe RegistryType)
typeVersionSummary_type = Lens.lens (\TypeVersionSummary' {type'} -> type') (\s@TypeVersionSummary' {} a -> s {type' = a} :: TypeVersionSummary)

-- | When the version was registered.
typeVersionSummary_timeCreated :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.UTCTime)
typeVersionSummary_timeCreated = Lens.lens (\TypeVersionSummary' {timeCreated} -> timeCreated) (\s@TypeVersionSummary' {} a -> s {timeCreated = a} :: TypeVersionSummary) Core.. Lens.mapping Core._Time

instance Core.FromXML TypeVersionSummary where
  parseXML x =
    TypeVersionSummary'
      Core.<$> (x Core..@? "TypeName")
      Core.<*> (x Core..@? "Arn")
      Core.<*> (x Core..@? "VersionId")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsDefaultVersion")
      Core.<*> (x Core..@? "Type")
      Core.<*> (x Core..@? "TimeCreated")

instance Core.Hashable TypeVersionSummary

instance Core.NFData TypeVersionSummary
