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
-- Module      : Network.AWS.CloudFormation.Types.TypeVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeVersionSummary where

import Network.AWS.CloudFormation.Types.RegistryType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary information about a specific version of a
-- CloudFormation type.
--
-- /See:/ 'newTypeVersionSummary' smart constructor.
data TypeVersionSummary = TypeVersionSummary'
  { -- | The name of the type.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the type version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a specific version of the type. The version ID is the value at
    -- the end of the Amazon Resource Name (ARN) assigned to the type version
    -- when it is registered.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The description of the type version.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The kind of type.
    type' :: Prelude.Maybe RegistryType,
    -- | When the version was registered.
    timeCreated :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { typeName = Prelude.Nothing,
      arn = Prelude.Nothing,
      versionId = Prelude.Nothing,
      description = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      type' = Prelude.Nothing,
      timeCreated = Prelude.Nothing
    }

-- | The name of the type.
typeVersionSummary_typeName :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_typeName = Lens.lens (\TypeVersionSummary' {typeName} -> typeName) (\s@TypeVersionSummary' {} a -> s {typeName = a} :: TypeVersionSummary)

-- | The Amazon Resource Name (ARN) of the type version.
typeVersionSummary_arn :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_arn = Lens.lens (\TypeVersionSummary' {arn} -> arn) (\s@TypeVersionSummary' {} a -> s {arn = a} :: TypeVersionSummary)

-- | The ID of a specific version of the type. The version ID is the value at
-- the end of the Amazon Resource Name (ARN) assigned to the type version
-- when it is registered.
typeVersionSummary_versionId :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_versionId = Lens.lens (\TypeVersionSummary' {versionId} -> versionId) (\s@TypeVersionSummary' {} a -> s {versionId = a} :: TypeVersionSummary)

-- | The description of the type version.
typeVersionSummary_description :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_description = Lens.lens (\TypeVersionSummary' {description} -> description) (\s@TypeVersionSummary' {} a -> s {description = a} :: TypeVersionSummary)

-- | Whether the specified type version is set as the default version.
typeVersionSummary_isDefaultVersion :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Bool)
typeVersionSummary_isDefaultVersion = Lens.lens (\TypeVersionSummary' {isDefaultVersion} -> isDefaultVersion) (\s@TypeVersionSummary' {} a -> s {isDefaultVersion = a} :: TypeVersionSummary)

-- | The kind of type.
typeVersionSummary_type :: Lens.Lens' TypeVersionSummary (Prelude.Maybe RegistryType)
typeVersionSummary_type = Lens.lens (\TypeVersionSummary' {type'} -> type') (\s@TypeVersionSummary' {} a -> s {type' = a} :: TypeVersionSummary)

-- | When the version was registered.
typeVersionSummary_timeCreated :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.UTCTime)
typeVersionSummary_timeCreated = Lens.lens (\TypeVersionSummary' {timeCreated} -> timeCreated) (\s@TypeVersionSummary' {} a -> s {timeCreated = a} :: TypeVersionSummary) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML TypeVersionSummary where
  parseXML x =
    TypeVersionSummary'
      Prelude.<$> (x Prelude..@? "TypeName")
      Prelude.<*> (x Prelude..@? "Arn")
      Prelude.<*> (x Prelude..@? "VersionId")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "IsDefaultVersion")
      Prelude.<*> (x Prelude..@? "Type")
      Prelude.<*> (x Prelude..@? "TimeCreated")

instance Prelude.Hashable TypeVersionSummary

instance Prelude.NFData TypeVersionSummary
