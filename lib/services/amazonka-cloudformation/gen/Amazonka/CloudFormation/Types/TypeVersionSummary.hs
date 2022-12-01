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
-- Module      : Amazonka.CloudFormation.Types.TypeVersionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TypeVersionSummary where

import Amazonka.CloudFormation.Types.RegistryType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a specific version of a
-- CloudFormation extension.
--
-- /See:/ 'newTypeVersionSummary' smart constructor.
data TypeVersionSummary = TypeVersionSummary'
  { -- | Whether the specified extension version is set as the default version.
    --
    -- This applies only to private extensions you have registered in your
    -- account, and extensions published by Amazon. For public third-party
    -- extensions, CloudFormation returns @null@.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The kind of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | The Amazon Resource Name (ARN) of the extension version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the version was registered.
    timeCreated :: Prelude.Maybe Core.ISO8601,
    -- | For public extensions that have been activated for this account and
    -- region, the version of the public extension to be used for
    -- CloudFormation operations in this account and region. For any extensions
    -- other than activated third-arty extensions, CloudFormation returns
    -- @null@.
    --
    -- How you specified @AutoUpdate@ when enabling the extension affects
    -- whether CloudFormation automatically updates the extension in this
    -- account and region when a new version is released. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
    -- in the /CloudFormation User Guide/.
    publicVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the extension version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of a specific version of the extension. The version ID is the
    -- value at the end of the Amazon Resource Name (ARN) assigned to the
    -- extension version when it\'s registered.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypeVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'typeVersionSummary_isDefaultVersion' - Whether the specified extension version is set as the default version.
--
-- This applies only to private extensions you have registered in your
-- account, and extensions published by Amazon. For public third-party
-- extensions, CloudFormation returns @null@.
--
-- 'type'', 'typeVersionSummary_type' - The kind of extension.
--
-- 'arn', 'typeVersionSummary_arn' - The Amazon Resource Name (ARN) of the extension version.
--
-- 'timeCreated', 'typeVersionSummary_timeCreated' - When the version was registered.
--
-- 'publicVersionNumber', 'typeVersionSummary_publicVersionNumber' - For public extensions that have been activated for this account and
-- region, the version of the public extension to be used for
-- CloudFormation operations in this account and region. For any extensions
-- other than activated third-arty extensions, CloudFormation returns
-- @null@.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
--
-- 'typeName', 'typeVersionSummary_typeName' - The name of the extension.
--
-- 'description', 'typeVersionSummary_description' - The description of the extension version.
--
-- 'versionId', 'typeVersionSummary_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it\'s registered.
newTypeVersionSummary ::
  TypeVersionSummary
newTypeVersionSummary =
  TypeVersionSummary'
    { isDefaultVersion =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = Prelude.Nothing,
      timeCreated = Prelude.Nothing,
      publicVersionNumber = Prelude.Nothing,
      typeName = Prelude.Nothing,
      description = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Whether the specified extension version is set as the default version.
--
-- This applies only to private extensions you have registered in your
-- account, and extensions published by Amazon. For public third-party
-- extensions, CloudFormation returns @null@.
typeVersionSummary_isDefaultVersion :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Bool)
typeVersionSummary_isDefaultVersion = Lens.lens (\TypeVersionSummary' {isDefaultVersion} -> isDefaultVersion) (\s@TypeVersionSummary' {} a -> s {isDefaultVersion = a} :: TypeVersionSummary)

-- | The kind of extension.
typeVersionSummary_type :: Lens.Lens' TypeVersionSummary (Prelude.Maybe RegistryType)
typeVersionSummary_type = Lens.lens (\TypeVersionSummary' {type'} -> type') (\s@TypeVersionSummary' {} a -> s {type' = a} :: TypeVersionSummary)

-- | The Amazon Resource Name (ARN) of the extension version.
typeVersionSummary_arn :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_arn = Lens.lens (\TypeVersionSummary' {arn} -> arn) (\s@TypeVersionSummary' {} a -> s {arn = a} :: TypeVersionSummary)

-- | When the version was registered.
typeVersionSummary_timeCreated :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.UTCTime)
typeVersionSummary_timeCreated = Lens.lens (\TypeVersionSummary' {timeCreated} -> timeCreated) (\s@TypeVersionSummary' {} a -> s {timeCreated = a} :: TypeVersionSummary) Prelude.. Lens.mapping Core._Time

-- | For public extensions that have been activated for this account and
-- region, the version of the public extension to be used for
-- CloudFormation operations in this account and region. For any extensions
-- other than activated third-arty extensions, CloudFormation returns
-- @null@.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
typeVersionSummary_publicVersionNumber :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_publicVersionNumber = Lens.lens (\TypeVersionSummary' {publicVersionNumber} -> publicVersionNumber) (\s@TypeVersionSummary' {} a -> s {publicVersionNumber = a} :: TypeVersionSummary)

-- | The name of the extension.
typeVersionSummary_typeName :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_typeName = Lens.lens (\TypeVersionSummary' {typeName} -> typeName) (\s@TypeVersionSummary' {} a -> s {typeName = a} :: TypeVersionSummary)

-- | The description of the extension version.
typeVersionSummary_description :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_description = Lens.lens (\TypeVersionSummary' {description} -> description) (\s@TypeVersionSummary' {} a -> s {description = a} :: TypeVersionSummary)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it\'s registered.
typeVersionSummary_versionId :: Lens.Lens' TypeVersionSummary (Prelude.Maybe Prelude.Text)
typeVersionSummary_versionId = Lens.lens (\TypeVersionSummary' {versionId} -> versionId) (\s@TypeVersionSummary' {} a -> s {versionId = a} :: TypeVersionSummary)

instance Core.FromXML TypeVersionSummary where
  parseXML x =
    TypeVersionSummary'
      Prelude.<$> (x Core..@? "IsDefaultVersion")
      Prelude.<*> (x Core..@? "Type")
      Prelude.<*> (x Core..@? "Arn")
      Prelude.<*> (x Core..@? "TimeCreated")
      Prelude.<*> (x Core..@? "PublicVersionNumber")
      Prelude.<*> (x Core..@? "TypeName")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "VersionId")

instance Prelude.Hashable TypeVersionSummary where
  hashWithSalt _salt TypeVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` isDefaultVersion
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` timeCreated
      `Prelude.hashWithSalt` publicVersionNumber
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData TypeVersionSummary where
  rnf TypeVersionSummary' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf timeCreated
      `Prelude.seq` Prelude.rnf publicVersionNumber
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionId
