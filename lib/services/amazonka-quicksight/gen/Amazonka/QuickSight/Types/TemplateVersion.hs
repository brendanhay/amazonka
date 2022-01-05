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
-- Module      : Amazonka.QuickSight.Types.TemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSetConfiguration
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.Sheet
import Amazonka.QuickSight.Types.TemplateError

-- | A version of a template.
--
-- /See:/ 'newTemplateVersion' smart constructor.
data TemplateVersion = TemplateVersion'
  { -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The ARN of the theme associated with this version of the template.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | A list of the associated sheets with the unique identifier and name of
    -- each sheet.
    sheets :: Prelude.Maybe [Sheet],
    -- | The time that this template version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an analysis or template that was used
    -- to create this template.
    sourceEntityArn :: Prelude.Maybe Prelude.Text,
    -- | Schema of the dataset identified by the placeholder. Any dashboard
    -- created from this template should be bound to new datasets matching the
    -- same schema described through this API operation.
    dataSetConfigurations :: Prelude.Maybe [DataSetConfiguration],
    -- | The version number of the template version.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | Errors associated with this template version.
    errors :: Prelude.Maybe (Prelude.NonEmpty TemplateError),
    -- | The description of the template.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'templateVersion_status' - The HTTP status of the request.
--
-- 'themeArn', 'templateVersion_themeArn' - The ARN of the theme associated with this version of the template.
--
-- 'sheets', 'templateVersion_sheets' - A list of the associated sheets with the unique identifier and name of
-- each sheet.
--
-- 'createdTime', 'templateVersion_createdTime' - The time that this template version was created.
--
-- 'sourceEntityArn', 'templateVersion_sourceEntityArn' - The Amazon Resource Name (ARN) of an analysis or template that was used
-- to create this template.
--
-- 'dataSetConfigurations', 'templateVersion_dataSetConfigurations' - Schema of the dataset identified by the placeholder. Any dashboard
-- created from this template should be bound to new datasets matching the
-- same schema described through this API operation.
--
-- 'versionNumber', 'templateVersion_versionNumber' - The version number of the template version.
--
-- 'errors', 'templateVersion_errors' - Errors associated with this template version.
--
-- 'description', 'templateVersion_description' - The description of the template.
newTemplateVersion ::
  TemplateVersion
newTemplateVersion =
  TemplateVersion'
    { status = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      sheets = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      sourceEntityArn = Prelude.Nothing,
      dataSetConfigurations = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      errors = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The HTTP status of the request.
templateVersion_status :: Lens.Lens' TemplateVersion (Prelude.Maybe ResourceStatus)
templateVersion_status = Lens.lens (\TemplateVersion' {status} -> status) (\s@TemplateVersion' {} a -> s {status = a} :: TemplateVersion)

-- | The ARN of the theme associated with this version of the template.
templateVersion_themeArn :: Lens.Lens' TemplateVersion (Prelude.Maybe Prelude.Text)
templateVersion_themeArn = Lens.lens (\TemplateVersion' {themeArn} -> themeArn) (\s@TemplateVersion' {} a -> s {themeArn = a} :: TemplateVersion)

-- | A list of the associated sheets with the unique identifier and name of
-- each sheet.
templateVersion_sheets :: Lens.Lens' TemplateVersion (Prelude.Maybe [Sheet])
templateVersion_sheets = Lens.lens (\TemplateVersion' {sheets} -> sheets) (\s@TemplateVersion' {} a -> s {sheets = a} :: TemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | The time that this template version was created.
templateVersion_createdTime :: Lens.Lens' TemplateVersion (Prelude.Maybe Prelude.UTCTime)
templateVersion_createdTime = Lens.lens (\TemplateVersion' {createdTime} -> createdTime) (\s@TemplateVersion' {} a -> s {createdTime = a} :: TemplateVersion) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an analysis or template that was used
-- to create this template.
templateVersion_sourceEntityArn :: Lens.Lens' TemplateVersion (Prelude.Maybe Prelude.Text)
templateVersion_sourceEntityArn = Lens.lens (\TemplateVersion' {sourceEntityArn} -> sourceEntityArn) (\s@TemplateVersion' {} a -> s {sourceEntityArn = a} :: TemplateVersion)

-- | Schema of the dataset identified by the placeholder. Any dashboard
-- created from this template should be bound to new datasets matching the
-- same schema described through this API operation.
templateVersion_dataSetConfigurations :: Lens.Lens' TemplateVersion (Prelude.Maybe [DataSetConfiguration])
templateVersion_dataSetConfigurations = Lens.lens (\TemplateVersion' {dataSetConfigurations} -> dataSetConfigurations) (\s@TemplateVersion' {} a -> s {dataSetConfigurations = a} :: TemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the template version.
templateVersion_versionNumber :: Lens.Lens' TemplateVersion (Prelude.Maybe Prelude.Natural)
templateVersion_versionNumber = Lens.lens (\TemplateVersion' {versionNumber} -> versionNumber) (\s@TemplateVersion' {} a -> s {versionNumber = a} :: TemplateVersion)

-- | Errors associated with this template version.
templateVersion_errors :: Lens.Lens' TemplateVersion (Prelude.Maybe (Prelude.NonEmpty TemplateError))
templateVersion_errors = Lens.lens (\TemplateVersion' {errors} -> errors) (\s@TemplateVersion' {} a -> s {errors = a} :: TemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | The description of the template.
templateVersion_description :: Lens.Lens' TemplateVersion (Prelude.Maybe Prelude.Text)
templateVersion_description = Lens.lens (\TemplateVersion' {description} -> description) (\s@TemplateVersion' {} a -> s {description = a} :: TemplateVersion)

instance Core.FromJSON TemplateVersion where
  parseJSON =
    Core.withObject
      "TemplateVersion"
      ( \x ->
          TemplateVersion'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ThemeArn")
            Prelude.<*> (x Core..:? "Sheets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "SourceEntityArn")
            Prelude.<*> ( x Core..:? "DataSetConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Errors")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable TemplateVersion where
  hashWithSalt _salt TemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` sourceEntityArn
      `Prelude.hashWithSalt` dataSetConfigurations
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` description

instance Prelude.NFData TemplateVersion where
  rnf TemplateVersion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf sheets
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf sourceEntityArn
      `Prelude.seq` Prelude.rnf dataSetConfigurations
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf description
