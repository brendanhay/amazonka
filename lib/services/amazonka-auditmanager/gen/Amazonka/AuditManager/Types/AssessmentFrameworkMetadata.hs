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
-- Module      : Amazonka.AuditManager.Types.AssessmentFrameworkMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentFrameworkMetadata where

import Amazonka.AuditManager.Types.FrameworkType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata that\'s associated with a standard framework or a custom
-- framework.
--
-- /See:/ 'newAssessmentFrameworkMetadata' smart constructor.
data AssessmentFrameworkMetadata = AssessmentFrameworkMetadata'
  { -- | The number of control sets that are associated with the framework.
    controlSetsCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the framework.
    name :: Prelude.Maybe Prelude.Text,
    -- | The framework type, such as a standard framework or a custom framework.
    type' :: Prelude.Maybe FrameworkType,
    -- | Specifies when the framework was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the framework.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the framework.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the framework.
    id :: Prelude.Maybe Prelude.Text,
    -- | The logo that\'s associated with the framework.
    logo :: Prelude.Maybe Prelude.Text,
    -- | The number of controls that are associated with the framework.
    controlsCount :: Prelude.Maybe Prelude.Int,
    -- | The compliance type that the new custom framework supports, such as CIS
    -- or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the framework was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentFrameworkMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlSetsCount', 'assessmentFrameworkMetadata_controlSetsCount' - The number of control sets that are associated with the framework.
--
-- 'name', 'assessmentFrameworkMetadata_name' - The name of the framework.
--
-- 'type'', 'assessmentFrameworkMetadata_type' - The framework type, such as a standard framework or a custom framework.
--
-- 'lastUpdatedAt', 'assessmentFrameworkMetadata_lastUpdatedAt' - Specifies when the framework was most recently updated.
--
-- 'arn', 'assessmentFrameworkMetadata_arn' - The Amazon Resource Name (ARN) of the framework.
--
-- 'description', 'assessmentFrameworkMetadata_description' - The description of the framework.
--
-- 'id', 'assessmentFrameworkMetadata_id' - The unique identifier for the framework.
--
-- 'logo', 'assessmentFrameworkMetadata_logo' - The logo that\'s associated with the framework.
--
-- 'controlsCount', 'assessmentFrameworkMetadata_controlsCount' - The number of controls that are associated with the framework.
--
-- 'complianceType', 'assessmentFrameworkMetadata_complianceType' - The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
--
-- 'createdAt', 'assessmentFrameworkMetadata_createdAt' - Specifies when the framework was created.
newAssessmentFrameworkMetadata ::
  AssessmentFrameworkMetadata
newAssessmentFrameworkMetadata =
  AssessmentFrameworkMetadata'
    { controlSetsCount =
        Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      logo = Prelude.Nothing,
      controlsCount = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The number of control sets that are associated with the framework.
assessmentFrameworkMetadata_controlSetsCount :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Int)
assessmentFrameworkMetadata_controlSetsCount = Lens.lens (\AssessmentFrameworkMetadata' {controlSetsCount} -> controlSetsCount) (\s@AssessmentFrameworkMetadata' {} a -> s {controlSetsCount = a} :: AssessmentFrameworkMetadata)

-- | The name of the framework.
assessmentFrameworkMetadata_name :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_name = Lens.lens (\AssessmentFrameworkMetadata' {name} -> name) (\s@AssessmentFrameworkMetadata' {} a -> s {name = a} :: AssessmentFrameworkMetadata)

-- | The framework type, such as a standard framework or a custom framework.
assessmentFrameworkMetadata_type :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe FrameworkType)
assessmentFrameworkMetadata_type = Lens.lens (\AssessmentFrameworkMetadata' {type'} -> type') (\s@AssessmentFrameworkMetadata' {} a -> s {type' = a} :: AssessmentFrameworkMetadata)

-- | Specifies when the framework was most recently updated.
assessmentFrameworkMetadata_lastUpdatedAt :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkMetadata_lastUpdatedAt = Lens.lens (\AssessmentFrameworkMetadata' {lastUpdatedAt} -> lastUpdatedAt) (\s@AssessmentFrameworkMetadata' {} a -> s {lastUpdatedAt = a} :: AssessmentFrameworkMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the framework.
assessmentFrameworkMetadata_arn :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_arn = Lens.lens (\AssessmentFrameworkMetadata' {arn} -> arn) (\s@AssessmentFrameworkMetadata' {} a -> s {arn = a} :: AssessmentFrameworkMetadata)

-- | The description of the framework.
assessmentFrameworkMetadata_description :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_description = Lens.lens (\AssessmentFrameworkMetadata' {description} -> description) (\s@AssessmentFrameworkMetadata' {} a -> s {description = a} :: AssessmentFrameworkMetadata)

-- | The unique identifier for the framework.
assessmentFrameworkMetadata_id :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_id = Lens.lens (\AssessmentFrameworkMetadata' {id} -> id) (\s@AssessmentFrameworkMetadata' {} a -> s {id = a} :: AssessmentFrameworkMetadata)

-- | The logo that\'s associated with the framework.
assessmentFrameworkMetadata_logo :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_logo = Lens.lens (\AssessmentFrameworkMetadata' {logo} -> logo) (\s@AssessmentFrameworkMetadata' {} a -> s {logo = a} :: AssessmentFrameworkMetadata)

-- | The number of controls that are associated with the framework.
assessmentFrameworkMetadata_controlsCount :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Int)
assessmentFrameworkMetadata_controlsCount = Lens.lens (\AssessmentFrameworkMetadata' {controlsCount} -> controlsCount) (\s@AssessmentFrameworkMetadata' {} a -> s {controlsCount = a} :: AssessmentFrameworkMetadata)

-- | The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
assessmentFrameworkMetadata_complianceType :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_complianceType = Lens.lens (\AssessmentFrameworkMetadata' {complianceType} -> complianceType) (\s@AssessmentFrameworkMetadata' {} a -> s {complianceType = a} :: AssessmentFrameworkMetadata)

-- | Specifies when the framework was created.
assessmentFrameworkMetadata_createdAt :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkMetadata_createdAt = Lens.lens (\AssessmentFrameworkMetadata' {createdAt} -> createdAt) (\s@AssessmentFrameworkMetadata' {} a -> s {createdAt = a} :: AssessmentFrameworkMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AssessmentFrameworkMetadata where
  parseJSON =
    Core.withObject
      "AssessmentFrameworkMetadata"
      ( \x ->
          AssessmentFrameworkMetadata'
            Prelude.<$> (x Core..:? "controlSetsCount")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "logo")
            Prelude.<*> (x Core..:? "controlsCount")
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable AssessmentFrameworkMetadata where
  hashWithSalt _salt AssessmentFrameworkMetadata' {..} =
    _salt `Prelude.hashWithSalt` controlSetsCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` logo
      `Prelude.hashWithSalt` controlsCount
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData AssessmentFrameworkMetadata where
  rnf AssessmentFrameworkMetadata' {..} =
    Prelude.rnf controlSetsCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf logo
      `Prelude.seq` Prelude.rnf controlsCount
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf createdAt
