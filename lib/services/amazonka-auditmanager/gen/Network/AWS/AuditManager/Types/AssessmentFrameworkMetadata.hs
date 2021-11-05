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
-- Module      : Network.AWS.AuditManager.Types.AssessmentFrameworkMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.AssessmentFrameworkMetadata where

import Network.AWS.AuditManager.Types.FrameworkType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata associated with a standard or custom framework.
--
-- /See:/ 'newAssessmentFrameworkMetadata' smart constructor.
data AssessmentFrameworkMetadata = AssessmentFrameworkMetadata'
  { -- | The number of controls associated with the specified framework.
    controlsCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies when the framework was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the framework.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the framework was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the specified framework.
    name :: Prelude.Maybe Prelude.Text,
    -- | The compliance type that the new custom framework supports, such as CIS
    -- or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The number of control sets associated with the specified framework.
    controlSetsCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identified for the specified framework.
    id :: Prelude.Maybe Prelude.Text,
    -- | The framework type, such as standard or custom.
    type' :: Prelude.Maybe FrameworkType,
    -- | The logo associated with the framework.
    logo :: Prelude.Maybe Prelude.Text,
    -- | The description of the specified framework.
    description :: Prelude.Maybe Prelude.Text
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
-- 'controlsCount', 'assessmentFrameworkMetadata_controlsCount' - The number of controls associated with the specified framework.
--
-- 'lastUpdatedAt', 'assessmentFrameworkMetadata_lastUpdatedAt' - Specifies when the framework was most recently updated.
--
-- 'arn', 'assessmentFrameworkMetadata_arn' - The Amazon Resource Name (ARN) of the framework.
--
-- 'createdAt', 'assessmentFrameworkMetadata_createdAt' - Specifies when the framework was created.
--
-- 'name', 'assessmentFrameworkMetadata_name' - The name of the specified framework.
--
-- 'complianceType', 'assessmentFrameworkMetadata_complianceType' - The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
--
-- 'controlSetsCount', 'assessmentFrameworkMetadata_controlSetsCount' - The number of control sets associated with the specified framework.
--
-- 'id', 'assessmentFrameworkMetadata_id' - The unique identified for the specified framework.
--
-- 'type'', 'assessmentFrameworkMetadata_type' - The framework type, such as standard or custom.
--
-- 'logo', 'assessmentFrameworkMetadata_logo' - The logo associated with the framework.
--
-- 'description', 'assessmentFrameworkMetadata_description' - The description of the specified framework.
newAssessmentFrameworkMetadata ::
  AssessmentFrameworkMetadata
newAssessmentFrameworkMetadata =
  AssessmentFrameworkMetadata'
    { controlsCount =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      controlSetsCount = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      logo = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The number of controls associated with the specified framework.
assessmentFrameworkMetadata_controlsCount :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Int)
assessmentFrameworkMetadata_controlsCount = Lens.lens (\AssessmentFrameworkMetadata' {controlsCount} -> controlsCount) (\s@AssessmentFrameworkMetadata' {} a -> s {controlsCount = a} :: AssessmentFrameworkMetadata)

-- | Specifies when the framework was most recently updated.
assessmentFrameworkMetadata_lastUpdatedAt :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkMetadata_lastUpdatedAt = Lens.lens (\AssessmentFrameworkMetadata' {lastUpdatedAt} -> lastUpdatedAt) (\s@AssessmentFrameworkMetadata' {} a -> s {lastUpdatedAt = a} :: AssessmentFrameworkMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the framework.
assessmentFrameworkMetadata_arn :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_arn = Lens.lens (\AssessmentFrameworkMetadata' {arn} -> arn) (\s@AssessmentFrameworkMetadata' {} a -> s {arn = a} :: AssessmentFrameworkMetadata)

-- | Specifies when the framework was created.
assessmentFrameworkMetadata_createdAt :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkMetadata_createdAt = Lens.lens (\AssessmentFrameworkMetadata' {createdAt} -> createdAt) (\s@AssessmentFrameworkMetadata' {} a -> s {createdAt = a} :: AssessmentFrameworkMetadata) Prelude.. Lens.mapping Core._Time

-- | The name of the specified framework.
assessmentFrameworkMetadata_name :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_name = Lens.lens (\AssessmentFrameworkMetadata' {name} -> name) (\s@AssessmentFrameworkMetadata' {} a -> s {name = a} :: AssessmentFrameworkMetadata)

-- | The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
assessmentFrameworkMetadata_complianceType :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_complianceType = Lens.lens (\AssessmentFrameworkMetadata' {complianceType} -> complianceType) (\s@AssessmentFrameworkMetadata' {} a -> s {complianceType = a} :: AssessmentFrameworkMetadata)

-- | The number of control sets associated with the specified framework.
assessmentFrameworkMetadata_controlSetsCount :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Int)
assessmentFrameworkMetadata_controlSetsCount = Lens.lens (\AssessmentFrameworkMetadata' {controlSetsCount} -> controlSetsCount) (\s@AssessmentFrameworkMetadata' {} a -> s {controlSetsCount = a} :: AssessmentFrameworkMetadata)

-- | The unique identified for the specified framework.
assessmentFrameworkMetadata_id :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_id = Lens.lens (\AssessmentFrameworkMetadata' {id} -> id) (\s@AssessmentFrameworkMetadata' {} a -> s {id = a} :: AssessmentFrameworkMetadata)

-- | The framework type, such as standard or custom.
assessmentFrameworkMetadata_type :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe FrameworkType)
assessmentFrameworkMetadata_type = Lens.lens (\AssessmentFrameworkMetadata' {type'} -> type') (\s@AssessmentFrameworkMetadata' {} a -> s {type' = a} :: AssessmentFrameworkMetadata)

-- | The logo associated with the framework.
assessmentFrameworkMetadata_logo :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_logo = Lens.lens (\AssessmentFrameworkMetadata' {logo} -> logo) (\s@AssessmentFrameworkMetadata' {} a -> s {logo = a} :: AssessmentFrameworkMetadata)

-- | The description of the specified framework.
assessmentFrameworkMetadata_description :: Lens.Lens' AssessmentFrameworkMetadata (Prelude.Maybe Prelude.Text)
assessmentFrameworkMetadata_description = Lens.lens (\AssessmentFrameworkMetadata' {description} -> description) (\s@AssessmentFrameworkMetadata' {} a -> s {description = a} :: AssessmentFrameworkMetadata)

instance Core.FromJSON AssessmentFrameworkMetadata where
  parseJSON =
    Core.withObject
      "AssessmentFrameworkMetadata"
      ( \x ->
          AssessmentFrameworkMetadata'
            Prelude.<$> (x Core..:? "controlsCount")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "controlSetsCount")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "logo")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable AssessmentFrameworkMetadata

instance Prelude.NFData AssessmentFrameworkMetadata
