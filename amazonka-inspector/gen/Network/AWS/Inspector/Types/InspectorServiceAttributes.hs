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
-- Module      : Network.AWS.Inspector.Types.InspectorServiceAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorServiceAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used in the Finding data type.
--
-- /See:/ 'newInspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { -- | The ARN of the rules package that is used to generate the finding.
    rulesPackageArn :: Core.Maybe Core.Text,
    -- | The ARN of the assessment run during which the finding is generated.
    assessmentRunArn :: Core.Maybe Core.Text,
    -- | The schema version of this data type.
    schemaVersion :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InspectorServiceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rulesPackageArn', 'inspectorServiceAttributes_rulesPackageArn' - The ARN of the rules package that is used to generate the finding.
--
-- 'assessmentRunArn', 'inspectorServiceAttributes_assessmentRunArn' - The ARN of the assessment run during which the finding is generated.
--
-- 'schemaVersion', 'inspectorServiceAttributes_schemaVersion' - The schema version of this data type.
newInspectorServiceAttributes ::
  -- | 'schemaVersion'
  Core.Natural ->
  InspectorServiceAttributes
newInspectorServiceAttributes pSchemaVersion_ =
  InspectorServiceAttributes'
    { rulesPackageArn =
        Core.Nothing,
      assessmentRunArn = Core.Nothing,
      schemaVersion = pSchemaVersion_
    }

-- | The ARN of the rules package that is used to generate the finding.
inspectorServiceAttributes_rulesPackageArn :: Lens.Lens' InspectorServiceAttributes (Core.Maybe Core.Text)
inspectorServiceAttributes_rulesPackageArn = Lens.lens (\InspectorServiceAttributes' {rulesPackageArn} -> rulesPackageArn) (\s@InspectorServiceAttributes' {} a -> s {rulesPackageArn = a} :: InspectorServiceAttributes)

-- | The ARN of the assessment run during which the finding is generated.
inspectorServiceAttributes_assessmentRunArn :: Lens.Lens' InspectorServiceAttributes (Core.Maybe Core.Text)
inspectorServiceAttributes_assessmentRunArn = Lens.lens (\InspectorServiceAttributes' {assessmentRunArn} -> assessmentRunArn) (\s@InspectorServiceAttributes' {} a -> s {assessmentRunArn = a} :: InspectorServiceAttributes)

-- | The schema version of this data type.
inspectorServiceAttributes_schemaVersion :: Lens.Lens' InspectorServiceAttributes Core.Natural
inspectorServiceAttributes_schemaVersion = Lens.lens (\InspectorServiceAttributes' {schemaVersion} -> schemaVersion) (\s@InspectorServiceAttributes' {} a -> s {schemaVersion = a} :: InspectorServiceAttributes)

instance Core.FromJSON InspectorServiceAttributes where
  parseJSON =
    Core.withObject
      "InspectorServiceAttributes"
      ( \x ->
          InspectorServiceAttributes'
            Core.<$> (x Core..:? "rulesPackageArn")
            Core.<*> (x Core..:? "assessmentRunArn")
            Core.<*> (x Core..: "schemaVersion")
      )

instance Core.Hashable InspectorServiceAttributes

instance Core.NFData InspectorServiceAttributes
