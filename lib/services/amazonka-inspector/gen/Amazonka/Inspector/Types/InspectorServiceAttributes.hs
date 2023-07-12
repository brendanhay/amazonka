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
-- Module      : Amazonka.Inspector.Types.InspectorServiceAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.InspectorServiceAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data type is used in the Finding data type.
--
-- /See:/ 'newInspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { -- | The ARN of the assessment run during which the finding is generated.
    assessmentRunArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the rules package that is used to generate the finding.
    rulesPackageArn :: Prelude.Maybe Prelude.Text,
    -- | The schema version of this data type.
    schemaVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InspectorServiceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunArn', 'inspectorServiceAttributes_assessmentRunArn' - The ARN of the assessment run during which the finding is generated.
--
-- 'rulesPackageArn', 'inspectorServiceAttributes_rulesPackageArn' - The ARN of the rules package that is used to generate the finding.
--
-- 'schemaVersion', 'inspectorServiceAttributes_schemaVersion' - The schema version of this data type.
newInspectorServiceAttributes ::
  -- | 'schemaVersion'
  Prelude.Natural ->
  InspectorServiceAttributes
newInspectorServiceAttributes pSchemaVersion_ =
  InspectorServiceAttributes'
    { assessmentRunArn =
        Prelude.Nothing,
      rulesPackageArn = Prelude.Nothing,
      schemaVersion = pSchemaVersion_
    }

-- | The ARN of the assessment run during which the finding is generated.
inspectorServiceAttributes_assessmentRunArn :: Lens.Lens' InspectorServiceAttributes (Prelude.Maybe Prelude.Text)
inspectorServiceAttributes_assessmentRunArn = Lens.lens (\InspectorServiceAttributes' {assessmentRunArn} -> assessmentRunArn) (\s@InspectorServiceAttributes' {} a -> s {assessmentRunArn = a} :: InspectorServiceAttributes)

-- | The ARN of the rules package that is used to generate the finding.
inspectorServiceAttributes_rulesPackageArn :: Lens.Lens' InspectorServiceAttributes (Prelude.Maybe Prelude.Text)
inspectorServiceAttributes_rulesPackageArn = Lens.lens (\InspectorServiceAttributes' {rulesPackageArn} -> rulesPackageArn) (\s@InspectorServiceAttributes' {} a -> s {rulesPackageArn = a} :: InspectorServiceAttributes)

-- | The schema version of this data type.
inspectorServiceAttributes_schemaVersion :: Lens.Lens' InspectorServiceAttributes Prelude.Natural
inspectorServiceAttributes_schemaVersion = Lens.lens (\InspectorServiceAttributes' {schemaVersion} -> schemaVersion) (\s@InspectorServiceAttributes' {} a -> s {schemaVersion = a} :: InspectorServiceAttributes)

instance Data.FromJSON InspectorServiceAttributes where
  parseJSON =
    Data.withObject
      "InspectorServiceAttributes"
      ( \x ->
          InspectorServiceAttributes'
            Prelude.<$> (x Data..:? "assessmentRunArn")
            Prelude.<*> (x Data..:? "rulesPackageArn")
            Prelude.<*> (x Data..: "schemaVersion")
      )

instance Prelude.Hashable InspectorServiceAttributes where
  hashWithSalt _salt InspectorServiceAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentRunArn
      `Prelude.hashWithSalt` rulesPackageArn
      `Prelude.hashWithSalt` schemaVersion

instance Prelude.NFData InspectorServiceAttributes where
  rnf InspectorServiceAttributes' {..} =
    Prelude.rnf assessmentRunArn
      `Prelude.seq` Prelude.rnf rulesPackageArn
      `Prelude.seq` Prelude.rnf schemaVersion
