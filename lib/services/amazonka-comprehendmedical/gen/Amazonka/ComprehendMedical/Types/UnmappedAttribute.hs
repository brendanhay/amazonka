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
-- Module      : Amazonka.ComprehendMedical.Types.UnmappedAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.UnmappedAttribute where

import Amazonka.ComprehendMedical.Types.Attribute
import Amazonka.ComprehendMedical.Types.EntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An attribute that was extracted, but Comprehend Medical; was unable to
-- relate to an entity.
--
-- /See:/ 'newUnmappedAttribute' smart constructor.
data UnmappedAttribute = UnmappedAttribute'
  { -- | The type of the unmapped attribute, could be one of the following
    -- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
    -- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
    type' :: Prelude.Maybe EntityType,
    -- | The specific attribute that has been extracted but not mapped to an
    -- entity.
    attribute :: Prelude.Maybe Attribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnmappedAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'unmappedAttribute_type' - The type of the unmapped attribute, could be one of the following
-- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
-- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
--
-- 'attribute', 'unmappedAttribute_attribute' - The specific attribute that has been extracted but not mapped to an
-- entity.
newUnmappedAttribute ::
  UnmappedAttribute
newUnmappedAttribute =
  UnmappedAttribute'
    { type' = Prelude.Nothing,
      attribute = Prelude.Nothing
    }

-- | The type of the unmapped attribute, could be one of the following
-- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
-- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
unmappedAttribute_type :: Lens.Lens' UnmappedAttribute (Prelude.Maybe EntityType)
unmappedAttribute_type = Lens.lens (\UnmappedAttribute' {type'} -> type') (\s@UnmappedAttribute' {} a -> s {type' = a} :: UnmappedAttribute)

-- | The specific attribute that has been extracted but not mapped to an
-- entity.
unmappedAttribute_attribute :: Lens.Lens' UnmappedAttribute (Prelude.Maybe Attribute)
unmappedAttribute_attribute = Lens.lens (\UnmappedAttribute' {attribute} -> attribute) (\s@UnmappedAttribute' {} a -> s {attribute = a} :: UnmappedAttribute)

instance Core.FromJSON UnmappedAttribute where
  parseJSON =
    Core.withObject
      "UnmappedAttribute"
      ( \x ->
          UnmappedAttribute'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Attribute")
      )

instance Prelude.Hashable UnmappedAttribute where
  hashWithSalt _salt UnmappedAttribute' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` attribute

instance Prelude.NFData UnmappedAttribute where
  rnf UnmappedAttribute' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf attribute
