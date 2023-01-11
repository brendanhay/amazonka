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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.UnmappedAttribute where

import Amazonka.ComprehendMedical.Types.Attribute
import Amazonka.ComprehendMedical.Types.EntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An attribute that was extracted, but Comprehend Medical; was unable to
-- relate to an entity.
--
-- /See:/ 'newUnmappedAttribute' smart constructor.
data UnmappedAttribute = UnmappedAttribute'
  { -- | The specific attribute that has been extracted but not mapped to an
    -- entity.
    attribute :: Prelude.Maybe Attribute,
    -- | The type of the unmapped attribute, could be one of the following
    -- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
    -- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
    type' :: Prelude.Maybe EntityType
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
-- 'attribute', 'unmappedAttribute_attribute' - The specific attribute that has been extracted but not mapped to an
-- entity.
--
-- 'type'', 'unmappedAttribute_type' - The type of the unmapped attribute, could be one of the following
-- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
-- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
newUnmappedAttribute ::
  UnmappedAttribute
newUnmappedAttribute =
  UnmappedAttribute'
    { attribute = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The specific attribute that has been extracted but not mapped to an
-- entity.
unmappedAttribute_attribute :: Lens.Lens' UnmappedAttribute (Prelude.Maybe Attribute)
unmappedAttribute_attribute = Lens.lens (\UnmappedAttribute' {attribute} -> attribute) (\s@UnmappedAttribute' {} a -> s {attribute = a} :: UnmappedAttribute)

-- | The type of the unmapped attribute, could be one of the following
-- values: \"MEDICATION\", \"MEDICAL_CONDITION\", \"ANATOMY\",
-- \"TEST_AND_TREATMENT_PROCEDURE\" or \"PROTECTED_HEALTH_INFORMATION\".
unmappedAttribute_type :: Lens.Lens' UnmappedAttribute (Prelude.Maybe EntityType)
unmappedAttribute_type = Lens.lens (\UnmappedAttribute' {type'} -> type') (\s@UnmappedAttribute' {} a -> s {type' = a} :: UnmappedAttribute)

instance Data.FromJSON UnmappedAttribute where
  parseJSON =
    Data.withObject
      "UnmappedAttribute"
      ( \x ->
          UnmappedAttribute'
            Prelude.<$> (x Data..:? "Attribute")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable UnmappedAttribute where
  hashWithSalt _salt UnmappedAttribute' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UnmappedAttribute where
  rnf UnmappedAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf type'
