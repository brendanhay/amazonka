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
-- Module      : Amazonka.Connect.Types.SecurityProfileSearchCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SecurityProfileSearchCriteria where

import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return security profiles.
--
-- The @name@ field support \"contains\" queries with a minimum of 2
-- characters and maximum of 25 characters. Any queries with character
-- lengths outside of this range will throw invalid results.
--
-- /See:/ 'newSecurityProfileSearchCriteria' smart constructor.
data SecurityProfileSearchCriteria = SecurityProfileSearchCriteria'
  { stringCondition :: Prelude.Maybe StringCondition,
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [SecurityProfileSearchCriteria],
    -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [SecurityProfileSearchCriteria]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringCondition', 'securityProfileSearchCriteria_stringCondition' - Undocumented member.
--
-- 'orConditions', 'securityProfileSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'andConditions', 'securityProfileSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
newSecurityProfileSearchCriteria ::
  SecurityProfileSearchCriteria
newSecurityProfileSearchCriteria =
  SecurityProfileSearchCriteria'
    { stringCondition =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      andConditions = Prelude.Nothing
    }

-- | Undocumented member.
securityProfileSearchCriteria_stringCondition :: Lens.Lens' SecurityProfileSearchCriteria (Prelude.Maybe StringCondition)
securityProfileSearchCriteria_stringCondition = Lens.lens (\SecurityProfileSearchCriteria' {stringCondition} -> stringCondition) (\s@SecurityProfileSearchCriteria' {} a -> s {stringCondition = a} :: SecurityProfileSearchCriteria)

-- | A list of conditions which would be applied together with an OR
-- condition.
securityProfileSearchCriteria_orConditions :: Lens.Lens' SecurityProfileSearchCriteria (Prelude.Maybe [SecurityProfileSearchCriteria])
securityProfileSearchCriteria_orConditions = Lens.lens (\SecurityProfileSearchCriteria' {orConditions} -> orConditions) (\s@SecurityProfileSearchCriteria' {} a -> s {orConditions = a} :: SecurityProfileSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an AND
-- condition.
securityProfileSearchCriteria_andConditions :: Lens.Lens' SecurityProfileSearchCriteria (Prelude.Maybe [SecurityProfileSearchCriteria])
securityProfileSearchCriteria_andConditions = Lens.lens (\SecurityProfileSearchCriteria' {andConditions} -> andConditions) (\s@SecurityProfileSearchCriteria' {} a -> s {andConditions = a} :: SecurityProfileSearchCriteria) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SecurityProfileSearchCriteria
  where
  hashWithSalt _salt SecurityProfileSearchCriteria' {..} =
    _salt `Prelude.hashWithSalt` stringCondition
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` andConditions

instance Prelude.NFData SecurityProfileSearchCriteria where
  rnf SecurityProfileSearchCriteria' {..} =
    Prelude.rnf stringCondition
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf andConditions

instance Core.ToJSON SecurityProfileSearchCriteria where
  toJSON SecurityProfileSearchCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StringCondition" Core..=)
              Prelude.<$> stringCondition,
            ("OrConditions" Core..=) Prelude.<$> orConditions,
            ("AndConditions" Core..=) Prelude.<$> andConditions
          ]
      )
