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
-- Module      : Amazonka.Connect.Types.RoutingProfileSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileSearchCriteria where

import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return routing profiles.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
--
-- /See:/ 'newRoutingProfileSearchCriteria' smart constructor.
data RoutingProfileSearchCriteria = RoutingProfileSearchCriteria'
  { -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [RoutingProfileSearchCriteria],
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [RoutingProfileSearchCriteria],
    stringCondition :: Prelude.Maybe StringCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'routingProfileSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'orConditions', 'routingProfileSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'stringCondition', 'routingProfileSearchCriteria_stringCondition' - Undocumented member.
newRoutingProfileSearchCriteria ::
  RoutingProfileSearchCriteria
newRoutingProfileSearchCriteria =
  RoutingProfileSearchCriteria'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      stringCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an AND
-- condition.
routingProfileSearchCriteria_andConditions :: Lens.Lens' RoutingProfileSearchCriteria (Prelude.Maybe [RoutingProfileSearchCriteria])
routingProfileSearchCriteria_andConditions = Lens.lens (\RoutingProfileSearchCriteria' {andConditions} -> andConditions) (\s@RoutingProfileSearchCriteria' {} a -> s {andConditions = a} :: RoutingProfileSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an OR
-- condition.
routingProfileSearchCriteria_orConditions :: Lens.Lens' RoutingProfileSearchCriteria (Prelude.Maybe [RoutingProfileSearchCriteria])
routingProfileSearchCriteria_orConditions = Lens.lens (\RoutingProfileSearchCriteria' {orConditions} -> orConditions) (\s@RoutingProfileSearchCriteria' {} a -> s {orConditions = a} :: RoutingProfileSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
routingProfileSearchCriteria_stringCondition :: Lens.Lens' RoutingProfileSearchCriteria (Prelude.Maybe StringCondition)
routingProfileSearchCriteria_stringCondition = Lens.lens (\RoutingProfileSearchCriteria' {stringCondition} -> stringCondition) (\s@RoutingProfileSearchCriteria' {} a -> s {stringCondition = a} :: RoutingProfileSearchCriteria)

instance
  Prelude.Hashable
    RoutingProfileSearchCriteria
  where
  hashWithSalt _salt RoutingProfileSearchCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` stringCondition

instance Prelude.NFData RoutingProfileSearchCriteria where
  rnf RoutingProfileSearchCriteria' {..} =
    Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf stringCondition

instance Data.ToJSON RoutingProfileSearchCriteria where
  toJSON RoutingProfileSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("StringCondition" Data..=)
              Prelude.<$> stringCondition
          ]
      )
