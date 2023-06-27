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
-- Module      : Amazonka.Connect.Types.ControlPlaneTagFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ControlPlaneTagFilter where

import Amazonka.Connect.Types.TagCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that can be used to specify Tag conditions inside the
-- @SearchFilter@. This accepts an @OR@ of @AND@ (List of List) input
-- where:
--
-- -   Top level list specifies conditions that need to be applied with
--     @OR@ operator
--
-- -   Inner list specifies conditions that need to be applied with @AND@
--     operator.
--
-- /See:/ 'newControlPlaneTagFilter' smart constructor.
data ControlPlaneTagFilter = ControlPlaneTagFilter'
  { -- | A list of conditions which would be applied together with an @AND@
    -- condition.
    andConditions :: Prelude.Maybe [TagCondition],
    -- | A list of conditions which would be applied together with an @OR@
    -- condition.
    orConditions :: Prelude.Maybe [[TagCondition]],
    -- | A leaf node condition which can be used to specify a tag condition.
    tagCondition :: Prelude.Maybe TagCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlPlaneTagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'controlPlaneTagFilter_andConditions' - A list of conditions which would be applied together with an @AND@
-- condition.
--
-- 'orConditions', 'controlPlaneTagFilter_orConditions' - A list of conditions which would be applied together with an @OR@
-- condition.
--
-- 'tagCondition', 'controlPlaneTagFilter_tagCondition' - A leaf node condition which can be used to specify a tag condition.
newControlPlaneTagFilter ::
  ControlPlaneTagFilter
newControlPlaneTagFilter =
  ControlPlaneTagFilter'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      tagCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an @AND@
-- condition.
controlPlaneTagFilter_andConditions :: Lens.Lens' ControlPlaneTagFilter (Prelude.Maybe [TagCondition])
controlPlaneTagFilter_andConditions = Lens.lens (\ControlPlaneTagFilter' {andConditions} -> andConditions) (\s@ControlPlaneTagFilter' {} a -> s {andConditions = a} :: ControlPlaneTagFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an @OR@
-- condition.
controlPlaneTagFilter_orConditions :: Lens.Lens' ControlPlaneTagFilter (Prelude.Maybe [[TagCondition]])
controlPlaneTagFilter_orConditions = Lens.lens (\ControlPlaneTagFilter' {orConditions} -> orConditions) (\s@ControlPlaneTagFilter' {} a -> s {orConditions = a} :: ControlPlaneTagFilter) Prelude.. Lens.mapping Lens.coerced

-- | A leaf node condition which can be used to specify a tag condition.
controlPlaneTagFilter_tagCondition :: Lens.Lens' ControlPlaneTagFilter (Prelude.Maybe TagCondition)
controlPlaneTagFilter_tagCondition = Lens.lens (\ControlPlaneTagFilter' {tagCondition} -> tagCondition) (\s@ControlPlaneTagFilter' {} a -> s {tagCondition = a} :: ControlPlaneTagFilter)

instance Prelude.Hashable ControlPlaneTagFilter where
  hashWithSalt _salt ControlPlaneTagFilter' {..} =
    _salt
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` tagCondition

instance Prelude.NFData ControlPlaneTagFilter where
  rnf ControlPlaneTagFilter' {..} =
    Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf tagCondition

instance Data.ToJSON ControlPlaneTagFilter where
  toJSON ControlPlaneTagFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("TagCondition" Data..=) Prelude.<$> tagCondition
          ]
      )
