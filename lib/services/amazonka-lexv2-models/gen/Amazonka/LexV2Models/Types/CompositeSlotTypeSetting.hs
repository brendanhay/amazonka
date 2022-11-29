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
-- Module      : Amazonka.LexV2Models.Types.CompositeSlotTypeSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CompositeSlotTypeSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.SubSlotTypeComposition
import qualified Amazonka.Prelude as Prelude

-- | A composite slot is a combination of two or more slots that capture
-- multiple pieces of information in a single user input.
--
-- /See:/ 'newCompositeSlotTypeSetting' smart constructor.
data CompositeSlotTypeSetting = CompositeSlotTypeSetting'
  { -- | Subslots in the composite slot.
    subSlots :: Prelude.Maybe [SubSlotTypeComposition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompositeSlotTypeSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subSlots', 'compositeSlotTypeSetting_subSlots' - Subslots in the composite slot.
newCompositeSlotTypeSetting ::
  CompositeSlotTypeSetting
newCompositeSlotTypeSetting =
  CompositeSlotTypeSetting'
    { subSlots =
        Prelude.Nothing
    }

-- | Subslots in the composite slot.
compositeSlotTypeSetting_subSlots :: Lens.Lens' CompositeSlotTypeSetting (Prelude.Maybe [SubSlotTypeComposition])
compositeSlotTypeSetting_subSlots = Lens.lens (\CompositeSlotTypeSetting' {subSlots} -> subSlots) (\s@CompositeSlotTypeSetting' {} a -> s {subSlots = a} :: CompositeSlotTypeSetting) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CompositeSlotTypeSetting where
  parseJSON =
    Core.withObject
      "CompositeSlotTypeSetting"
      ( \x ->
          CompositeSlotTypeSetting'
            Prelude.<$> (x Core..:? "subSlots" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CompositeSlotTypeSetting where
  hashWithSalt _salt CompositeSlotTypeSetting' {..} =
    _salt `Prelude.hashWithSalt` subSlots

instance Prelude.NFData CompositeSlotTypeSetting where
  rnf CompositeSlotTypeSetting' {..} =
    Prelude.rnf subSlots

instance Core.ToJSON CompositeSlotTypeSetting where
  toJSON CompositeSlotTypeSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [("subSlots" Core..=) Prelude.<$> subSlots]
      )
