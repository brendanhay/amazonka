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
-- Module      : Amazonka.LexV2Models.Types.RuntimeHintDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.RuntimeHintDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.RuntimeHintValue
import qualified Amazonka.Prelude as Prelude

-- | Provides an array of phrases that should be given preference when
-- resolving values for a slot.
--
-- /See:/ 'newRuntimeHintDetails' smart constructor.
data RuntimeHintDetails = RuntimeHintDetails'
  { -- | One or more strings that Amazon Lex should look for in the input to the
    -- bot. Each phrase is given preference when deciding on slot values.
    runtimeHintValues :: Prelude.Maybe (Prelude.NonEmpty RuntimeHintValue),
    -- | A map of constituent sub slot names inside a composite slot in the
    -- intent and the phrases that should be added for each sub slot. Inside
    -- each composite slot hints, this structure provides a mechanism to add
    -- granular sub slot phrases. Only sub slot hints are supported for
    -- composite slots. The intent name, composite slot name and the
    -- constituent sub slot names must exist.
    subSlotHints :: Prelude.Maybe (Prelude.HashMap Name RuntimeHintDetails)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeHintDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeHintValues', 'runtimeHintDetails_runtimeHintValues' - One or more strings that Amazon Lex should look for in the input to the
-- bot. Each phrase is given preference when deciding on slot values.
--
-- 'subSlotHints', 'runtimeHintDetails_subSlotHints' - A map of constituent sub slot names inside a composite slot in the
-- intent and the phrases that should be added for each sub slot. Inside
-- each composite slot hints, this structure provides a mechanism to add
-- granular sub slot phrases. Only sub slot hints are supported for
-- composite slots. The intent name, composite slot name and the
-- constituent sub slot names must exist.
newRuntimeHintDetails ::
  RuntimeHintDetails
newRuntimeHintDetails =
  RuntimeHintDetails'
    { runtimeHintValues =
        Prelude.Nothing,
      subSlotHints = Prelude.Nothing
    }

-- | One or more strings that Amazon Lex should look for in the input to the
-- bot. Each phrase is given preference when deciding on slot values.
runtimeHintDetails_runtimeHintValues :: Lens.Lens' RuntimeHintDetails (Prelude.Maybe (Prelude.NonEmpty RuntimeHintValue))
runtimeHintDetails_runtimeHintValues = Lens.lens (\RuntimeHintDetails' {runtimeHintValues} -> runtimeHintValues) (\s@RuntimeHintDetails' {} a -> s {runtimeHintValues = a} :: RuntimeHintDetails) Prelude.. Lens.mapping Lens.coerced

-- | A map of constituent sub slot names inside a composite slot in the
-- intent and the phrases that should be added for each sub slot. Inside
-- each composite slot hints, this structure provides a mechanism to add
-- granular sub slot phrases. Only sub slot hints are supported for
-- composite slots. The intent name, composite slot name and the
-- constituent sub slot names must exist.
runtimeHintDetails_subSlotHints :: Lens.Lens' RuntimeHintDetails (Prelude.Maybe (Prelude.HashMap Name RuntimeHintDetails))
runtimeHintDetails_subSlotHints = Lens.lens (\RuntimeHintDetails' {subSlotHints} -> subSlotHints) (\s@RuntimeHintDetails' {} a -> s {subSlotHints = a} :: RuntimeHintDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RuntimeHintDetails where
  parseJSON =
    Data.withObject
      "RuntimeHintDetails"
      ( \x ->
          RuntimeHintDetails'
            Prelude.<$> (x Data..:? "runtimeHintValues")
            Prelude.<*> (x Data..:? "subSlotHints" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RuntimeHintDetails where
  hashWithSalt _salt RuntimeHintDetails' {..} =
    _salt
      `Prelude.hashWithSalt` runtimeHintValues
      `Prelude.hashWithSalt` subSlotHints

instance Prelude.NFData RuntimeHintDetails where
  rnf RuntimeHintDetails' {..} =
    Prelude.rnf runtimeHintValues
      `Prelude.seq` Prelude.rnf subSlotHints
