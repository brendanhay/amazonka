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
-- Module      : Amazonka.LexV2Models.Types.AssociatedTranscriptFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AssociatedTranscriptFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AssociatedTranscriptFilterName
import qualified Amazonka.Prelude as Prelude

-- | Filters to search for the associated transcript.
--
-- /See:/ 'newAssociatedTranscriptFilter' smart constructor.
data AssociatedTranscriptFilter = AssociatedTranscriptFilter'
  { -- | The name of the field to use for filtering. The allowed names are
    -- IntentId and SlotTypeId.
    name :: AssociatedTranscriptFilterName,
    -- | The values to use to filter the transcript.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedTranscriptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'associatedTranscriptFilter_name' - The name of the field to use for filtering. The allowed names are
-- IntentId and SlotTypeId.
--
-- 'values', 'associatedTranscriptFilter_values' - The values to use to filter the transcript.
newAssociatedTranscriptFilter ::
  -- | 'name'
  AssociatedTranscriptFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  AssociatedTranscriptFilter
newAssociatedTranscriptFilter pName_ pValues_ =
  AssociatedTranscriptFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the field to use for filtering. The allowed names are
-- IntentId and SlotTypeId.
associatedTranscriptFilter_name :: Lens.Lens' AssociatedTranscriptFilter AssociatedTranscriptFilterName
associatedTranscriptFilter_name = Lens.lens (\AssociatedTranscriptFilter' {name} -> name) (\s@AssociatedTranscriptFilter' {} a -> s {name = a} :: AssociatedTranscriptFilter)

-- | The values to use to filter the transcript.
associatedTranscriptFilter_values :: Lens.Lens' AssociatedTranscriptFilter (Prelude.NonEmpty Prelude.Text)
associatedTranscriptFilter_values = Lens.lens (\AssociatedTranscriptFilter' {values} -> values) (\s@AssociatedTranscriptFilter' {} a -> s {values = a} :: AssociatedTranscriptFilter) Prelude.. Lens.coerced

instance Prelude.Hashable AssociatedTranscriptFilter where
  hashWithSalt _salt AssociatedTranscriptFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData AssociatedTranscriptFilter where
  rnf AssociatedTranscriptFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON AssociatedTranscriptFilter where
  toJSON AssociatedTranscriptFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values)
          ]
      )
