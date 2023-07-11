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
-- Module      : Amazonka.QuickSight.Types.FieldSort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldSort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SortDirection

-- | The sort configuration for a field in a field well.
--
-- /See:/ 'newFieldSort' smart constructor.
data FieldSort = FieldSort'
  { -- | The sort configuration target field.
    fieldId :: Prelude.Text,
    -- | The sort direction. Choose one of the following options:
    --
    -- -   @ASC@: Ascending
    --
    -- -   @DESC@: Descending
    direction :: SortDirection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldSort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'fieldSort_fieldId' - The sort configuration target field.
--
-- 'direction', 'fieldSort_direction' - The sort direction. Choose one of the following options:
--
-- -   @ASC@: Ascending
--
-- -   @DESC@: Descending
newFieldSort ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'direction'
  SortDirection ->
  FieldSort
newFieldSort pFieldId_ pDirection_ =
  FieldSort'
    { fieldId = pFieldId_,
      direction = pDirection_
    }

-- | The sort configuration target field.
fieldSort_fieldId :: Lens.Lens' FieldSort Prelude.Text
fieldSort_fieldId = Lens.lens (\FieldSort' {fieldId} -> fieldId) (\s@FieldSort' {} a -> s {fieldId = a} :: FieldSort)

-- | The sort direction. Choose one of the following options:
--
-- -   @ASC@: Ascending
--
-- -   @DESC@: Descending
fieldSort_direction :: Lens.Lens' FieldSort SortDirection
fieldSort_direction = Lens.lens (\FieldSort' {direction} -> direction) (\s@FieldSort' {} a -> s {direction = a} :: FieldSort)

instance Data.FromJSON FieldSort where
  parseJSON =
    Data.withObject
      "FieldSort"
      ( \x ->
          FieldSort'
            Prelude.<$> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Direction")
      )

instance Prelude.Hashable FieldSort where
  hashWithSalt _salt FieldSort' {..} =
    _salt
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` direction

instance Prelude.NFData FieldSort where
  rnf FieldSort' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf direction

instance Data.ToJSON FieldSort where
  toJSON FieldSort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Direction" Data..= direction)
          ]
      )
