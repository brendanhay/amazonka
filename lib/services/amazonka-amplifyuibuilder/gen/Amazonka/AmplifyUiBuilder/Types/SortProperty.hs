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
-- Module      : Amazonka.AmplifyUiBuilder.Types.SortProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.SortProperty where

import Amazonka.AmplifyUiBuilder.Types.SortDirection
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes how to sort the data that you bind to a component.
--
-- /See:/ 'newSortProperty' smart constructor.
data SortProperty = SortProperty'
  { -- | The field to perform the sort on.
    field :: Prelude.Text,
    -- | The direction of the sort, either ascending or descending.
    direction :: SortDirection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'sortProperty_field' - The field to perform the sort on.
--
-- 'direction', 'sortProperty_direction' - The direction of the sort, either ascending or descending.
newSortProperty ::
  -- | 'field'
  Prelude.Text ->
  -- | 'direction'
  SortDirection ->
  SortProperty
newSortProperty pField_ pDirection_ =
  SortProperty'
    { field = pField_,
      direction = pDirection_
    }

-- | The field to perform the sort on.
sortProperty_field :: Lens.Lens' SortProperty Prelude.Text
sortProperty_field = Lens.lens (\SortProperty' {field} -> field) (\s@SortProperty' {} a -> s {field = a} :: SortProperty)

-- | The direction of the sort, either ascending or descending.
sortProperty_direction :: Lens.Lens' SortProperty SortDirection
sortProperty_direction = Lens.lens (\SortProperty' {direction} -> direction) (\s@SortProperty' {} a -> s {direction = a} :: SortProperty)

instance Data.FromJSON SortProperty where
  parseJSON =
    Data.withObject
      "SortProperty"
      ( \x ->
          SortProperty'
            Prelude.<$> (x Data..: "field")
            Prelude.<*> (x Data..: "direction")
      )

instance Prelude.Hashable SortProperty where
  hashWithSalt _salt SortProperty' {..} =
    _salt
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` direction

instance Prelude.NFData SortProperty where
  rnf SortProperty' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf direction

instance Data.ToJSON SortProperty where
  toJSON SortProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("field" Data..= field),
            Prelude.Just ("direction" Data..= direction)
          ]
      )
