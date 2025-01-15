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
-- Module      : Amazonka.QuickSight.Types.VisualCustomActionOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualCustomActionOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomActionFilterOperation
import Amazonka.QuickSight.Types.CustomActionNavigationOperation
import Amazonka.QuickSight.Types.CustomActionSetParametersOperation
import Amazonka.QuickSight.Types.CustomActionURLOperation

-- | The operation that is defined by the custom action.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newVisualCustomActionOperation' smart constructor.
data VisualCustomActionOperation = VisualCustomActionOperation'
  { -- | The filter operation that filters data included in a visual or in an
    -- entire sheet.
    filterOperation :: Prelude.Maybe CustomActionFilterOperation,
    -- | The navigation operation that navigates between different sheets in the
    -- same analysis.
    navigationOperation :: Prelude.Maybe CustomActionNavigationOperation,
    -- | The set parameter operation that sets parameters in custom action.
    setParametersOperation :: Prelude.Maybe CustomActionSetParametersOperation,
    -- | The URL operation that opens a link to another webpage.
    uRLOperation :: Prelude.Maybe CustomActionURLOperation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualCustomActionOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterOperation', 'visualCustomActionOperation_filterOperation' - The filter operation that filters data included in a visual or in an
-- entire sheet.
--
-- 'navigationOperation', 'visualCustomActionOperation_navigationOperation' - The navigation operation that navigates between different sheets in the
-- same analysis.
--
-- 'setParametersOperation', 'visualCustomActionOperation_setParametersOperation' - The set parameter operation that sets parameters in custom action.
--
-- 'uRLOperation', 'visualCustomActionOperation_uRLOperation' - The URL operation that opens a link to another webpage.
newVisualCustomActionOperation ::
  VisualCustomActionOperation
newVisualCustomActionOperation =
  VisualCustomActionOperation'
    { filterOperation =
        Prelude.Nothing,
      navigationOperation = Prelude.Nothing,
      setParametersOperation = Prelude.Nothing,
      uRLOperation = Prelude.Nothing
    }

-- | The filter operation that filters data included in a visual or in an
-- entire sheet.
visualCustomActionOperation_filterOperation :: Lens.Lens' VisualCustomActionOperation (Prelude.Maybe CustomActionFilterOperation)
visualCustomActionOperation_filterOperation = Lens.lens (\VisualCustomActionOperation' {filterOperation} -> filterOperation) (\s@VisualCustomActionOperation' {} a -> s {filterOperation = a} :: VisualCustomActionOperation)

-- | The navigation operation that navigates between different sheets in the
-- same analysis.
visualCustomActionOperation_navigationOperation :: Lens.Lens' VisualCustomActionOperation (Prelude.Maybe CustomActionNavigationOperation)
visualCustomActionOperation_navigationOperation = Lens.lens (\VisualCustomActionOperation' {navigationOperation} -> navigationOperation) (\s@VisualCustomActionOperation' {} a -> s {navigationOperation = a} :: VisualCustomActionOperation)

-- | The set parameter operation that sets parameters in custom action.
visualCustomActionOperation_setParametersOperation :: Lens.Lens' VisualCustomActionOperation (Prelude.Maybe CustomActionSetParametersOperation)
visualCustomActionOperation_setParametersOperation = Lens.lens (\VisualCustomActionOperation' {setParametersOperation} -> setParametersOperation) (\s@VisualCustomActionOperation' {} a -> s {setParametersOperation = a} :: VisualCustomActionOperation)

-- | The URL operation that opens a link to another webpage.
visualCustomActionOperation_uRLOperation :: Lens.Lens' VisualCustomActionOperation (Prelude.Maybe CustomActionURLOperation)
visualCustomActionOperation_uRLOperation = Lens.lens (\VisualCustomActionOperation' {uRLOperation} -> uRLOperation) (\s@VisualCustomActionOperation' {} a -> s {uRLOperation = a} :: VisualCustomActionOperation)

instance Data.FromJSON VisualCustomActionOperation where
  parseJSON =
    Data.withObject
      "VisualCustomActionOperation"
      ( \x ->
          VisualCustomActionOperation'
            Prelude.<$> (x Data..:? "FilterOperation")
            Prelude.<*> (x Data..:? "NavigationOperation")
            Prelude.<*> (x Data..:? "SetParametersOperation")
            Prelude.<*> (x Data..:? "URLOperation")
      )

instance Prelude.Hashable VisualCustomActionOperation where
  hashWithSalt _salt VisualCustomActionOperation' {..} =
    _salt
      `Prelude.hashWithSalt` filterOperation
      `Prelude.hashWithSalt` navigationOperation
      `Prelude.hashWithSalt` setParametersOperation
      `Prelude.hashWithSalt` uRLOperation

instance Prelude.NFData VisualCustomActionOperation where
  rnf VisualCustomActionOperation' {..} =
    Prelude.rnf filterOperation `Prelude.seq`
      Prelude.rnf navigationOperation `Prelude.seq`
        Prelude.rnf setParametersOperation `Prelude.seq`
          Prelude.rnf uRLOperation

instance Data.ToJSON VisualCustomActionOperation where
  toJSON VisualCustomActionOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterOperation" Data..=)
              Prelude.<$> filterOperation,
            ("NavigationOperation" Data..=)
              Prelude.<$> navigationOperation,
            ("SetParametersOperation" Data..=)
              Prelude.<$> setParametersOperation,
            ("URLOperation" Data..=) Prelude.<$> uRLOperation
          ]
      )
