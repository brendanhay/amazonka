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
-- Module      : Amazonka.QuickSight.Types.SecondaryValueOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SecondaryValueOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the presentation of the secondary value of a
-- KPI visual.
--
-- /See:/ 'newSecondaryValueOptions' smart constructor.
data SecondaryValueOptions = SecondaryValueOptions'
  { -- | Determines the visibility of the secondary value.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecondaryValueOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'secondaryValueOptions_visibility' - Determines the visibility of the secondary value.
newSecondaryValueOptions ::
  SecondaryValueOptions
newSecondaryValueOptions =
  SecondaryValueOptions'
    { visibility =
        Prelude.Nothing
    }

-- | Determines the visibility of the secondary value.
secondaryValueOptions_visibility :: Lens.Lens' SecondaryValueOptions (Prelude.Maybe Visibility)
secondaryValueOptions_visibility = Lens.lens (\SecondaryValueOptions' {visibility} -> visibility) (\s@SecondaryValueOptions' {} a -> s {visibility = a} :: SecondaryValueOptions)

instance Data.FromJSON SecondaryValueOptions where
  parseJSON =
    Data.withObject
      "SecondaryValueOptions"
      ( \x ->
          SecondaryValueOptions'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable SecondaryValueOptions where
  hashWithSalt _salt SecondaryValueOptions' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData SecondaryValueOptions where
  rnf SecondaryValueOptions' {..} =
    Prelude.rnf visibility

instance Data.ToJSON SecondaryValueOptions where
  toJSON SecondaryValueOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
