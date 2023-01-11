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
-- Module      : Amazonka.QuickSight.Types.DateAxisOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateAxisOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine how a date axis is displayed.
--
-- /See:/ 'newDateAxisOptions' smart constructor.
data DateAxisOptions = DateAxisOptions'
  { -- | Determines whether or not missing dates are displayed.
    missingDateVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateAxisOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missingDateVisibility', 'dateAxisOptions_missingDateVisibility' - Determines whether or not missing dates are displayed.
newDateAxisOptions ::
  DateAxisOptions
newDateAxisOptions =
  DateAxisOptions'
    { missingDateVisibility =
        Prelude.Nothing
    }

-- | Determines whether or not missing dates are displayed.
dateAxisOptions_missingDateVisibility :: Lens.Lens' DateAxisOptions (Prelude.Maybe Visibility)
dateAxisOptions_missingDateVisibility = Lens.lens (\DateAxisOptions' {missingDateVisibility} -> missingDateVisibility) (\s@DateAxisOptions' {} a -> s {missingDateVisibility = a} :: DateAxisOptions)

instance Data.FromJSON DateAxisOptions where
  parseJSON =
    Data.withObject
      "DateAxisOptions"
      ( \x ->
          DateAxisOptions'
            Prelude.<$> (x Data..:? "MissingDateVisibility")
      )

instance Prelude.Hashable DateAxisOptions where
  hashWithSalt _salt DateAxisOptions' {..} =
    _salt `Prelude.hashWithSalt` missingDateVisibility

instance Prelude.NFData DateAxisOptions where
  rnf DateAxisOptions' {..} =
    Prelude.rnf missingDateVisibility

instance Data.ToJSON DateAxisOptions where
  toJSON DateAxisOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MissingDateVisibility" Data..=)
              Prelude.<$> missingDateVisibility
          ]
      )
