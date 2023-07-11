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
-- Module      : Amazonka.QuickSight.Types.BorderStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BorderStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The display options for tile borders for visuals.
--
-- /See:/ 'newBorderStyle' smart constructor.
data BorderStyle = BorderStyle'
  { -- | The option to enable display of borders for visuals.
    show :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BorderStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'show', 'borderStyle_show' - The option to enable display of borders for visuals.
newBorderStyle ::
  BorderStyle
newBorderStyle = BorderStyle' {show = Prelude.Nothing}

-- | The option to enable display of borders for visuals.
borderStyle_show :: Lens.Lens' BorderStyle (Prelude.Maybe Prelude.Bool)
borderStyle_show = Lens.lens (\BorderStyle' {show} -> show) (\s@BorderStyle' {} a -> s {show = a} :: BorderStyle)

instance Data.FromJSON BorderStyle where
  parseJSON =
    Data.withObject
      "BorderStyle"
      (\x -> BorderStyle' Prelude.<$> (x Data..:? "Show"))

instance Prelude.Hashable BorderStyle where
  hashWithSalt _salt BorderStyle' {..} =
    _salt `Prelude.hashWithSalt` show

instance Prelude.NFData BorderStyle where
  rnf BorderStyle' {..} = Prelude.rnf show

instance Data.ToJSON BorderStyle where
  toJSON BorderStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Show" Data..=) Prelude.<$> show]
      )
