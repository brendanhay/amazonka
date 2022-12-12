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
-- Module      : Amazonka.QuickSight.Types.SimpleClusterMarker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SimpleClusterMarker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The simple cluster marker of the cluster marker.
--
-- /See:/ 'newSimpleClusterMarker' smart constructor.
data SimpleClusterMarker = SimpleClusterMarker'
  { -- | The color of the simple cluster marker.
    color :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleClusterMarker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'simpleClusterMarker_color' - The color of the simple cluster marker.
newSimpleClusterMarker ::
  SimpleClusterMarker
newSimpleClusterMarker =
  SimpleClusterMarker' {color = Prelude.Nothing}

-- | The color of the simple cluster marker.
simpleClusterMarker_color :: Lens.Lens' SimpleClusterMarker (Prelude.Maybe Prelude.Text)
simpleClusterMarker_color = Lens.lens (\SimpleClusterMarker' {color} -> color) (\s@SimpleClusterMarker' {} a -> s {color = a} :: SimpleClusterMarker)

instance Data.FromJSON SimpleClusterMarker where
  parseJSON =
    Data.withObject
      "SimpleClusterMarker"
      ( \x ->
          SimpleClusterMarker'
            Prelude.<$> (x Data..:? "Color")
      )

instance Prelude.Hashable SimpleClusterMarker where
  hashWithSalt _salt SimpleClusterMarker' {..} =
    _salt `Prelude.hashWithSalt` color

instance Prelude.NFData SimpleClusterMarker where
  rnf SimpleClusterMarker' {..} = Prelude.rnf color

instance Data.ToJSON SimpleClusterMarker where
  toJSON SimpleClusterMarker' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Color" Data..=) Prelude.<$> color]
      )
