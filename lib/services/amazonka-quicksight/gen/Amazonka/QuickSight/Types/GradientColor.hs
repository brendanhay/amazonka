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
-- Module      : Amazonka.QuickSight.Types.GradientColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GradientColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GradientStop

-- | Determines the gradient color settings.
--
-- /See:/ 'newGradientColor' smart constructor.
data GradientColor = GradientColor'
  { -- | The list of gradient color stops.
    stops :: Prelude.Maybe [GradientStop]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GradientColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stops', 'gradientColor_stops' - The list of gradient color stops.
newGradientColor ::
  GradientColor
newGradientColor =
  GradientColor' {stops = Prelude.Nothing}

-- | The list of gradient color stops.
gradientColor_stops :: Lens.Lens' GradientColor (Prelude.Maybe [GradientStop])
gradientColor_stops = Lens.lens (\GradientColor' {stops} -> stops) (\s@GradientColor' {} a -> s {stops = a} :: GradientColor) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GradientColor where
  parseJSON =
    Data.withObject
      "GradientColor"
      ( \x ->
          GradientColor'
            Prelude.<$> (x Data..:? "Stops" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GradientColor where
  hashWithSalt _salt GradientColor' {..} =
    _salt `Prelude.hashWithSalt` stops

instance Prelude.NFData GradientColor where
  rnf GradientColor' {..} = Prelude.rnf stops

instance Data.ToJSON GradientColor where
  toJSON GradientColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Stops" Data..=) Prelude.<$> stops]
      )
