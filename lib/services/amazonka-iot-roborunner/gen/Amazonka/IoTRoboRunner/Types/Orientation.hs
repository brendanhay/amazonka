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
-- Module      : Amazonka.IoTRoboRunner.Types.Orientation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.Orientation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Worker orientation measured in units clockwise from north.
--
-- /See:/ 'newOrientation' smart constructor.
data Orientation = Orientation'
  { -- | Degrees, limited on [0, 360)
    degrees :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Orientation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'degrees', 'orientation_degrees' - Degrees, limited on [0, 360)
newOrientation ::
  Orientation
newOrientation =
  Orientation' {degrees = Prelude.Nothing}

-- | Degrees, limited on [0, 360)
orientation_degrees :: Lens.Lens' Orientation (Prelude.Maybe Prelude.Double)
orientation_degrees = Lens.lens (\Orientation' {degrees} -> degrees) (\s@Orientation' {} a -> s {degrees = a} :: Orientation)

instance Core.FromJSON Orientation where
  parseJSON =
    Core.withObject
      "Orientation"
      ( \x ->
          Orientation' Prelude.<$> (x Core..:? "degrees")
      )

instance Prelude.Hashable Orientation where
  hashWithSalt _salt Orientation' {..} =
    _salt `Prelude.hashWithSalt` degrees

instance Prelude.NFData Orientation where
  rnf Orientation' {..} = Prelude.rnf degrees

instance Core.ToJSON Orientation where
  toJSON Orientation' {..} =
    Core.object
      ( Prelude.catMaybes
          [("degrees" Core..=) Prelude.<$> degrees]
      )
