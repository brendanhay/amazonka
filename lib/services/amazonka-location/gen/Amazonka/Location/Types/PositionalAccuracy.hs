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
-- Module      : Amazonka.Location.Types.PositionalAccuracy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.PositionalAccuracy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the level of certainty of the position.
--
-- /See:/ 'newPositionalAccuracy' smart constructor.
data PositionalAccuracy = PositionalAccuracy'
  { -- | Estimated maximum distance, in meters, between the measured position and
    -- the true position of a device, along the Earth\'s surface.
    horizontal :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PositionalAccuracy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'horizontal', 'positionalAccuracy_horizontal' - Estimated maximum distance, in meters, between the measured position and
-- the true position of a device, along the Earth\'s surface.
newPositionalAccuracy ::
  -- | 'horizontal'
  Prelude.Double ->
  PositionalAccuracy
newPositionalAccuracy pHorizontal_ =
  PositionalAccuracy' {horizontal = pHorizontal_}

-- | Estimated maximum distance, in meters, between the measured position and
-- the true position of a device, along the Earth\'s surface.
positionalAccuracy_horizontal :: Lens.Lens' PositionalAccuracy Prelude.Double
positionalAccuracy_horizontal = Lens.lens (\PositionalAccuracy' {horizontal} -> horizontal) (\s@PositionalAccuracy' {} a -> s {horizontal = a} :: PositionalAccuracy)

instance Data.FromJSON PositionalAccuracy where
  parseJSON =
    Data.withObject
      "PositionalAccuracy"
      ( \x ->
          PositionalAccuracy'
            Prelude.<$> (x Data..: "Horizontal")
      )

instance Prelude.Hashable PositionalAccuracy where
  hashWithSalt _salt PositionalAccuracy' {..} =
    _salt `Prelude.hashWithSalt` horizontal

instance Prelude.NFData PositionalAccuracy where
  rnf PositionalAccuracy' {..} = Prelude.rnf horizontal

instance Data.ToJSON PositionalAccuracy where
  toJSON PositionalAccuracy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Horizontal" Data..= horizontal)]
      )
