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
-- Module      : Amazonka.QuickSight.Types.NegativeValueConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NegativeValueConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NegativeValueDisplayMode

-- | The options that determine the negative value configuration.
--
-- /See:/ 'newNegativeValueConfiguration' smart constructor.
data NegativeValueConfiguration = NegativeValueConfiguration'
  { -- | Determines the display mode of the negative value configuration.
    displayMode :: NegativeValueDisplayMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NegativeValueConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayMode', 'negativeValueConfiguration_displayMode' - Determines the display mode of the negative value configuration.
newNegativeValueConfiguration ::
  -- | 'displayMode'
  NegativeValueDisplayMode ->
  NegativeValueConfiguration
newNegativeValueConfiguration pDisplayMode_ =
  NegativeValueConfiguration'
    { displayMode =
        pDisplayMode_
    }

-- | Determines the display mode of the negative value configuration.
negativeValueConfiguration_displayMode :: Lens.Lens' NegativeValueConfiguration NegativeValueDisplayMode
negativeValueConfiguration_displayMode = Lens.lens (\NegativeValueConfiguration' {displayMode} -> displayMode) (\s@NegativeValueConfiguration' {} a -> s {displayMode = a} :: NegativeValueConfiguration)

instance Data.FromJSON NegativeValueConfiguration where
  parseJSON =
    Data.withObject
      "NegativeValueConfiguration"
      ( \x ->
          NegativeValueConfiguration'
            Prelude.<$> (x Data..: "DisplayMode")
      )

instance Prelude.Hashable NegativeValueConfiguration where
  hashWithSalt _salt NegativeValueConfiguration' {..} =
    _salt `Prelude.hashWithSalt` displayMode

instance Prelude.NFData NegativeValueConfiguration where
  rnf NegativeValueConfiguration' {..} =
    Prelude.rnf displayMode

instance Data.ToJSON NegativeValueConfiguration where
  toJSON NegativeValueConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DisplayMode" Data..= displayMode)]
      )
