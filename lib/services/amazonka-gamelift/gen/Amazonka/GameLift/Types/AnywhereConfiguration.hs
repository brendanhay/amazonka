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
-- Module      : Amazonka.GameLift.Types.AnywhereConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.AnywhereConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | GameLift Anywhere configuration options for your Anywhere fleets.
--
-- /See:/ 'newAnywhereConfiguration' smart constructor.
data AnywhereConfiguration = AnywhereConfiguration'
  { -- | The cost to run your fleet per hour. GameLift uses the provided cost of
    -- your fleet to balance usage in queues. For more information about
    -- queues, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Setting up queues>.
    cost :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnywhereConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cost', 'anywhereConfiguration_cost' - The cost to run your fleet per hour. GameLift uses the provided cost of
-- your fleet to balance usage in queues. For more information about
-- queues, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Setting up queues>.
newAnywhereConfiguration ::
  -- | 'cost'
  Prelude.Text ->
  AnywhereConfiguration
newAnywhereConfiguration pCost_ =
  AnywhereConfiguration' {cost = pCost_}

-- | The cost to run your fleet per hour. GameLift uses the provided cost of
-- your fleet to balance usage in queues. For more information about
-- queues, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Setting up queues>.
anywhereConfiguration_cost :: Lens.Lens' AnywhereConfiguration Prelude.Text
anywhereConfiguration_cost = Lens.lens (\AnywhereConfiguration' {cost} -> cost) (\s@AnywhereConfiguration' {} a -> s {cost = a} :: AnywhereConfiguration)

instance Data.FromJSON AnywhereConfiguration where
  parseJSON =
    Data.withObject
      "AnywhereConfiguration"
      ( \x ->
          AnywhereConfiguration'
            Prelude.<$> (x Data..: "Cost")
      )

instance Prelude.Hashable AnywhereConfiguration where
  hashWithSalt _salt AnywhereConfiguration' {..} =
    _salt `Prelude.hashWithSalt` cost

instance Prelude.NFData AnywhereConfiguration where
  rnf AnywhereConfiguration' {..} = Prelude.rnf cost

instance Data.ToJSON AnywhereConfiguration where
  toJSON AnywhereConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Cost" Data..= cost)]
      )
