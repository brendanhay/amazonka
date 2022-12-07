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
-- Module      : Amazonka.CodeDeploy.Types.TimeBasedLinear
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TimeBasedLinear where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration that shifts traffic from one version of a Lambda
-- function or ECS task set to another in equal increments, with an equal
-- number of minutes between each increment. The original and target Lambda
-- function versions or ECS task sets are specified in the deployment\'s
-- AppSpec file.
--
-- /See:/ 'newTimeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { -- | The number of minutes between each incremental traffic shift of a
    -- @TimeBasedLinear@ deployment.
    linearInterval :: Prelude.Maybe Prelude.Int,
    -- | The percentage of traffic that is shifted at the start of each increment
    -- of a @TimeBasedLinear@ deployment.
    linearPercentage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeBasedLinear' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linearInterval', 'timeBasedLinear_linearInterval' - The number of minutes between each incremental traffic shift of a
-- @TimeBasedLinear@ deployment.
--
-- 'linearPercentage', 'timeBasedLinear_linearPercentage' - The percentage of traffic that is shifted at the start of each increment
-- of a @TimeBasedLinear@ deployment.
newTimeBasedLinear ::
  TimeBasedLinear
newTimeBasedLinear =
  TimeBasedLinear'
    { linearInterval = Prelude.Nothing,
      linearPercentage = Prelude.Nothing
    }

-- | The number of minutes between each incremental traffic shift of a
-- @TimeBasedLinear@ deployment.
timeBasedLinear_linearInterval :: Lens.Lens' TimeBasedLinear (Prelude.Maybe Prelude.Int)
timeBasedLinear_linearInterval = Lens.lens (\TimeBasedLinear' {linearInterval} -> linearInterval) (\s@TimeBasedLinear' {} a -> s {linearInterval = a} :: TimeBasedLinear)

-- | The percentage of traffic that is shifted at the start of each increment
-- of a @TimeBasedLinear@ deployment.
timeBasedLinear_linearPercentage :: Lens.Lens' TimeBasedLinear (Prelude.Maybe Prelude.Int)
timeBasedLinear_linearPercentage = Lens.lens (\TimeBasedLinear' {linearPercentage} -> linearPercentage) (\s@TimeBasedLinear' {} a -> s {linearPercentage = a} :: TimeBasedLinear)

instance Data.FromJSON TimeBasedLinear where
  parseJSON =
    Data.withObject
      "TimeBasedLinear"
      ( \x ->
          TimeBasedLinear'
            Prelude.<$> (x Data..:? "linearInterval")
            Prelude.<*> (x Data..:? "linearPercentage")
      )

instance Prelude.Hashable TimeBasedLinear where
  hashWithSalt _salt TimeBasedLinear' {..} =
    _salt `Prelude.hashWithSalt` linearInterval
      `Prelude.hashWithSalt` linearPercentage

instance Prelude.NFData TimeBasedLinear where
  rnf TimeBasedLinear' {..} =
    Prelude.rnf linearInterval
      `Prelude.seq` Prelude.rnf linearPercentage

instance Data.ToJSON TimeBasedLinear where
  toJSON TimeBasedLinear' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("linearInterval" Data..=)
              Prelude.<$> linearInterval,
            ("linearPercentage" Data..=)
              Prelude.<$> linearPercentage
          ]
      )
