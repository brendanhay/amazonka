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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ScalableTargetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ScalableTargetAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the minimum and maximum capacity for a scheduled action.
--
-- /See:/ 'newScalableTargetAction' smart constructor.
data ScalableTargetAction = ScalableTargetAction'
  { -- | The maximum capacity.
    --
    -- Although you can specify a large maximum capacity, note that service
    -- quotas may impose lower limits. Each service has its own default quotas
    -- for the maximum capacity of the resource. If you want to specify a
    -- higher limit, you can request an increase. For more information, consult
    -- the documentation for that service. For information about the default
    -- quotas for each service, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service endpoints and quotas>
    -- in the /Amazon Web Services General Reference/.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The minimum capacity.
    --
    -- When the scheduled action runs, the resource will have at least this
    -- much capacity, but it might have more depending on other settings, such
    -- as the target utilization level of a target tracking scaling policy.
    minCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalableTargetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'scalableTargetAction_maxCapacity' - The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service
-- quotas may impose lower limits. Each service has its own default quotas
-- for the maximum capacity of the resource. If you want to specify a
-- higher limit, you can request an increase. For more information, consult
-- the documentation for that service. For information about the default
-- quotas for each service, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- 'minCapacity', 'scalableTargetAction_minCapacity' - The minimum capacity.
--
-- When the scheduled action runs, the resource will have at least this
-- much capacity, but it might have more depending on other settings, such
-- as the target utilization level of a target tracking scaling policy.
newScalableTargetAction ::
  ScalableTargetAction
newScalableTargetAction =
  ScalableTargetAction'
    { maxCapacity =
        Prelude.Nothing,
      minCapacity = Prelude.Nothing
    }

-- | The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service
-- quotas may impose lower limits. Each service has its own default quotas
-- for the maximum capacity of the resource. If you want to specify a
-- higher limit, you can request an increase. For more information, consult
-- the documentation for that service. For information about the default
-- quotas for each service, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
scalableTargetAction_maxCapacity :: Lens.Lens' ScalableTargetAction (Prelude.Maybe Prelude.Int)
scalableTargetAction_maxCapacity = Lens.lens (\ScalableTargetAction' {maxCapacity} -> maxCapacity) (\s@ScalableTargetAction' {} a -> s {maxCapacity = a} :: ScalableTargetAction)

-- | The minimum capacity.
--
-- When the scheduled action runs, the resource will have at least this
-- much capacity, but it might have more depending on other settings, such
-- as the target utilization level of a target tracking scaling policy.
scalableTargetAction_minCapacity :: Lens.Lens' ScalableTargetAction (Prelude.Maybe Prelude.Int)
scalableTargetAction_minCapacity = Lens.lens (\ScalableTargetAction' {minCapacity} -> minCapacity) (\s@ScalableTargetAction' {} a -> s {minCapacity = a} :: ScalableTargetAction)

instance Data.FromJSON ScalableTargetAction where
  parseJSON =
    Data.withObject
      "ScalableTargetAction"
      ( \x ->
          ScalableTargetAction'
            Prelude.<$> (x Data..:? "MaxCapacity")
            Prelude.<*> (x Data..:? "MinCapacity")
      )

instance Prelude.Hashable ScalableTargetAction where
  hashWithSalt _salt ScalableTargetAction' {..} =
    _salt
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` minCapacity

instance Prelude.NFData ScalableTargetAction where
  rnf ScalableTargetAction' {..} =
    Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf minCapacity

instance Data.ToJSON ScalableTargetAction where
  toJSON ScalableTargetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("MinCapacity" Data..=) Prelude.<$> minCapacity
          ]
      )
