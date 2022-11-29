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
-- Module      : Amazonka.EMR.Types.AutoTerminationPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoTerminationPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An auto-termination policy for an Amazon EMR cluster. An
-- auto-termination policy defines the amount of idle time in seconds after
-- which a cluster automatically terminates. For alternative cluster
-- termination options, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-termination.html Control cluster termination>.
--
-- /See:/ 'newAutoTerminationPolicy' smart constructor.
data AutoTerminationPolicy = AutoTerminationPolicy'
  { -- | Specifies the amount of idle time in seconds after which the cluster
    -- automatically terminates. You can specify a minimum of 60 seconds and a
    -- maximum of 604800 seconds (seven days).
    idleTimeout :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTerminationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idleTimeout', 'autoTerminationPolicy_idleTimeout' - Specifies the amount of idle time in seconds after which the cluster
-- automatically terminates. You can specify a minimum of 60 seconds and a
-- maximum of 604800 seconds (seven days).
newAutoTerminationPolicy ::
  AutoTerminationPolicy
newAutoTerminationPolicy =
  AutoTerminationPolicy'
    { idleTimeout =
        Prelude.Nothing
    }

-- | Specifies the amount of idle time in seconds after which the cluster
-- automatically terminates. You can specify a minimum of 60 seconds and a
-- maximum of 604800 seconds (seven days).
autoTerminationPolicy_idleTimeout :: Lens.Lens' AutoTerminationPolicy (Prelude.Maybe Prelude.Integer)
autoTerminationPolicy_idleTimeout = Lens.lens (\AutoTerminationPolicy' {idleTimeout} -> idleTimeout) (\s@AutoTerminationPolicy' {} a -> s {idleTimeout = a} :: AutoTerminationPolicy)

instance Core.FromJSON AutoTerminationPolicy where
  parseJSON =
    Core.withObject
      "AutoTerminationPolicy"
      ( \x ->
          AutoTerminationPolicy'
            Prelude.<$> (x Core..:? "IdleTimeout")
      )

instance Prelude.Hashable AutoTerminationPolicy where
  hashWithSalt _salt AutoTerminationPolicy' {..} =
    _salt `Prelude.hashWithSalt` idleTimeout

instance Prelude.NFData AutoTerminationPolicy where
  rnf AutoTerminationPolicy' {..} =
    Prelude.rnf idleTimeout

instance Core.ToJSON AutoTerminationPolicy where
  toJSON AutoTerminationPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IdleTimeout" Core..=) Prelude.<$> idleTimeout]
      )
