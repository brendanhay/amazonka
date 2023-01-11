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
-- Module      : Amazonka.EC2.Types.InstanceMaintenanceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMaintenanceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceAutoRecoveryState
import qualified Amazonka.Prelude as Prelude

-- | The maintenance options for the instance.
--
-- /See:/ 'newInstanceMaintenanceOptions' smart constructor.
data InstanceMaintenanceOptions = InstanceMaintenanceOptions'
  { -- | Provides information on the current automatic recovery behavior of your
    -- instance.
    autoRecovery :: Prelude.Maybe InstanceAutoRecoveryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMaintenanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'instanceMaintenanceOptions_autoRecovery' - Provides information on the current automatic recovery behavior of your
-- instance.
newInstanceMaintenanceOptions ::
  InstanceMaintenanceOptions
newInstanceMaintenanceOptions =
  InstanceMaintenanceOptions'
    { autoRecovery =
        Prelude.Nothing
    }

-- | Provides information on the current automatic recovery behavior of your
-- instance.
instanceMaintenanceOptions_autoRecovery :: Lens.Lens' InstanceMaintenanceOptions (Prelude.Maybe InstanceAutoRecoveryState)
instanceMaintenanceOptions_autoRecovery = Lens.lens (\InstanceMaintenanceOptions' {autoRecovery} -> autoRecovery) (\s@InstanceMaintenanceOptions' {} a -> s {autoRecovery = a} :: InstanceMaintenanceOptions)

instance Data.FromXML InstanceMaintenanceOptions where
  parseXML x =
    InstanceMaintenanceOptions'
      Prelude.<$> (x Data..@? "autoRecovery")

instance Prelude.Hashable InstanceMaintenanceOptions where
  hashWithSalt _salt InstanceMaintenanceOptions' {..} =
    _salt `Prelude.hashWithSalt` autoRecovery

instance Prelude.NFData InstanceMaintenanceOptions where
  rnf InstanceMaintenanceOptions' {..} =
    Prelude.rnf autoRecovery
