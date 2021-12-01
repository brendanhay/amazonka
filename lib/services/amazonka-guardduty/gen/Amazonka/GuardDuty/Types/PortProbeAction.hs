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
-- Module      : Amazonka.GuardDuty.Types.PortProbeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PortProbeAction where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.PortProbeDetail
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the PORT_PROBE action described in the
-- finding.
--
-- /See:/ 'newPortProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { -- | A list of objects related to port probe details.
    portProbeDetails :: Prelude.Maybe [PortProbeDetail],
    -- | Indicates whether EC2 blocked the port probe to the instance, such as
    -- with an ACL.
    blocked :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortProbeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portProbeDetails', 'portProbeAction_portProbeDetails' - A list of objects related to port probe details.
--
-- 'blocked', 'portProbeAction_blocked' - Indicates whether EC2 blocked the port probe to the instance, such as
-- with an ACL.
newPortProbeAction ::
  PortProbeAction
newPortProbeAction =
  PortProbeAction'
    { portProbeDetails =
        Prelude.Nothing,
      blocked = Prelude.Nothing
    }

-- | A list of objects related to port probe details.
portProbeAction_portProbeDetails :: Lens.Lens' PortProbeAction (Prelude.Maybe [PortProbeDetail])
portProbeAction_portProbeDetails = Lens.lens (\PortProbeAction' {portProbeDetails} -> portProbeDetails) (\s@PortProbeAction' {} a -> s {portProbeDetails = a} :: PortProbeAction) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether EC2 blocked the port probe to the instance, such as
-- with an ACL.
portProbeAction_blocked :: Lens.Lens' PortProbeAction (Prelude.Maybe Prelude.Bool)
portProbeAction_blocked = Lens.lens (\PortProbeAction' {blocked} -> blocked) (\s@PortProbeAction' {} a -> s {blocked = a} :: PortProbeAction)

instance Core.FromJSON PortProbeAction where
  parseJSON =
    Core.withObject
      "PortProbeAction"
      ( \x ->
          PortProbeAction'
            Prelude.<$> ( x Core..:? "portProbeDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "blocked")
      )

instance Prelude.Hashable PortProbeAction where
  hashWithSalt salt' PortProbeAction' {..} =
    salt' `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` portProbeDetails

instance Prelude.NFData PortProbeAction where
  rnf PortProbeAction' {..} =
    Prelude.rnf portProbeDetails
      `Prelude.seq` Prelude.rnf blocked
