{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.PortProbeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeAction where

import Network.AWS.GuardDuty.Types.PortProbeDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
portProbeAction_portProbeDetails = Lens.lens (\PortProbeAction' {portProbeDetails} -> portProbeDetails) (\s@PortProbeAction' {} a -> s {portProbeDetails = a} :: PortProbeAction) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether EC2 blocked the port probe to the instance, such as
-- with an ACL.
portProbeAction_blocked :: Lens.Lens' PortProbeAction (Prelude.Maybe Prelude.Bool)
portProbeAction_blocked = Lens.lens (\PortProbeAction' {blocked} -> blocked) (\s@PortProbeAction' {} a -> s {blocked = a} :: PortProbeAction)

instance Prelude.FromJSON PortProbeAction where
  parseJSON =
    Prelude.withObject
      "PortProbeAction"
      ( \x ->
          PortProbeAction'
            Prelude.<$> ( x Prelude..:? "portProbeDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "blocked")
      )

instance Prelude.Hashable PortProbeAction

instance Prelude.NFData PortProbeAction
