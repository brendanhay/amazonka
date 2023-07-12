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
-- Module      : Amazonka.SecurityHub.Types.PortProbeAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PortProbeAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.PortProbeDetail

-- | Provided if @ActionType@ is @PORT_PROBE@. It provides details about the
-- attempted port probe that was detected.
--
-- /See:/ 'newPortProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { -- | Indicates whether the port probe was blocked.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | Information about the ports affected by the port probe.
    portProbeDetails :: Prelude.Maybe [PortProbeDetail]
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
-- 'blocked', 'portProbeAction_blocked' - Indicates whether the port probe was blocked.
--
-- 'portProbeDetails', 'portProbeAction_portProbeDetails' - Information about the ports affected by the port probe.
newPortProbeAction ::
  PortProbeAction
newPortProbeAction =
  PortProbeAction'
    { blocked = Prelude.Nothing,
      portProbeDetails = Prelude.Nothing
    }

-- | Indicates whether the port probe was blocked.
portProbeAction_blocked :: Lens.Lens' PortProbeAction (Prelude.Maybe Prelude.Bool)
portProbeAction_blocked = Lens.lens (\PortProbeAction' {blocked} -> blocked) (\s@PortProbeAction' {} a -> s {blocked = a} :: PortProbeAction)

-- | Information about the ports affected by the port probe.
portProbeAction_portProbeDetails :: Lens.Lens' PortProbeAction (Prelude.Maybe [PortProbeDetail])
portProbeAction_portProbeDetails = Lens.lens (\PortProbeAction' {portProbeDetails} -> portProbeDetails) (\s@PortProbeAction' {} a -> s {portProbeDetails = a} :: PortProbeAction) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PortProbeAction where
  parseJSON =
    Data.withObject
      "PortProbeAction"
      ( \x ->
          PortProbeAction'
            Prelude.<$> (x Data..:? "Blocked")
            Prelude.<*> ( x
                            Data..:? "PortProbeDetails"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PortProbeAction where
  hashWithSalt _salt PortProbeAction' {..} =
    _salt
      `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` portProbeDetails

instance Prelude.NFData PortProbeAction where
  rnf PortProbeAction' {..} =
    Prelude.rnf blocked
      `Prelude.seq` Prelude.rnf portProbeDetails

instance Data.ToJSON PortProbeAction where
  toJSON PortProbeAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Blocked" Data..=) Prelude.<$> blocked,
            ("PortProbeDetails" Data..=)
              Prelude.<$> portProbeDetails
          ]
      )
