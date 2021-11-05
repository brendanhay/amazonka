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
-- Module      : Amazonka.IoTSecureTunneling.Types.TunnelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.TunnelSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTSecureTunneling.Types.TunnelStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the tunnel.
--
-- /See:/ 'newTunnelSummary' smart constructor.
data TunnelSummary = TunnelSummary'
  { -- | The status of a tunnel. Valid values are: Open and Closed.
    status :: Prelude.Maybe TunnelStatus,
    -- | The time the tunnel was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The time the tunnel was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name of the tunnel. The tunnel ARN format is
    -- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | The unique alpha-numeric identifier for the tunnel.
    tunnelId :: Prelude.Maybe Prelude.Text,
    -- | A description of the tunnel.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TunnelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'tunnelSummary_status' - The status of a tunnel. Valid values are: Open and Closed.
--
-- 'lastUpdatedAt', 'tunnelSummary_lastUpdatedAt' - The time the tunnel was last updated.
--
-- 'createdAt', 'tunnelSummary_createdAt' - The time the tunnel was created.
--
-- 'tunnelArn', 'tunnelSummary_tunnelArn' - The Amazon Resource Name of the tunnel. The tunnel ARN format is
-- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
--
-- 'tunnelId', 'tunnelSummary_tunnelId' - The unique alpha-numeric identifier for the tunnel.
--
-- 'description', 'tunnelSummary_description' - A description of the tunnel.
newTunnelSummary ::
  TunnelSummary
newTunnelSummary =
  TunnelSummary'
    { status = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      tunnelId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of a tunnel. Valid values are: Open and Closed.
tunnelSummary_status :: Lens.Lens' TunnelSummary (Prelude.Maybe TunnelStatus)
tunnelSummary_status = Lens.lens (\TunnelSummary' {status} -> status) (\s@TunnelSummary' {} a -> s {status = a} :: TunnelSummary)

-- | The time the tunnel was last updated.
tunnelSummary_lastUpdatedAt :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.UTCTime)
tunnelSummary_lastUpdatedAt = Lens.lens (\TunnelSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@TunnelSummary' {} a -> s {lastUpdatedAt = a} :: TunnelSummary) Prelude.. Lens.mapping Core._Time

-- | The time the tunnel was created.
tunnelSummary_createdAt :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.UTCTime)
tunnelSummary_createdAt = Lens.lens (\TunnelSummary' {createdAt} -> createdAt) (\s@TunnelSummary' {} a -> s {createdAt = a} :: TunnelSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name of the tunnel. The tunnel ARN format is
-- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
tunnelSummary_tunnelArn :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_tunnelArn = Lens.lens (\TunnelSummary' {tunnelArn} -> tunnelArn) (\s@TunnelSummary' {} a -> s {tunnelArn = a} :: TunnelSummary)

-- | The unique alpha-numeric identifier for the tunnel.
tunnelSummary_tunnelId :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_tunnelId = Lens.lens (\TunnelSummary' {tunnelId} -> tunnelId) (\s@TunnelSummary' {} a -> s {tunnelId = a} :: TunnelSummary)

-- | A description of the tunnel.
tunnelSummary_description :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_description = Lens.lens (\TunnelSummary' {description} -> description) (\s@TunnelSummary' {} a -> s {description = a} :: TunnelSummary)

instance Core.FromJSON TunnelSummary where
  parseJSON =
    Core.withObject
      "TunnelSummary"
      ( \x ->
          TunnelSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "tunnelArn")
            Prelude.<*> (x Core..:? "tunnelId")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable TunnelSummary

instance Prelude.NFData TunnelSummary
