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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.TunnelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types.TunnelStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the tunnel.
--
-- /See:/ 'newTunnelSummary' smart constructor.
data TunnelSummary = TunnelSummary'
  { -- | The time the tunnel was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | A description of the tunnel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time the tunnel was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The status of a tunnel. Valid values are: Open and Closed.
    status :: Prelude.Maybe TunnelStatus,
    -- | The Amazon Resource Name of the tunnel.
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | The unique alpha-numeric identifier for the tunnel.
    tunnelId :: Prelude.Maybe Prelude.Text
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
-- 'createdAt', 'tunnelSummary_createdAt' - The time the tunnel was created.
--
-- 'description', 'tunnelSummary_description' - A description of the tunnel.
--
-- 'lastUpdatedAt', 'tunnelSummary_lastUpdatedAt' - The time the tunnel was last updated.
--
-- 'status', 'tunnelSummary_status' - The status of a tunnel. Valid values are: Open and Closed.
--
-- 'tunnelArn', 'tunnelSummary_tunnelArn' - The Amazon Resource Name of the tunnel.
--
-- 'tunnelId', 'tunnelSummary_tunnelId' - The unique alpha-numeric identifier for the tunnel.
newTunnelSummary ::
  TunnelSummary
newTunnelSummary =
  TunnelSummary'
    { createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      tunnelId = Prelude.Nothing
    }

-- | The time the tunnel was created.
tunnelSummary_createdAt :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.UTCTime)
tunnelSummary_createdAt = Lens.lens (\TunnelSummary' {createdAt} -> createdAt) (\s@TunnelSummary' {} a -> s {createdAt = a} :: TunnelSummary) Prelude.. Lens.mapping Data._Time

-- | A description of the tunnel.
tunnelSummary_description :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_description = Lens.lens (\TunnelSummary' {description} -> description) (\s@TunnelSummary' {} a -> s {description = a} :: TunnelSummary)

-- | The time the tunnel was last updated.
tunnelSummary_lastUpdatedAt :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.UTCTime)
tunnelSummary_lastUpdatedAt = Lens.lens (\TunnelSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@TunnelSummary' {} a -> s {lastUpdatedAt = a} :: TunnelSummary) Prelude.. Lens.mapping Data._Time

-- | The status of a tunnel. Valid values are: Open and Closed.
tunnelSummary_status :: Lens.Lens' TunnelSummary (Prelude.Maybe TunnelStatus)
tunnelSummary_status = Lens.lens (\TunnelSummary' {status} -> status) (\s@TunnelSummary' {} a -> s {status = a} :: TunnelSummary)

-- | The Amazon Resource Name of the tunnel.
tunnelSummary_tunnelArn :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_tunnelArn = Lens.lens (\TunnelSummary' {tunnelArn} -> tunnelArn) (\s@TunnelSummary' {} a -> s {tunnelArn = a} :: TunnelSummary)

-- | The unique alpha-numeric identifier for the tunnel.
tunnelSummary_tunnelId :: Lens.Lens' TunnelSummary (Prelude.Maybe Prelude.Text)
tunnelSummary_tunnelId = Lens.lens (\TunnelSummary' {tunnelId} -> tunnelId) (\s@TunnelSummary' {} a -> s {tunnelId = a} :: TunnelSummary)

instance Data.FromJSON TunnelSummary where
  parseJSON =
    Data.withObject
      "TunnelSummary"
      ( \x ->
          TunnelSummary'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tunnelArn")
            Prelude.<*> (x Data..:? "tunnelId")
      )

instance Prelude.Hashable TunnelSummary where
  hashWithSalt _salt TunnelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tunnelArn
      `Prelude.hashWithSalt` tunnelId

instance Prelude.NFData TunnelSummary where
  rnf TunnelSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tunnelArn
      `Prelude.seq` Prelude.rnf tunnelId
