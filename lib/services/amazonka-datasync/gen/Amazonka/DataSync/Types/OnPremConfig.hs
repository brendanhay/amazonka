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
-- Module      : Amazonka.DataSync.Types.OnPremConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.OnPremConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of Amazon Resource Names (ARNs) of agents to use for a Network
-- File System (NFS) location.
--
-- /See:/ 'newOnPremConfig' smart constructor.
data OnPremConfig = OnPremConfig'
  { -- | ARNs of the agents to use for an NFS location.
    agentArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnPremConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'onPremConfig_agentArns' - ARNs of the agents to use for an NFS location.
newOnPremConfig ::
  -- | 'agentArns'
  Prelude.NonEmpty Prelude.Text ->
  OnPremConfig
newOnPremConfig pAgentArns_ =
  OnPremConfig'
    { agentArns =
        Lens.coerced Lens.# pAgentArns_
    }

-- | ARNs of the agents to use for an NFS location.
onPremConfig_agentArns :: Lens.Lens' OnPremConfig (Prelude.NonEmpty Prelude.Text)
onPremConfig_agentArns = Lens.lens (\OnPremConfig' {agentArns} -> agentArns) (\s@OnPremConfig' {} a -> s {agentArns = a} :: OnPremConfig) Prelude.. Lens.coerced

instance Core.FromJSON OnPremConfig where
  parseJSON =
    Core.withObject
      "OnPremConfig"
      ( \x ->
          OnPremConfig' Prelude.<$> (x Core..: "AgentArns")
      )

instance Prelude.Hashable OnPremConfig where
  hashWithSalt _salt OnPremConfig' {..} =
    _salt `Prelude.hashWithSalt` agentArns

instance Prelude.NFData OnPremConfig where
  rnf OnPremConfig' {..} = Prelude.rnf agentArns

instance Core.ToJSON OnPremConfig where
  toJSON OnPremConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AgentArns" Core..= agentArns)]
      )
