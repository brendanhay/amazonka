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
-- Module      : Amazonka.Inspector.Types.AgentFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AgentFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.AgentHealth
import Amazonka.Inspector.Types.AgentHealthCode
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Inspector agent. This data type is
-- used as a request parameter in the ListAssessmentRunAgents action.
--
-- /See:/ 'newAgentFilter' smart constructor.
data AgentFilter = AgentFilter'
  { -- | The current health state of the agent. Values can be set to __HEALTHY__
    -- or __UNHEALTHY__.
    agentHealths :: [AgentHealth],
    -- | The detailed health state of the agent. Values can be set to __IDLE__,
    -- __RUNNING__, __SHUTDOWN__, __UNHEALTHY__, __THROTTLED__, and
    -- __UNKNOWN__.
    agentHealthCodes :: [AgentHealthCode]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentHealths', 'agentFilter_agentHealths' - The current health state of the agent. Values can be set to __HEALTHY__
-- or __UNHEALTHY__.
--
-- 'agentHealthCodes', 'agentFilter_agentHealthCodes' - The detailed health state of the agent. Values can be set to __IDLE__,
-- __RUNNING__, __SHUTDOWN__, __UNHEALTHY__, __THROTTLED__, and
-- __UNKNOWN__.
newAgentFilter ::
  AgentFilter
newAgentFilter =
  AgentFilter'
    { agentHealths = Prelude.mempty,
      agentHealthCodes = Prelude.mempty
    }

-- | The current health state of the agent. Values can be set to __HEALTHY__
-- or __UNHEALTHY__.
agentFilter_agentHealths :: Lens.Lens' AgentFilter [AgentHealth]
agentFilter_agentHealths = Lens.lens (\AgentFilter' {agentHealths} -> agentHealths) (\s@AgentFilter' {} a -> s {agentHealths = a} :: AgentFilter) Prelude.. Lens.coerced

-- | The detailed health state of the agent. Values can be set to __IDLE__,
-- __RUNNING__, __SHUTDOWN__, __UNHEALTHY__, __THROTTLED__, and
-- __UNKNOWN__.
agentFilter_agentHealthCodes :: Lens.Lens' AgentFilter [AgentHealthCode]
agentFilter_agentHealthCodes = Lens.lens (\AgentFilter' {agentHealthCodes} -> agentHealthCodes) (\s@AgentFilter' {} a -> s {agentHealthCodes = a} :: AgentFilter) Prelude.. Lens.coerced

instance Prelude.Hashable AgentFilter where
  hashWithSalt _salt AgentFilter' {..} =
    _salt
      `Prelude.hashWithSalt` agentHealths
      `Prelude.hashWithSalt` agentHealthCodes

instance Prelude.NFData AgentFilter where
  rnf AgentFilter' {..} =
    Prelude.rnf agentHealths `Prelude.seq`
      Prelude.rnf agentHealthCodes

instance Data.ToJSON AgentFilter where
  toJSON AgentFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("agentHealths" Data..= agentHealths),
            Prelude.Just
              ("agentHealthCodes" Data..= agentHealthCodes)
          ]
      )
