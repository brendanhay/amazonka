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
-- Module      : Network.AWS.Inspector.Types.AgentFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentFilter where

import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
agentFilter_agentHealths = Lens.lens (\AgentFilter' {agentHealths} -> agentHealths) (\s@AgentFilter' {} a -> s {agentHealths = a} :: AgentFilter) Prelude.. Prelude._Coerce

-- | The detailed health state of the agent. Values can be set to __IDLE__,
-- __RUNNING__, __SHUTDOWN__, __UNHEALTHY__, __THROTTLED__, and
-- __UNKNOWN__.
agentFilter_agentHealthCodes :: Lens.Lens' AgentFilter [AgentHealthCode]
agentFilter_agentHealthCodes = Lens.lens (\AgentFilter' {agentHealthCodes} -> agentHealthCodes) (\s@AgentFilter' {} a -> s {agentHealthCodes = a} :: AgentFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable AgentFilter

instance Prelude.NFData AgentFilter

instance Prelude.ToJSON AgentFilter where
  toJSON AgentFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("agentHealths" Prelude..= agentHealths),
            Prelude.Just
              ("agentHealthCodes" Prelude..= agentHealthCodes)
          ]
      )
