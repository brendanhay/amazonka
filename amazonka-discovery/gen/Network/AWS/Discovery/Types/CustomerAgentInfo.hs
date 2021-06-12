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
-- Module      : Network.AWS.Discovery.Types.CustomerAgentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerAgentInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Inventory data for installed discovery agents.
--
-- /See:/ 'newCustomerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { -- | Number of active discovery agents.
    activeAgents :: Core.Int,
    -- | Number of healthy discovery agents
    healthyAgents :: Core.Int,
    -- | Number of blacklisted discovery agents.
    blackListedAgents :: Core.Int,
    -- | Number of discovery agents with status SHUTDOWN.
    shutdownAgents :: Core.Int,
    -- | Number of unhealthy discovery agents.
    unhealthyAgents :: Core.Int,
    -- | Total number of discovery agents.
    totalAgents :: Core.Int,
    -- | Number of unknown discovery agents.
    unknownAgents :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomerAgentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeAgents', 'customerAgentInfo_activeAgents' - Number of active discovery agents.
--
-- 'healthyAgents', 'customerAgentInfo_healthyAgents' - Number of healthy discovery agents
--
-- 'blackListedAgents', 'customerAgentInfo_blackListedAgents' - Number of blacklisted discovery agents.
--
-- 'shutdownAgents', 'customerAgentInfo_shutdownAgents' - Number of discovery agents with status SHUTDOWN.
--
-- 'unhealthyAgents', 'customerAgentInfo_unhealthyAgents' - Number of unhealthy discovery agents.
--
-- 'totalAgents', 'customerAgentInfo_totalAgents' - Total number of discovery agents.
--
-- 'unknownAgents', 'customerAgentInfo_unknownAgents' - Number of unknown discovery agents.
newCustomerAgentInfo ::
  -- | 'activeAgents'
  Core.Int ->
  -- | 'healthyAgents'
  Core.Int ->
  -- | 'blackListedAgents'
  Core.Int ->
  -- | 'shutdownAgents'
  Core.Int ->
  -- | 'unhealthyAgents'
  Core.Int ->
  -- | 'totalAgents'
  Core.Int ->
  -- | 'unknownAgents'
  Core.Int ->
  CustomerAgentInfo
newCustomerAgentInfo
  pActiveAgents_
  pHealthyAgents_
  pBlackListedAgents_
  pShutdownAgents_
  pUnhealthyAgents_
  pTotalAgents_
  pUnknownAgents_ =
    CustomerAgentInfo'
      { activeAgents = pActiveAgents_,
        healthyAgents = pHealthyAgents_,
        blackListedAgents = pBlackListedAgents_,
        shutdownAgents = pShutdownAgents_,
        unhealthyAgents = pUnhealthyAgents_,
        totalAgents = pTotalAgents_,
        unknownAgents = pUnknownAgents_
      }

-- | Number of active discovery agents.
customerAgentInfo_activeAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_activeAgents = Lens.lens (\CustomerAgentInfo' {activeAgents} -> activeAgents) (\s@CustomerAgentInfo' {} a -> s {activeAgents = a} :: CustomerAgentInfo)

-- | Number of healthy discovery agents
customerAgentInfo_healthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_healthyAgents = Lens.lens (\CustomerAgentInfo' {healthyAgents} -> healthyAgents) (\s@CustomerAgentInfo' {} a -> s {healthyAgents = a} :: CustomerAgentInfo)

-- | Number of blacklisted discovery agents.
customerAgentInfo_blackListedAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_blackListedAgents = Lens.lens (\CustomerAgentInfo' {blackListedAgents} -> blackListedAgents) (\s@CustomerAgentInfo' {} a -> s {blackListedAgents = a} :: CustomerAgentInfo)

-- | Number of discovery agents with status SHUTDOWN.
customerAgentInfo_shutdownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_shutdownAgents = Lens.lens (\CustomerAgentInfo' {shutdownAgents} -> shutdownAgents) (\s@CustomerAgentInfo' {} a -> s {shutdownAgents = a} :: CustomerAgentInfo)

-- | Number of unhealthy discovery agents.
customerAgentInfo_unhealthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_unhealthyAgents = Lens.lens (\CustomerAgentInfo' {unhealthyAgents} -> unhealthyAgents) (\s@CustomerAgentInfo' {} a -> s {unhealthyAgents = a} :: CustomerAgentInfo)

-- | Total number of discovery agents.
customerAgentInfo_totalAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_totalAgents = Lens.lens (\CustomerAgentInfo' {totalAgents} -> totalAgents) (\s@CustomerAgentInfo' {} a -> s {totalAgents = a} :: CustomerAgentInfo)

-- | Number of unknown discovery agents.
customerAgentInfo_unknownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
customerAgentInfo_unknownAgents = Lens.lens (\CustomerAgentInfo' {unknownAgents} -> unknownAgents) (\s@CustomerAgentInfo' {} a -> s {unknownAgents = a} :: CustomerAgentInfo)

instance Core.FromJSON CustomerAgentInfo where
  parseJSON =
    Core.withObject
      "CustomerAgentInfo"
      ( \x ->
          CustomerAgentInfo'
            Core.<$> (x Core..: "activeAgents")
            Core.<*> (x Core..: "healthyAgents")
            Core.<*> (x Core..: "blackListedAgents")
            Core.<*> (x Core..: "shutdownAgents")
            Core.<*> (x Core..: "unhealthyAgents")
            Core.<*> (x Core..: "totalAgents")
            Core.<*> (x Core..: "unknownAgents")
      )

instance Core.Hashable CustomerAgentInfo

instance Core.NFData CustomerAgentInfo
