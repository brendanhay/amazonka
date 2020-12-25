{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
  ( MemberDataSourceConfiguration (..),

    -- * Smart constructor
    mkMemberDataSourceConfiguration,

    -- * Lenses
    mdscAccountId,
    mdscDataSources,
  )
where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on which data sources are enabled for a member account.
--
-- /See:/ 'mkMemberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { -- | The account ID for the member account.
    accountId :: Types.AccountId,
    -- | Contains information on the status of data sources for the account.
    dataSources :: Types.DataSourceConfigurationsResult
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MemberDataSourceConfiguration' value with any optional fields omitted.
mkMemberDataSourceConfiguration ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'dataSources'
  Types.DataSourceConfigurationsResult ->
  MemberDataSourceConfiguration
mkMemberDataSourceConfiguration accountId dataSources =
  MemberDataSourceConfiguration' {accountId, dataSources}

-- | The account ID for the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscAccountId :: Lens.Lens' MemberDataSourceConfiguration Types.AccountId
mdscAccountId = Lens.field @"accountId"
{-# DEPRECATED mdscAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Contains information on the status of data sources for the account.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscDataSources :: Lens.Lens' MemberDataSourceConfiguration Types.DataSourceConfigurationsResult
mdscDataSources = Lens.field @"dataSources"
{-# DEPRECATED mdscDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

instance Core.FromJSON MemberDataSourceConfiguration where
  parseJSON =
    Core.withObject "MemberDataSourceConfiguration" Core.$
      \x ->
        MemberDataSourceConfiguration'
          Core.<$> (x Core..: "accountId") Core.<*> (x Core..: "dataSources")
