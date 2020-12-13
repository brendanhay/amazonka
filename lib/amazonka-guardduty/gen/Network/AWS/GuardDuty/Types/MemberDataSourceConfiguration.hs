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
    mdscDataSources,
    mdscAccountId,
  )
where

import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on which data sources are enabled for a member account.
--
-- /See:/ 'mkMemberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { -- | Contains information on the status of data sources for the account.
    dataSources :: DataSourceConfigurationsResult,
    -- | The account ID for the member account.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MemberDataSourceConfiguration' with the minimum fields required to make a request.
--
-- * 'dataSources' - Contains information on the status of data sources for the account.
-- * 'accountId' - The account ID for the member account.
mkMemberDataSourceConfiguration ::
  -- | 'dataSources'
  DataSourceConfigurationsResult ->
  -- | 'accountId'
  Lude.Text ->
  MemberDataSourceConfiguration
mkMemberDataSourceConfiguration pDataSources_ pAccountId_ =
  MemberDataSourceConfiguration'
    { dataSources = pDataSources_,
      accountId = pAccountId_
    }

-- | Contains information on the status of data sources for the account.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscDataSources :: Lens.Lens' MemberDataSourceConfiguration DataSourceConfigurationsResult
mdscDataSources = Lens.lens (dataSources :: MemberDataSourceConfiguration -> DataSourceConfigurationsResult) (\s a -> s {dataSources = a} :: MemberDataSourceConfiguration)
{-# DEPRECATED mdscDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The account ID for the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscAccountId :: Lens.Lens' MemberDataSourceConfiguration Lude.Text
mdscAccountId = Lens.lens (accountId :: MemberDataSourceConfiguration -> Lude.Text) (\s a -> s {accountId = a} :: MemberDataSourceConfiguration)
{-# DEPRECATED mdscAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.FromJSON MemberDataSourceConfiguration where
  parseJSON =
    Lude.withObject
      "MemberDataSourceConfiguration"
      ( \x ->
          MemberDataSourceConfiguration'
            Lude.<$> (x Lude..: "dataSources") Lude.<*> (x Lude..: "accountId")
      )
