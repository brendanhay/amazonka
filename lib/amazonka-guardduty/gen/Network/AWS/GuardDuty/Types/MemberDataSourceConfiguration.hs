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

import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on which data sources are enabled for a member account.
--
-- /See:/ 'mkMemberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { accountId ::
      Lude.Text,
    dataSources ::
      DataSourceConfigurationsResult
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MemberDataSourceConfiguration' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID for the member account.
-- * 'dataSources' - Contains information on the status of data sources for the account.
mkMemberDataSourceConfiguration ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'dataSources'
  DataSourceConfigurationsResult ->
  MemberDataSourceConfiguration
mkMemberDataSourceConfiguration pAccountId_ pDataSources_ =
  MemberDataSourceConfiguration'
    { accountId = pAccountId_,
      dataSources = pDataSources_
    }

-- | The account ID for the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscAccountId :: Lens.Lens' MemberDataSourceConfiguration Lude.Text
mdscAccountId = Lens.lens (accountId :: MemberDataSourceConfiguration -> Lude.Text) (\s a -> s {accountId = a} :: MemberDataSourceConfiguration)
{-# DEPRECATED mdscAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Contains information on the status of data sources for the account.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdscDataSources :: Lens.Lens' MemberDataSourceConfiguration DataSourceConfigurationsResult
mdscDataSources = Lens.lens (dataSources :: MemberDataSourceConfiguration -> DataSourceConfigurationsResult) (\s a -> s {dataSources = a} :: MemberDataSourceConfiguration)
{-# DEPRECATED mdscDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

instance Lude.FromJSON MemberDataSourceConfiguration where
  parseJSON =
    Lude.withObject
      "MemberDataSourceConfiguration"
      ( \x ->
          MemberDataSourceConfiguration'
            Lude.<$> (x Lude..: "accountId") Lude.<*> (x Lude..: "dataSources")
      )
