-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurations
  ( DataSourceConfigurations (..),

    -- * Smart constructor
    mkDataSourceConfigurations,

    -- * Lenses
    dscS3Logs,
  )
where

import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about which data sources are enabled.
--
-- /See:/ 'mkDataSourceConfigurations' smart constructor.
newtype DataSourceConfigurations = DataSourceConfigurations'
  { s3Logs ::
      Lude.Maybe S3LogsConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSourceConfigurations' with the minimum fields required to make a request.
--
-- * 's3Logs' - Describes whether S3 data event logs are enabled as a data source.
mkDataSourceConfigurations ::
  DataSourceConfigurations
mkDataSourceConfigurations =
  DataSourceConfigurations' {s3Logs = Lude.Nothing}

-- | Describes whether S3 data event logs are enabled as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscS3Logs :: Lens.Lens' DataSourceConfigurations (Lude.Maybe S3LogsConfiguration)
dscS3Logs = Lens.lens (s3Logs :: DataSourceConfigurations -> Lude.Maybe S3LogsConfiguration) (\s a -> s {s3Logs = a} :: DataSourceConfigurations)
{-# DEPRECATED dscS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

instance Lude.ToJSON DataSourceConfigurations where
  toJSON DataSourceConfigurations' {..} =
    Lude.object (Lude.catMaybes [("s3Logs" Lude..=) Lude.<$> s3Logs])
