{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
  ( DNSLogsConfigurationResult (..),

    -- * Smart constructor
    mkDNSLogsConfigurationResult,

    -- * Lenses
    dlcrStatus,
  )
where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the status of DNS logs as a data source.
--
-- /See:/ 'mkDNSLogsConfigurationResult' smart constructor.
newtype DNSLogsConfigurationResult = DNSLogsConfigurationResult'
  { -- | Denotes whether DNS logs is enabled as a data source.
    status :: DataSourceStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSLogsConfigurationResult' with the minimum fields required to make a request.
--
-- * 'status' - Denotes whether DNS logs is enabled as a data source.
mkDNSLogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  DNSLogsConfigurationResult
mkDNSLogsConfigurationResult pStatus_ =
  DNSLogsConfigurationResult' {status = pStatus_}

-- | Denotes whether DNS logs is enabled as a data source.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrStatus :: Lens.Lens' DNSLogsConfigurationResult DataSourceStatus
dlcrStatus = Lens.lens (status :: DNSLogsConfigurationResult -> DataSourceStatus) (\s a -> s {status = a} :: DNSLogsConfigurationResult)
{-# DEPRECATED dlcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON DNSLogsConfigurationResult where
  parseJSON =
    Lude.withObject
      "DNSLogsConfigurationResult"
      (\x -> DNSLogsConfigurationResult' Lude.<$> (x Lude..: "status"))
