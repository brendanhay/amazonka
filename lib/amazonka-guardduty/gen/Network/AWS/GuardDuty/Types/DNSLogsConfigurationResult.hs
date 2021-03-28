{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
  ( DNSLogsConfigurationResult (..)
  -- * Smart constructor
  , mkDNSLogsConfigurationResult
  -- * Lenses
  , dnslcrStatus
  ) where

import qualified Network.AWS.GuardDuty.Types.DataSourceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the status of DNS logs as a data source.
--
-- /See:/ 'mkDNSLogsConfigurationResult' smart constructor.
newtype DNSLogsConfigurationResult = DNSLogsConfigurationResult'
  { status :: Types.DataSourceStatus
    -- ^ Denotes whether DNS logs is enabled as a data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DNSLogsConfigurationResult' value with any optional fields omitted.
mkDNSLogsConfigurationResult
    :: Types.DataSourceStatus -- ^ 'status'
    -> DNSLogsConfigurationResult
mkDNSLogsConfigurationResult status
  = DNSLogsConfigurationResult'{status}

-- | Denotes whether DNS logs is enabled as a data source.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnslcrStatus :: Lens.Lens' DNSLogsConfigurationResult Types.DataSourceStatus
dnslcrStatus = Lens.field @"status"
{-# INLINEABLE dnslcrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON DNSLogsConfigurationResult where
        parseJSON
          = Core.withObject "DNSLogsConfigurationResult" Core.$
              \ x -> DNSLogsConfigurationResult' Core.<$> (x Core..: "status")
