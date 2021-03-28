{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
  ( CloudTrailConfigurationResult (..)
  -- * Smart constructor
  , mkCloudTrailConfigurationResult
  -- * Lenses
  , ctcrStatus
  ) where

import qualified Network.AWS.GuardDuty.Types.DataSourceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the status of CloudTrail as a data source for the detector.
--
-- /See:/ 'mkCloudTrailConfigurationResult' smart constructor.
newtype CloudTrailConfigurationResult = CloudTrailConfigurationResult'
  { status :: Types.DataSourceStatus
    -- ^ Describes whether CloudTrail is enabled as a data source for the detector.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloudTrailConfigurationResult' value with any optional fields omitted.
mkCloudTrailConfigurationResult
    :: Types.DataSourceStatus -- ^ 'status'
    -> CloudTrailConfigurationResult
mkCloudTrailConfigurationResult status
  = CloudTrailConfigurationResult'{status}

-- | Describes whether CloudTrail is enabled as a data source for the detector.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrStatus :: Lens.Lens' CloudTrailConfigurationResult Types.DataSourceStatus
ctcrStatus = Lens.field @"status"
{-# INLINEABLE ctcrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON CloudTrailConfigurationResult where
        parseJSON
          = Core.withObject "CloudTrailConfigurationResult" Core.$
              \ x -> CloudTrailConfigurationResult' Core.<$> (x Core..: "status")
