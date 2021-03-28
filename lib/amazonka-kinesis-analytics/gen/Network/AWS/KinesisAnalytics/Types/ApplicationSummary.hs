{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.ApplicationSummary
  ( ApplicationSummary (..)
  -- * Smart constructor
  , mkApplicationSummary
  -- * Lenses
  , asApplicationName
  , asApplicationARN
  , asApplicationStatus
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ApplicationName as Types
import qualified Network.AWS.KinesisAnalytics.Types.ApplicationStatus as Types
import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.
--
-- /See:/ 'mkApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the application.
  , applicationARN :: Types.ResourceARN
    -- ^ ARN of the application.
  , applicationStatus :: Types.ApplicationStatus
    -- ^ Status of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSummary' value with any optional fields omitted.
mkApplicationSummary
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.ResourceARN -- ^ 'applicationARN'
    -> Types.ApplicationStatus -- ^ 'applicationStatus'
    -> ApplicationSummary
mkApplicationSummary applicationName applicationARN
  applicationStatus
  = ApplicationSummary'{applicationName, applicationARN,
                        applicationStatus}

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationName :: Lens.Lens' ApplicationSummary Types.ApplicationName
asApplicationName = Lens.field @"applicationName"
{-# INLINEABLE asApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | ARN of the application.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationARN :: Lens.Lens' ApplicationSummary Types.ResourceARN
asApplicationARN = Lens.field @"applicationARN"
{-# INLINEABLE asApplicationARN #-}
{-# DEPRECATED applicationARN "Use generic-lens or generic-optics with 'applicationARN' instead"  #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationStatus :: Lens.Lens' ApplicationSummary Types.ApplicationStatus
asApplicationStatus = Lens.field @"applicationStatus"
{-# INLINEABLE asApplicationStatus #-}
{-# DEPRECATED applicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead"  #-}

instance Core.FromJSON ApplicationSummary where
        parseJSON
          = Core.withObject "ApplicationSummary" Core.$
              \ x ->
                ApplicationSummary' Core.<$>
                  (x Core..: "ApplicationName") Core.<*> x Core..: "ApplicationARN"
                    Core.<*> x Core..: "ApplicationStatus"
