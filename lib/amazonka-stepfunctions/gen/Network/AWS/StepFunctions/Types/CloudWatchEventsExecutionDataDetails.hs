{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
  ( CloudWatchEventsExecutionDataDetails (..)
  -- * Smart constructor
  , mkCloudWatchEventsExecutionDataDetails
  -- * Lenses
  , cweeddIncluded
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides details about execution input or output.
--
-- /See:/ 'mkCloudWatchEventsExecutionDataDetails' smart constructor.
newtype CloudWatchEventsExecutionDataDetails = CloudWatchEventsExecutionDataDetails'
  { included :: Core.Maybe Core.Bool
    -- ^ Indicates whether input or output was included in the response. Always @true@ for API calls. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchEventsExecutionDataDetails' value with any optional fields omitted.
mkCloudWatchEventsExecutionDataDetails
    :: CloudWatchEventsExecutionDataDetails
mkCloudWatchEventsExecutionDataDetails
  = CloudWatchEventsExecutionDataDetails'{included = Core.Nothing}

-- | Indicates whether input or output was included in the response. Always @true@ for API calls. 
--
-- /Note:/ Consider using 'included' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweeddIncluded :: Lens.Lens' CloudWatchEventsExecutionDataDetails (Core.Maybe Core.Bool)
cweeddIncluded = Lens.field @"included"
{-# INLINEABLE cweeddIncluded #-}
{-# DEPRECATED included "Use generic-lens or generic-optics with 'included' instead"  #-}

instance Core.FromJSON CloudWatchEventsExecutionDataDetails where
        parseJSON
          = Core.withObject "CloudWatchEventsExecutionDataDetails" Core.$
              \ x ->
                CloudWatchEventsExecutionDataDetails' Core.<$>
                  (x Core..:? "included")
