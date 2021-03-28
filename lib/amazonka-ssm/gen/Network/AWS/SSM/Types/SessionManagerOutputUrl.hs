{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionManagerOutputUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionManagerOutputUrl
  ( SessionManagerOutputUrl (..)
  -- * Smart constructor
  , mkSessionManagerOutputUrl
  -- * Lenses
  , smouCloudWatchOutputUrl
  , smouS3OutputUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.SessionManagerCloudWatchOutputUrl as Types
import qualified Network.AWS.SSM.Types.SessionManagerS3OutputUrl as Types

-- | Reserved for future use.
--
-- /See:/ 'mkSessionManagerOutputUrl' smart constructor.
data SessionManagerOutputUrl = SessionManagerOutputUrl'
  { cloudWatchOutputUrl :: Core.Maybe Types.SessionManagerCloudWatchOutputUrl
    -- ^ Reserved for future use.
  , s3OutputUrl :: Core.Maybe Types.SessionManagerS3OutputUrl
    -- ^ Reserved for future use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SessionManagerOutputUrl' value with any optional fields omitted.
mkSessionManagerOutputUrl
    :: SessionManagerOutputUrl
mkSessionManagerOutputUrl
  = SessionManagerOutputUrl'{cloudWatchOutputUrl = Core.Nothing,
                             s3OutputUrl = Core.Nothing}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'cloudWatchOutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smouCloudWatchOutputUrl :: Lens.Lens' SessionManagerOutputUrl (Core.Maybe Types.SessionManagerCloudWatchOutputUrl)
smouCloudWatchOutputUrl = Lens.field @"cloudWatchOutputUrl"
{-# INLINEABLE smouCloudWatchOutputUrl #-}
{-# DEPRECATED cloudWatchOutputUrl "Use generic-lens or generic-optics with 'cloudWatchOutputUrl' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 's3OutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smouS3OutputUrl :: Lens.Lens' SessionManagerOutputUrl (Core.Maybe Types.SessionManagerS3OutputUrl)
smouS3OutputUrl = Lens.field @"s3OutputUrl"
{-# INLINEABLE smouS3OutputUrl #-}
{-# DEPRECATED s3OutputUrl "Use generic-lens or generic-optics with 's3OutputUrl' instead"  #-}

instance Core.FromJSON SessionManagerOutputUrl where
        parseJSON
          = Core.withObject "SessionManagerOutputUrl" Core.$
              \ x ->
                SessionManagerOutputUrl' Core.<$>
                  (x Core..:? "CloudWatchOutputUrl") Core.<*>
                    x Core..:? "S3OutputUrl"
