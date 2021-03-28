{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.BucketLoggingStatus
  ( BucketLoggingStatus (..)
  -- * Smart constructor
  , mkBucketLoggingStatus
  -- * Lenses
  , blsLoggingEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.LoggingEnabled as Types

-- | Container for logging status information.
--
-- /See:/ 'mkBucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
  { loggingEnabled :: Core.Maybe Types.LoggingEnabled
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BucketLoggingStatus' value with any optional fields omitted.
mkBucketLoggingStatus
    :: BucketLoggingStatus
mkBucketLoggingStatus
  = BucketLoggingStatus'{loggingEnabled = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blsLoggingEnabled :: Lens.Lens' BucketLoggingStatus (Core.Maybe Types.LoggingEnabled)
blsLoggingEnabled = Lens.field @"loggingEnabled"
{-# INLINEABLE blsLoggingEnabled #-}
{-# DEPRECATED loggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead"  #-}

instance Core.ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus{..}
          = Core.maybe Core.mempty (Core.toXMLElement "LoggingEnabled")
              loggingEnabled
