{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.AccelerateConfiguration
  ( AccelerateConfiguration (..)
  -- * Smart constructor
  , mkAccelerateConfiguration
  -- * Lenses
  , acStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.BucketAccelerateStatus as Types

-- | Configures the transfer acceleration state for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Amazon S3 Transfer Acceleration> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkAccelerateConfiguration' smart constructor.
newtype AccelerateConfiguration = AccelerateConfiguration'
  { status :: Core.Maybe Types.BucketAccelerateStatus
    -- ^ Specifies the transfer acceleration status of the bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccelerateConfiguration' value with any optional fields omitted.
mkAccelerateConfiguration
    :: AccelerateConfiguration
mkAccelerateConfiguration
  = AccelerateConfiguration'{status = Core.Nothing}

-- | Specifies the transfer acceleration status of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acStatus :: Lens.Lens' AccelerateConfiguration (Core.Maybe Types.BucketAccelerateStatus)
acStatus = Lens.field @"status"
{-# INLINEABLE acStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToXML AccelerateConfiguration where
        toXML AccelerateConfiguration{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Status") status
