{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Storage
  ( Storage (..)
  -- * Smart constructor
  , mkStorage
  -- * Lenses
  , sS3
  ) where

import qualified Network.AWS.EC2.Types.S3Storage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'mkStorage' smart constructor.
newtype Storage = Storage'
  { s3 :: Core.Maybe Types.S3Storage
    -- ^ An Amazon S3 storage location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Storage' value with any optional fields omitted.
mkStorage
    :: Storage
mkStorage = Storage'{s3 = Core.Nothing}

-- | An Amazon S3 storage location.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3 :: Lens.Lens' Storage (Core.Maybe Types.S3Storage)
sS3 = Lens.field @"s3"
{-# INLINEABLE sS3 #-}
{-# DEPRECATED s3 "Use generic-lens or generic-optics with 's3' instead"  #-}

instance Core.ToQuery Storage where
        toQuery Storage{..}
          = Core.maybe Core.mempty (Core.toQueryPair "S3") s3

instance Core.FromXML Storage where
        parseXML x = Storage' Core.<$> (x Core..@? "S3")
