{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientData
  ( ClientData (..)
  -- * Smart constructor
  , mkClientData
  -- * Lenses
  , cdComment
  , cdUploadEnd
  , cdUploadSize
  , cdUploadStart
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the client-specific data.
--
-- /See:/ 'mkClientData' smart constructor.
data ClientData = ClientData'
  { comment :: Core.Maybe Core.Text
    -- ^ A user-defined comment about the disk upload.
  , uploadEnd :: Core.Maybe Core.UTCTime
    -- ^ The time that the disk upload ends.
  , uploadSize :: Core.Maybe Core.Double
    -- ^ The size of the uploaded disk image, in GiB.
  , uploadStart :: Core.Maybe Core.UTCTime
    -- ^ The time that the disk upload starts.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClientData' value with any optional fields omitted.
mkClientData
    :: ClientData
mkClientData
  = ClientData'{comment = Core.Nothing, uploadEnd = Core.Nothing,
                uploadSize = Core.Nothing, uploadStart = Core.Nothing}

-- | A user-defined comment about the disk upload.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComment :: Lens.Lens' ClientData (Core.Maybe Core.Text)
cdComment = Lens.field @"comment"
{-# INLINEABLE cdComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The time that the disk upload ends.
--
-- /Note:/ Consider using 'uploadEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadEnd :: Lens.Lens' ClientData (Core.Maybe Core.UTCTime)
cdUploadEnd = Lens.field @"uploadEnd"
{-# INLINEABLE cdUploadEnd #-}
{-# DEPRECATED uploadEnd "Use generic-lens or generic-optics with 'uploadEnd' instead"  #-}

-- | The size of the uploaded disk image, in GiB.
--
-- /Note:/ Consider using 'uploadSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadSize :: Lens.Lens' ClientData (Core.Maybe Core.Double)
cdUploadSize = Lens.field @"uploadSize"
{-# INLINEABLE cdUploadSize #-}
{-# DEPRECATED uploadSize "Use generic-lens or generic-optics with 'uploadSize' instead"  #-}

-- | The time that the disk upload starts.
--
-- /Note:/ Consider using 'uploadStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadStart :: Lens.Lens' ClientData (Core.Maybe Core.UTCTime)
cdUploadStart = Lens.field @"uploadStart"
{-# INLINEABLE cdUploadStart #-}
{-# DEPRECATED uploadStart "Use generic-lens or generic-optics with 'uploadStart' instead"  #-}

instance Core.ToQuery ClientData where
        toQuery ClientData{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Comment") comment
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UploadEnd") uploadEnd
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UploadSize") uploadSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UploadStart") uploadStart
