{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Warning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Warning
  ( Warning (..)
  -- * Smart constructor
  , mkWarning
  -- * Lenses
  , wCode
  , wMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /See:/ 'mkWarning' smart constructor.
data Warning = Warning'
  { code :: Core.Maybe Core.Text
    -- ^ The code of the cross-regional warning.
  , message :: Core.Maybe Core.Text
    -- ^ The message explaining what resources are in a different region from the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Warning' value with any optional fields omitted.
mkWarning
    :: Warning
mkWarning = Warning'{code = Core.Nothing, message = Core.Nothing}

-- | The code of the cross-regional warning.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCode :: Lens.Lens' Warning (Core.Maybe Core.Text)
wCode = Lens.field @"code"
{-# INLINEABLE wCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The message explaining what resources are in a different region from the pipeline.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wMessage :: Lens.Lens' Warning (Core.Maybe Core.Text)
wMessage = Lens.field @"message"
{-# INLINEABLE wMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON Warning where
        parseJSON
          = Core.withObject "Warning" Core.$
              \ x ->
                Warning' Core.<$> (x Core..:? "Code") Core.<*> x Core..:? "Message"
