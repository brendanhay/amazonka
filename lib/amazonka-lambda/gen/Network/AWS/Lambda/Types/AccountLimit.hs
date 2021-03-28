{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.AccountLimit
  ( AccountLimit (..)
  -- * Smart constructor
  , mkAccountLimit
  -- * Lenses
  , alCodeSizeUnzipped
  , alCodeSizeZipped
  , alConcurrentExecutions
  , alTotalCodeSize
  , alUnreservedConcurrentExecutions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Limits that are related to concurrency and storage. All file and storage sizes are in bytes.
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { codeSizeUnzipped :: Core.Maybe Core.Integer
    -- ^ The maximum size of a function's deployment package and layers when they're extracted.
  , codeSizeZipped :: Core.Maybe Core.Integer
    -- ^ The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
  , concurrentExecutions :: Core.Maybe Core.Int
    -- ^ The maximum number of simultaneous function executions.
  , totalCodeSize :: Core.Maybe Core.Integer
    -- ^ The amount of storage space that you can use for all deployment packages and layer archives.
  , unreservedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountLimit' value with any optional fields omitted.
mkAccountLimit
    :: AccountLimit
mkAccountLimit
  = AccountLimit'{codeSizeUnzipped = Core.Nothing,
                  codeSizeZipped = Core.Nothing, concurrentExecutions = Core.Nothing,
                  totalCodeSize = Core.Nothing,
                  unreservedConcurrentExecutions = Core.Nothing}

-- | The maximum size of a function's deployment package and layers when they're extracted.
--
-- /Note:/ Consider using 'codeSizeUnzipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alCodeSizeUnzipped :: Lens.Lens' AccountLimit (Core.Maybe Core.Integer)
alCodeSizeUnzipped = Lens.field @"codeSizeUnzipped"
{-# INLINEABLE alCodeSizeUnzipped #-}
{-# DEPRECATED codeSizeUnzipped "Use generic-lens or generic-optics with 'codeSizeUnzipped' instead"  #-}

-- | The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
--
-- /Note:/ Consider using 'codeSizeZipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alCodeSizeZipped :: Lens.Lens' AccountLimit (Core.Maybe Core.Integer)
alCodeSizeZipped = Lens.field @"codeSizeZipped"
{-# INLINEABLE alCodeSizeZipped #-}
{-# DEPRECATED codeSizeZipped "Use generic-lens or generic-optics with 'codeSizeZipped' instead"  #-}

-- | The maximum number of simultaneous function executions.
--
-- /Note:/ Consider using 'concurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alConcurrentExecutions :: Lens.Lens' AccountLimit (Core.Maybe Core.Int)
alConcurrentExecutions = Lens.field @"concurrentExecutions"
{-# INLINEABLE alConcurrentExecutions #-}
{-# DEPRECATED concurrentExecutions "Use generic-lens or generic-optics with 'concurrentExecutions' instead"  #-}

-- | The amount of storage space that you can use for all deployment packages and layer archives.
--
-- /Note:/ Consider using 'totalCodeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alTotalCodeSize :: Lens.Lens' AccountLimit (Core.Maybe Core.Integer)
alTotalCodeSize = Lens.field @"totalCodeSize"
{-# INLINEABLE alTotalCodeSize #-}
{-# DEPRECATED totalCodeSize "Use generic-lens or generic-optics with 'totalCodeSize' instead"  #-}

-- | The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
--
-- /Note:/ Consider using 'unreservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alUnreservedConcurrentExecutions :: Lens.Lens' AccountLimit (Core.Maybe Core.Natural)
alUnreservedConcurrentExecutions = Lens.field @"unreservedConcurrentExecutions"
{-# INLINEABLE alUnreservedConcurrentExecutions #-}
{-# DEPRECATED unreservedConcurrentExecutions "Use generic-lens or generic-optics with 'unreservedConcurrentExecutions' instead"  #-}

instance Core.FromJSON AccountLimit where
        parseJSON
          = Core.withObject "AccountLimit" Core.$
              \ x ->
                AccountLimit' Core.<$>
                  (x Core..:? "CodeSizeUnzipped") Core.<*>
                    x Core..:? "CodeSizeZipped"
                    Core.<*> x Core..:? "ConcurrentExecutions"
                    Core.<*> x Core..:? "TotalCodeSize"
                    Core.<*> x Core..:? "UnreservedConcurrentExecutions"
