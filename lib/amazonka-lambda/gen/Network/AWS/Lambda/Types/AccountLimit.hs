-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AccountLimit
  ( AccountLimit (..),

    -- * Smart constructor
    mkAccountLimit,

    -- * Lenses
    alConcurrentExecutions,
    alTotalCodeSize,
    alUnreservedConcurrentExecutions,
    alCodeSizeUnzipped,
    alCodeSizeZipped,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Limits that are related to concurrency and storage. All file and storage sizes are in bytes.
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { concurrentExecutions ::
      Lude.Maybe Lude.Int,
    totalCodeSize :: Lude.Maybe Lude.Integer,
    unreservedConcurrentExecutions :: Lude.Maybe Lude.Natural,
    codeSizeUnzipped :: Lude.Maybe Lude.Integer,
    codeSizeZipped :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- * 'codeSizeUnzipped' - The maximum size of a function's deployment package and layers when they're extracted.
-- * 'codeSizeZipped' - The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
-- * 'concurrentExecutions' - The maximum number of simultaneous function executions.
-- * 'totalCodeSize' - The amount of storage space that you can use for all deployment packages and layer archives.
-- * 'unreservedConcurrentExecutions' - The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
mkAccountLimit ::
  AccountLimit
mkAccountLimit =
  AccountLimit'
    { concurrentExecutions = Lude.Nothing,
      totalCodeSize = Lude.Nothing,
      unreservedConcurrentExecutions = Lude.Nothing,
      codeSizeUnzipped = Lude.Nothing,
      codeSizeZipped = Lude.Nothing
    }

-- | The maximum number of simultaneous function executions.
--
-- /Note:/ Consider using 'concurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alConcurrentExecutions :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Int)
alConcurrentExecutions = Lens.lens (concurrentExecutions :: AccountLimit -> Lude.Maybe Lude.Int) (\s a -> s {concurrentExecutions = a} :: AccountLimit)
{-# DEPRECATED alConcurrentExecutions "Use generic-lens or generic-optics with 'concurrentExecutions' instead." #-}

-- | The amount of storage space that you can use for all deployment packages and layer archives.
--
-- /Note:/ Consider using 'totalCodeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alTotalCodeSize :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Integer)
alTotalCodeSize = Lens.lens (totalCodeSize :: AccountLimit -> Lude.Maybe Lude.Integer) (\s a -> s {totalCodeSize = a} :: AccountLimit)
{-# DEPRECATED alTotalCodeSize "Use generic-lens or generic-optics with 'totalCodeSize' instead." #-}

-- | The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
--
-- /Note:/ Consider using 'unreservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alUnreservedConcurrentExecutions :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Natural)
alUnreservedConcurrentExecutions = Lens.lens (unreservedConcurrentExecutions :: AccountLimit -> Lude.Maybe Lude.Natural) (\s a -> s {unreservedConcurrentExecutions = a} :: AccountLimit)
{-# DEPRECATED alUnreservedConcurrentExecutions "Use generic-lens or generic-optics with 'unreservedConcurrentExecutions' instead." #-}

-- | The maximum size of a function's deployment package and layers when they're extracted.
--
-- /Note:/ Consider using 'codeSizeUnzipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alCodeSizeUnzipped :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Integer)
alCodeSizeUnzipped = Lens.lens (codeSizeUnzipped :: AccountLimit -> Lude.Maybe Lude.Integer) (\s a -> s {codeSizeUnzipped = a} :: AccountLimit)
{-# DEPRECATED alCodeSizeUnzipped "Use generic-lens or generic-optics with 'codeSizeUnzipped' instead." #-}

-- | The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
--
-- /Note:/ Consider using 'codeSizeZipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alCodeSizeZipped :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Integer)
alCodeSizeZipped = Lens.lens (codeSizeZipped :: AccountLimit -> Lude.Maybe Lude.Integer) (\s a -> s {codeSizeZipped = a} :: AccountLimit)
{-# DEPRECATED alCodeSizeZipped "Use generic-lens or generic-optics with 'codeSizeZipped' instead." #-}

instance Lude.FromJSON AccountLimit where
  parseJSON =
    Lude.withObject
      "AccountLimit"
      ( \x ->
          AccountLimit'
            Lude.<$> (x Lude..:? "ConcurrentExecutions")
            Lude.<*> (x Lude..:? "TotalCodeSize")
            Lude.<*> (x Lude..:? "UnreservedConcurrentExecutions")
            Lude.<*> (x Lude..:? "CodeSizeUnzipped")
            Lude.<*> (x Lude..:? "CodeSizeZipped")
      )
