{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BatchStatementError
  ( BatchStatementError (..)
  -- * Smart constructor
  , mkBatchStatementError
  -- * Lenses
  , bseCode
  , bseMessage
  ) where

import qualified Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An error associated with a statement in a PartiQL batch that was run. 
--
-- /See:/ 'mkBatchStatementError' smart constructor.
data BatchStatementError = BatchStatementError'
  { code :: Core.Maybe Types.BatchStatementErrorCodeEnum
    -- ^ The error code associated with the failed PartiQL batch statement. 
  , message :: Core.Maybe Core.Text
    -- ^ The error message associated with the PartiQL batch resposne. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStatementError' value with any optional fields omitted.
mkBatchStatementError
    :: BatchStatementError
mkBatchStatementError
  = BatchStatementError'{code = Core.Nothing, message = Core.Nothing}

-- | The error code associated with the failed PartiQL batch statement. 
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bseCode :: Lens.Lens' BatchStatementError (Core.Maybe Types.BatchStatementErrorCodeEnum)
bseCode = Lens.field @"code"
{-# INLINEABLE bseCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The error message associated with the PartiQL batch resposne. 
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bseMessage :: Lens.Lens' BatchStatementError (Core.Maybe Core.Text)
bseMessage = Lens.field @"message"
{-# INLINEABLE bseMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON BatchStatementError where
        parseJSON
          = Core.withObject "BatchStatementError" Core.$
              \ x ->
                BatchStatementError' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
