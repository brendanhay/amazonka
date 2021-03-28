{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BatchStatementRequest
  ( BatchStatementRequest (..)
  -- * Smart constructor
  , mkBatchStatementRequest
  -- * Lenses
  , bsrStatement
  , bsrConsistentRead
  , bsrParameters
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.Statement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A PartiQL batch statement request. 
--
-- /See:/ 'mkBatchStatementRequest' smart constructor.
data BatchStatementRequest = BatchStatementRequest'
  { statement :: Types.Statement
    -- ^ A valid PartiQL statement. 
  , consistentRead :: Core.Maybe Core.Bool
    -- ^ The read consistency of the PartiQL batch request. 
  , parameters :: Core.Maybe (Core.NonEmpty Types.AttributeValue)
    -- ^ The parameters associated with a PartiQL statement in the batch request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStatementRequest' value with any optional fields omitted.
mkBatchStatementRequest
    :: Types.Statement -- ^ 'statement'
    -> BatchStatementRequest
mkBatchStatementRequest statement
  = BatchStatementRequest'{statement, consistentRead = Core.Nothing,
                           parameters = Core.Nothing}

-- | A valid PartiQL statement. 
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrStatement :: Lens.Lens' BatchStatementRequest Types.Statement
bsrStatement = Lens.field @"statement"
{-# INLINEABLE bsrStatement #-}
{-# DEPRECATED statement "Use generic-lens or generic-optics with 'statement' instead"  #-}

-- | The read consistency of the PartiQL batch request. 
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrConsistentRead :: Lens.Lens' BatchStatementRequest (Core.Maybe Core.Bool)
bsrConsistentRead = Lens.field @"consistentRead"
{-# INLINEABLE bsrConsistentRead #-}
{-# DEPRECATED consistentRead "Use generic-lens or generic-optics with 'consistentRead' instead"  #-}

-- | The parameters associated with a PartiQL statement in the batch request. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrParameters :: Lens.Lens' BatchStatementRequest (Core.Maybe (Core.NonEmpty Types.AttributeValue))
bsrParameters = Lens.field @"parameters"
{-# INLINEABLE bsrParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromJSON BatchStatementRequest where
        toJSON BatchStatementRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Statement" Core..= statement),
                  ("ConsistentRead" Core..=) Core.<$> consistentRead,
                  ("Parameters" Core..=) Core.<$> parameters])
