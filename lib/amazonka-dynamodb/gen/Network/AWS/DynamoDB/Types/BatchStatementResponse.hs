{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BatchStatementResponse
  ( BatchStatementResponse (..)
  -- * Smart constructor
  , mkBatchStatementResponse
  -- * Lenses
  , bsrError
  , bsrItem
  , bsrTableName
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.BatchStatementError as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A PartiQL batch statement response.. 
--
-- /See:/ 'mkBatchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { error :: Core.Maybe Types.BatchStatementError
    -- ^ The error associated with a failed PartiQL batch statement. 
  , item :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A DynamoDB item associated with a BatchStatementResponse 
  , tableName :: Core.Maybe Types.TableName
    -- ^ The table name associated with a failed PartiQL batch statement. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStatementResponse' value with any optional fields omitted.
mkBatchStatementResponse
    :: BatchStatementResponse
mkBatchStatementResponse
  = BatchStatementResponse'{error = Core.Nothing,
                            item = Core.Nothing, tableName = Core.Nothing}

-- | The error associated with a failed PartiQL batch statement. 
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrError :: Lens.Lens' BatchStatementResponse (Core.Maybe Types.BatchStatementError)
bsrError = Lens.field @"error"
{-# INLINEABLE bsrError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | A DynamoDB item associated with a BatchStatementResponse 
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrItem :: Lens.Lens' BatchStatementResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
bsrItem = Lens.field @"item"
{-# INLINEABLE bsrItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The table name associated with a failed PartiQL batch statement. 
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrTableName :: Lens.Lens' BatchStatementResponse (Core.Maybe Types.TableName)
bsrTableName = Lens.field @"tableName"
{-# INLINEABLE bsrTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.FromJSON BatchStatementResponse where
        parseJSON
          = Core.withObject "BatchStatementResponse" Core.$
              \ x ->
                BatchStatementResponse' Core.<$>
                  (x Core..:? "Error") Core.<*> x Core..:? "Item" Core.<*>
                    x Core..:? "TableName"
