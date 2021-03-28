{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.WriteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.WriteRequest
  ( WriteRequest (..)
  -- * Smart constructor
  , mkWriteRequest
  -- * Lenses
  , wrDeleteRequest
  , wrPutRequest
  ) where

import qualified Network.AWS.DynamoDB.Types.DeleteRequest as Types
import qualified Network.AWS.DynamoDB.Types.PutRequest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@ . You can only request one of these operations, not both, in a single @WriteRequest@ . If you do need to perform both of these operations, you need to provide two separate @WriteRequest@ objects.
--
-- /See:/ 'mkWriteRequest' smart constructor.
data WriteRequest = WriteRequest'
  { deleteRequest :: Core.Maybe Types.DeleteRequest
    -- ^ A request to perform a @DeleteItem@ operation.
  , putRequest :: Core.Maybe Types.PutRequest
    -- ^ A request to perform a @PutItem@ operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WriteRequest' value with any optional fields omitted.
mkWriteRequest
    :: WriteRequest
mkWriteRequest
  = WriteRequest'{deleteRequest = Core.Nothing,
                  putRequest = Core.Nothing}

-- | A request to perform a @DeleteItem@ operation.
--
-- /Note:/ Consider using 'deleteRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrDeleteRequest :: Lens.Lens' WriteRequest (Core.Maybe Types.DeleteRequest)
wrDeleteRequest = Lens.field @"deleteRequest"
{-# INLINEABLE wrDeleteRequest #-}
{-# DEPRECATED deleteRequest "Use generic-lens or generic-optics with 'deleteRequest' instead"  #-}

-- | A request to perform a @PutItem@ operation.
--
-- /Note:/ Consider using 'putRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrPutRequest :: Lens.Lens' WriteRequest (Core.Maybe Types.PutRequest)
wrPutRequest = Lens.field @"putRequest"
{-# INLINEABLE wrPutRequest #-}
{-# DEPRECATED putRequest "Use generic-lens or generic-optics with 'putRequest' instead"  #-}

instance Core.FromJSON WriteRequest where
        toJSON WriteRequest{..}
          = Core.object
              (Core.catMaybes
                 [("DeleteRequest" Core..=) Core.<$> deleteRequest,
                  ("PutRequest" Core..=) Core.<$> putRequest])

instance Core.FromJSON WriteRequest where
        parseJSON
          = Core.withObject "WriteRequest" Core.$
              \ x ->
                WriteRequest' Core.<$>
                  (x Core..:? "DeleteRequest") Core.<*> x Core..:? "PutRequest"
