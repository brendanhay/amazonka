{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteSlotTypeVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a slot type. To delete all versions of a slot type, use the 'DeleteSlotType' operation. 
--
-- This operation requires permissions for the @lex:DeleteSlotTypeVersion@ action.
module Network.AWS.LexModels.DeleteSlotTypeVersion
    (
    -- * Creating a request
      DeleteSlotTypeVersion (..)
    , mkDeleteSlotTypeVersion
    -- ** Request lenses
    , dstvName
    , dstvVersion

    -- * Destructuring the response
    , DeleteSlotTypeVersionResponse (..)
    , mkDeleteSlotTypeVersionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSlotTypeVersion' smart constructor.
data DeleteSlotTypeVersion = DeleteSlotTypeVersion'
  { name :: Types.SlotTypeName
    -- ^ The name of the slot type.
  , version :: Types.NumericalVersion
    -- ^ The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSlotTypeVersion' value with any optional fields omitted.
mkDeleteSlotTypeVersion
    :: Types.SlotTypeName -- ^ 'name'
    -> Types.NumericalVersion -- ^ 'version'
    -> DeleteSlotTypeVersion
mkDeleteSlotTypeVersion name version
  = DeleteSlotTypeVersion'{name, version}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstvName :: Lens.Lens' DeleteSlotTypeVersion Types.SlotTypeName
dstvName = Lens.field @"name"
{-# INLINEABLE dstvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstvVersion :: Lens.Lens' DeleteSlotTypeVersion Types.NumericalVersion
dstvVersion = Lens.field @"version"
{-# INLINEABLE dstvVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery DeleteSlotTypeVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSlotTypeVersion where
        toHeaders DeleteSlotTypeVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteSlotTypeVersion where
        type Rs DeleteSlotTypeVersion = DeleteSlotTypeVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/slottypes/" Core.<> Core.toText name Core.<> "/version/" Core.<>
                             Core.toText version,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteSlotTypeVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSlotTypeVersionResponse' smart constructor.
data DeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSlotTypeVersionResponse' value with any optional fields omitted.
mkDeleteSlotTypeVersionResponse
    :: DeleteSlotTypeVersionResponse
mkDeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
