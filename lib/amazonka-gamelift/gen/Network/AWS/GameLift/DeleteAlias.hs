{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias. This operation removes all record of the alias. Game clients attempting to access a server process using the deleted alias receive an error. To delete an alias, specify the alias ID to be deleted.
--
--
--     * 'CreateAlias' 
--
--
--     * 'ListAliases' 
--
--
--     * 'DescribeAlias' 
--
--
--     * 'UpdateAlias' 
--
--
--     * 'DeleteAlias' 
--
--
--     * 'ResolveAlias' 
--
--
module Network.AWS.GameLift.DeleteAlias
    (
    -- * Creating a request
      DeleteAlias (..)
    , mkDeleteAlias
    -- ** Request lenses
    , daAliasId

    -- * Destructuring the response
    , DeleteAliasResponse (..)
    , mkDeleteAliasResponse
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias'
  { aliasId :: Types.AliasIdOrArn
    -- ^ A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlias' value with any optional fields omitted.
mkDeleteAlias
    :: Types.AliasIdOrArn -- ^ 'aliasId'
    -> DeleteAlias
mkDeleteAlias aliasId = DeleteAlias'{aliasId}

-- | A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAliasId :: Lens.Lens' DeleteAlias Types.AliasIdOrArn
daAliasId = Lens.field @"aliasId"
{-# INLINEABLE daAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

instance Core.ToQuery DeleteAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAlias where
        toHeaders DeleteAlias{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteAlias") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAlias where
        toJSON DeleteAlias{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest DeleteAlias where
        type Rs DeleteAlias = DeleteAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAliasResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAliasResponse' value with any optional fields omitted.
mkDeleteAliasResponse
    :: DeleteAliasResponse
mkDeleteAliasResponse = DeleteAliasResponse'
