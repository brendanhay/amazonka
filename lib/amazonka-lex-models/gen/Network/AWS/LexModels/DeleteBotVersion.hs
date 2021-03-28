{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all versions of a bot, use the 'DeleteBot' operation. 
--
-- This operation requires permissions for the @lex:DeleteBotVersion@ action.
module Network.AWS.LexModels.DeleteBotVersion
    (
    -- * Creating a request
      DeleteBotVersion (..)
    , mkDeleteBotVersion
    -- ** Request lenses
    , dbvName
    , dbvVersion

    -- * Destructuring the response
    , DeleteBotVersionResponse (..)
    , mkDeleteBotVersionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { name :: Types.BotName
    -- ^ The name of the bot.
  , version :: Types.NumericalVersion
    -- ^ The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotVersion' value with any optional fields omitted.
mkDeleteBotVersion
    :: Types.BotName -- ^ 'name'
    -> Types.NumericalVersion -- ^ 'version'
    -> DeleteBotVersion
mkDeleteBotVersion name version = DeleteBotVersion'{name, version}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbvName :: Lens.Lens' DeleteBotVersion Types.BotName
dbvName = Lens.field @"name"
{-# INLINEABLE dbvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbvVersion :: Lens.Lens' DeleteBotVersion Types.NumericalVersion
dbvVersion = Lens.field @"version"
{-# INLINEABLE dbvVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery DeleteBotVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBotVersion where
        toHeaders DeleteBotVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteBotVersion where
        type Rs DeleteBotVersion = DeleteBotVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText name Core.<> "/versions/" Core.<>
                             Core.toText version,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBotVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotVersionResponse' value with any optional fields omitted.
mkDeleteBotVersionResponse
    :: DeleteBotVersionResponse
mkDeleteBotVersionResponse = DeleteBotVersionResponse'
