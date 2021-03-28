{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias for the specified bot. 
--
-- You can't delete an alias that is used in the association between a bot and a messaging channel. If an alias is used in a channel association, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the channel association that refers to the bot. You can remove the reference to the alias by deleting the channel association. If you get the same exception again, delete the referring association until the @DeleteBotAlias@ operation is successful.
module Network.AWS.LexModels.DeleteBotAlias
    (
    -- * Creating a request
      DeleteBotAlias (..)
    , mkDeleteBotAlias
    -- ** Request lenses
    , dbaName
    , dbaBotName

    -- * Destructuring the response
    , DeleteBotAliasResponse (..)
    , mkDeleteBotAliasResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBotAlias' smart constructor.
data DeleteBotAlias = DeleteBotAlias'
  { name :: Types.AliasName
    -- ^ The name of the alias to delete. The name is case sensitive. 
  , botName :: Types.BotName
    -- ^ The name of the bot that the alias points to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotAlias' value with any optional fields omitted.
mkDeleteBotAlias
    :: Types.AliasName -- ^ 'name'
    -> Types.BotName -- ^ 'botName'
    -> DeleteBotAlias
mkDeleteBotAlias name botName = DeleteBotAlias'{name, botName}

-- | The name of the alias to delete. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaName :: Lens.Lens' DeleteBotAlias Types.AliasName
dbaName = Lens.field @"name"
{-# INLINEABLE dbaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaBotName :: Lens.Lens' DeleteBotAlias Types.BotName
dbaBotName = Lens.field @"botName"
{-# INLINEABLE dbaBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

instance Core.ToQuery DeleteBotAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBotAlias where
        toHeaders DeleteBotAlias{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteBotAlias where
        type Rs DeleteBotAlias = DeleteBotAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText botName Core.<> "/aliases/" Core.<>
                             Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBotAliasResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBotAliasResponse' smart constructor.
data DeleteBotAliasResponse = DeleteBotAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotAliasResponse' value with any optional fields omitted.
mkDeleteBotAliasResponse
    :: DeleteBotAliasResponse
mkDeleteBotAliasResponse = DeleteBotAliasResponse'
