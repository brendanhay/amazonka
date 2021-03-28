{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the bot, including the @> LATEST@ version. To delete a specific version of the bot, use the 'DeleteBotVersion' operation. The @DeleteBot@ operation doesn't immediately remove the bot schema. Instead, it is marked for deletion and removed later.
--
-- Amazon Lex stores utterances indefinitely for improving the ability of your bot to respond to user inputs. These utterances are not removed when the bot is deleted. To remove the utterances, use the 'DeleteUtterances' operation.
-- If a bot has an alias, you can't delete it. Instead, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the alias that refers to the bot. To remove the reference to the bot, delete the alias. If you get the same exception again, delete the referring alias until the @DeleteBot@ operation is successful.
-- This operation requires permissions for the @lex:DeleteBot@ action.
module Network.AWS.LexModels.DeleteBot
    (
    -- * Creating a request
      DeleteBot (..)
    , mkDeleteBot
    -- ** Request lenses
    , dbName

    -- * Destructuring the response
    , DeleteBotResponse (..)
    , mkDeleteBotResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBot' smart constructor.
newtype DeleteBot = DeleteBot'
  { name :: Types.BotName
    -- ^ The name of the bot. The name is case sensitive. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBot' value with any optional fields omitted.
mkDeleteBot
    :: Types.BotName -- ^ 'name'
    -> DeleteBot
mkDeleteBot name = DeleteBot'{name}

-- | The name of the bot. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbName :: Lens.Lens' DeleteBot Types.BotName
dbName = Lens.field @"name"
{-# INLINEABLE dbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteBot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBot where
        toHeaders DeleteBot{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteBot where
        type Rs DeleteBot = DeleteBotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/bots/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBotResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBotResponse' smart constructor.
data DeleteBotResponse = DeleteBotResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotResponse' value with any optional fields omitted.
mkDeleteBotResponse
    :: DeleteBotResponse
mkDeleteBotResponse = DeleteBotResponse'
