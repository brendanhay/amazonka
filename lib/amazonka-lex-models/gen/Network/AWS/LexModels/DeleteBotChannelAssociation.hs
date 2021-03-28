{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between an Amazon Lex bot and a messaging platform.
--
-- This operation requires permission for the @lex:DeleteBotChannelAssociation@ action.
module Network.AWS.LexModels.DeleteBotChannelAssociation
    (
    -- * Creating a request
      DeleteBotChannelAssociation (..)
    , mkDeleteBotChannelAssociation
    -- ** Request lenses
    , dbcaName
    , dbcaBotName
    , dbcaBotAlias

    -- * Destructuring the response
    , DeleteBotChannelAssociationResponse (..)
    , mkDeleteBotChannelAssociationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBotChannelAssociation' smart constructor.
data DeleteBotChannelAssociation = DeleteBotChannelAssociation'
  { name :: Types.BotChannelName
    -- ^ The name of the association. The name is case sensitive. 
  , botName :: Types.BotName
    -- ^ The name of the Amazon Lex bot.
  , botAlias :: Types.AliasName
    -- ^ An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotChannelAssociation' value with any optional fields omitted.
mkDeleteBotChannelAssociation
    :: Types.BotChannelName -- ^ 'name'
    -> Types.BotName -- ^ 'botName'
    -> Types.AliasName -- ^ 'botAlias'
    -> DeleteBotChannelAssociation
mkDeleteBotChannelAssociation name botName botAlias
  = DeleteBotChannelAssociation'{name, botName, botAlias}

-- | The name of the association. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaName :: Lens.Lens' DeleteBotChannelAssociation Types.BotChannelName
dbcaName = Lens.field @"name"
{-# INLINEABLE dbcaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaBotName :: Lens.Lens' DeleteBotChannelAssociation Types.BotName
dbcaBotName = Lens.field @"botName"
{-# INLINEABLE dbcaBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaBotAlias :: Lens.Lens' DeleteBotChannelAssociation Types.AliasName
dbcaBotAlias = Lens.field @"botAlias"
{-# INLINEABLE dbcaBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

instance Core.ToQuery DeleteBotChannelAssociation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBotChannelAssociation where
        toHeaders DeleteBotChannelAssociation{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteBotChannelAssociation where
        type Rs DeleteBotChannelAssociation =
             DeleteBotChannelAssociationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText botName Core.<> "/aliases/" Core.<>
                             Core.toText botAlias
                             Core.<> "/channels/"
                             Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteBotChannelAssociationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBotChannelAssociationResponse' smart constructor.
data DeleteBotChannelAssociationResponse = DeleteBotChannelAssociationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBotChannelAssociationResponse' value with any optional fields omitted.
mkDeleteBotChannelAssociationResponse
    :: DeleteBotChannelAssociationResponse
mkDeleteBotChannelAssociationResponse
  = DeleteBotChannelAssociationResponse'
