{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes authorization from the specified instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.DisassociateLexBot
    (
    -- * Creating a request
      DisassociateLexBot (..)
    , mkDisassociateLexBot
    -- ** Request lenses
    , dlbInstanceId
    , dlbBotName
    , dlbLexRegion

    -- * Destructuring the response
    , DisassociateLexBotResponse (..)
    , mkDisassociateLexBotResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , botName :: Types.BotName
    -- ^ The name of the Amazon Lex bot. Maximum character limit of 50.
  , lexRegion :: Types.LexRegion
    -- ^ The Region in which the Amazon Lex bot has been created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLexBot' value with any optional fields omitted.
mkDisassociateLexBot
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.BotName -- ^ 'botName'
    -> Types.LexRegion -- ^ 'lexRegion'
    -> DisassociateLexBot
mkDisassociateLexBot instanceId botName lexRegion
  = DisassociateLexBot'{instanceId, botName, lexRegion}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbInstanceId :: Lens.Lens' DisassociateLexBot Types.InstanceId
dlbInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dlbInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbBotName :: Lens.Lens' DisassociateLexBot Types.BotName
dlbBotName = Lens.field @"botName"
{-# INLINEABLE dlbBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The Region in which the Amazon Lex bot has been created.
--
-- /Note:/ Consider using 'lexRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLexRegion :: Lens.Lens' DisassociateLexBot Types.LexRegion
dlbLexRegion = Lens.field @"lexRegion"
{-# INLINEABLE dlbLexRegion #-}
{-# DEPRECATED lexRegion "Use generic-lens or generic-optics with 'lexRegion' instead"  #-}

instance Core.ToQuery DisassociateLexBot where
        toQuery DisassociateLexBot{..}
          = Core.toQueryPair "botName" botName Core.<>
              Core.toQueryPair "lexRegion" lexRegion

instance Core.ToHeaders DisassociateLexBot where
        toHeaders DisassociateLexBot{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateLexBot where
        type Rs DisassociateLexBot = DisassociateLexBotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<> "/lex-bot",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisassociateLexBotResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLexBotResponse' value with any optional fields omitted.
mkDisassociateLexBotResponse
    :: DisassociateLexBotResponse
mkDisassociateLexBotResponse = DisassociateLexBotResponse'
