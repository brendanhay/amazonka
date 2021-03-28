{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.AssociateLexBot
    (
    -- * Creating a request
      AssociateLexBot (..)
    , mkAssociateLexBot
    -- ** Request lenses
    , albInstanceId
    , albLexBot

    -- * Destructuring the response
    , AssociateLexBotResponse (..)
    , mkAssociateLexBotResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateLexBot' smart constructor.
data AssociateLexBot = AssociateLexBot'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , lexBot :: Types.LexBot
    -- ^ The Amazon Lex box to associate with the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateLexBot' value with any optional fields omitted.
mkAssociateLexBot
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.LexBot -- ^ 'lexBot'
    -> AssociateLexBot
mkAssociateLexBot instanceId lexBot
  = AssociateLexBot'{instanceId, lexBot}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albInstanceId :: Lens.Lens' AssociateLexBot Types.InstanceId
albInstanceId = Lens.field @"instanceId"
{-# INLINEABLE albInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The Amazon Lex box to associate with the instance.
--
-- /Note:/ Consider using 'lexBot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albLexBot :: Lens.Lens' AssociateLexBot Types.LexBot
albLexBot = Lens.field @"lexBot"
{-# INLINEABLE albLexBot #-}
{-# DEPRECATED lexBot "Use generic-lens or generic-optics with 'lexBot' instead"  #-}

instance Core.ToQuery AssociateLexBot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateLexBot where
        toHeaders AssociateLexBot{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateLexBot where
        toJSON AssociateLexBot{..}
          = Core.object
              (Core.catMaybes [Core.Just ("LexBot" Core..= lexBot)])

instance Core.AWSRequest AssociateLexBot where
        type Rs AssociateLexBot = AssociateLexBotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<> "/lex-bot",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssociateLexBotResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateLexBotResponse' smart constructor.
data AssociateLexBotResponse = AssociateLexBotResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateLexBotResponse' value with any optional fields omitted.
mkAssociateLexBotResponse
    :: AssociateLexBotResponse
mkAssociateLexBotResponse = AssociateLexBotResponse'
