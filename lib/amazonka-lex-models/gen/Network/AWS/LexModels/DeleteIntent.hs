{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the intent, including the @> LATEST@ version. To delete a specific version of the intent, use the 'DeleteIntentVersion' operation.
--
-- You can delete a version of an intent only if it is not referenced. To delete an intent that is referred to in one or more bots (see 'how-it-works' ), you must remove those references first. 
-- This operation requires permission for the @lex:DeleteIntent@ action. 
module Network.AWS.LexModels.DeleteIntent
    (
    -- * Creating a request
      DeleteIntent (..)
    , mkDeleteIntent
    -- ** Request lenses
    , diName

    -- * Destructuring the response
    , DeleteIntentResponse (..)
    , mkDeleteIntentResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteIntent' smart constructor.
newtype DeleteIntent = DeleteIntent'
  { name :: Types.IntentName
    -- ^ The name of the intent. The name is case sensitive. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntent' value with any optional fields omitted.
mkDeleteIntent
    :: Types.IntentName -- ^ 'name'
    -> DeleteIntent
mkDeleteIntent name = DeleteIntent'{name}

-- | The name of the intent. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeleteIntent Types.IntentName
diName = Lens.field @"name"
{-# INLINEABLE diName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteIntent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIntent where
        toHeaders DeleteIntent{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteIntent where
        type Rs DeleteIntent = DeleteIntentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/intents/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteIntentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIntentResponse' smart constructor.
data DeleteIntentResponse = DeleteIntentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntentResponse' value with any optional fields omitted.
mkDeleteIntentResponse
    :: DeleteIntentResponse
mkDeleteIntentResponse = DeleteIntentResponse'
