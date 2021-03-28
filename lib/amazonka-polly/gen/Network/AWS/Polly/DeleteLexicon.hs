{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.DeleteLexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pronunciation lexicon stored in an AWS Region. A lexicon which has been deleted is not available for speech synthesis, nor is it possible to retrieve it using either the @GetLexicon@ or @ListLexicon@ APIs.
--
-- For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
module Network.AWS.Polly.DeleteLexicon
    (
    -- * Creating a request
      DeleteLexicon (..)
    , mkDeleteLexicon
    -- ** Request lenses
    , dlName

    -- * Destructuring the response
    , DeleteLexiconResponse (..)
    , mkDeleteLexiconResponse
    -- ** Response lenses
    , dlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLexicon' smart constructor.
newtype DeleteLexicon = DeleteLexicon'
  { name :: Types.LexiconName
    -- ^ The name of the lexicon to delete. Must be an existing lexicon in the region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLexicon' value with any optional fields omitted.
mkDeleteLexicon
    :: Types.LexiconName -- ^ 'name'
    -> DeleteLexicon
mkDeleteLexicon name = DeleteLexicon'{name}

-- | The name of the lexicon to delete. Must be an existing lexicon in the region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlName :: Lens.Lens' DeleteLexicon Types.LexiconName
dlName = Lens.field @"name"
{-# INLINEABLE dlName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteLexicon where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLexicon where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLexicon where
        type Rs DeleteLexicon = DeleteLexiconResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/v1/lexicons/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteLexiconResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLexiconResponse' smart constructor.
newtype DeleteLexiconResponse = DeleteLexiconResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLexiconResponse' value with any optional fields omitted.
mkDeleteLexiconResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLexiconResponse
mkDeleteLexiconResponse responseStatus
  = DeleteLexiconResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DeleteLexiconResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
