{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.PutLexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.
--
-- For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
module Network.AWS.Polly.PutLexicon
    (
    -- * Creating a request
      PutLexicon (..)
    , mkPutLexicon
    -- ** Request lenses
    , plName
    , plContent

    -- * Destructuring the response
    , PutLexiconResponse (..)
    , mkPutLexiconResponse
    -- ** Response lenses
    , plrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { name :: Types.LexiconName
    -- ^ Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long. 
  , content :: Types.Content
    -- ^ Content of the PLS lexicon as string data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLexicon' value with any optional fields omitted.
mkPutLexicon
    :: Types.LexiconName -- ^ 'name'
    -> Types.Content -- ^ 'content'
    -> PutLexicon
mkPutLexicon name content = PutLexicon'{name, content}

-- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plName :: Lens.Lens' PutLexicon Types.LexiconName
plName = Lens.field @"name"
{-# INLINEABLE plName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Content of the PLS lexicon as string data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plContent :: Lens.Lens' PutLexicon Types.Content
plContent = Lens.field @"content"
{-# INLINEABLE plContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

instance Core.ToQuery PutLexicon where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutLexicon where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PutLexicon where
        toJSON PutLexicon{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Content" Core..= content)])

instance Core.AWSRequest PutLexicon where
        type Rs PutLexicon = PutLexiconResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/v1/lexicons/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutLexiconResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutLexiconResponse' smart constructor.
newtype PutLexiconResponse = PutLexiconResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutLexiconResponse' value with any optional fields omitted.
mkPutLexiconResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutLexiconResponse
mkPutLexiconResponse responseStatus
  = PutLexiconResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plrrsResponseStatus :: Lens.Lens' PutLexiconResponse Core.Int
plrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE plrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
