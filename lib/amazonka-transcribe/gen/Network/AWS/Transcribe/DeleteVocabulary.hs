{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe. 
module Network.AWS.Transcribe.DeleteVocabulary
    (
    -- * Creating a request
      DeleteVocabulary (..)
    , mkDeleteVocabulary
    -- ** Request lenses
    , dvVocabularyName

    -- * Destructuring the response
    , DeleteVocabularyResponse (..)
    , mkDeleteVocabularyResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDeleteVocabulary' smart constructor.
newtype DeleteVocabulary = DeleteVocabulary'
  { vocabularyName :: Types.VocabularyName
    -- ^ The name of the vocabulary to delete. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVocabulary' value with any optional fields omitted.
mkDeleteVocabulary
    :: Types.VocabularyName -- ^ 'vocabularyName'
    -> DeleteVocabulary
mkDeleteVocabulary vocabularyName
  = DeleteVocabulary'{vocabularyName}

-- | The name of the vocabulary to delete. 
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVocabularyName :: Lens.Lens' DeleteVocabulary Types.VocabularyName
dvVocabularyName = Lens.field @"vocabularyName"
{-# INLINEABLE dvVocabularyName #-}
{-# DEPRECATED vocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead"  #-}

instance Core.ToQuery DeleteVocabulary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVocabulary where
        toHeaders DeleteVocabulary{..}
          = Core.pure ("X-Amz-Target", "Transcribe.DeleteVocabulary") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteVocabulary where
        toJSON DeleteVocabulary{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VocabularyName" Core..= vocabularyName)])

instance Core.AWSRequest DeleteVocabulary where
        type Rs DeleteVocabulary = DeleteVocabularyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteVocabularyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVocabularyResponse' value with any optional fields omitted.
mkDeleteVocabularyResponse
    :: DeleteVocabularyResponse
mkDeleteVocabularyResponse = DeleteVocabularyResponse'
