{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe Medical.
module Network.AWS.Transcribe.DeleteMedicalVocabulary
    (
    -- * Creating a request
      DeleteMedicalVocabulary (..)
    , mkDeleteMedicalVocabulary
    -- ** Request lenses
    , dmvVocabularyName

    -- * Destructuring the response
    , DeleteMedicalVocabularyResponse (..)
    , mkDeleteMedicalVocabularyResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDeleteMedicalVocabulary' smart constructor.
newtype DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { vocabularyName :: Types.VocabularyName
    -- ^ The name of the vocabulary that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMedicalVocabulary' value with any optional fields omitted.
mkDeleteMedicalVocabulary
    :: Types.VocabularyName -- ^ 'vocabularyName'
    -> DeleteMedicalVocabulary
mkDeleteMedicalVocabulary vocabularyName
  = DeleteMedicalVocabulary'{vocabularyName}

-- | The name of the vocabulary that you want to delete.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmvVocabularyName :: Lens.Lens' DeleteMedicalVocabulary Types.VocabularyName
dmvVocabularyName = Lens.field @"vocabularyName"
{-# INLINEABLE dmvVocabularyName #-}
{-# DEPRECATED vocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead"  #-}

instance Core.ToQuery DeleteMedicalVocabulary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMedicalVocabulary where
        toHeaders DeleteMedicalVocabulary{..}
          = Core.pure ("X-Amz-Target", "Transcribe.DeleteMedicalVocabulary")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteMedicalVocabulary where
        toJSON DeleteMedicalVocabulary{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VocabularyName" Core..= vocabularyName)])

instance Core.AWSRequest DeleteMedicalVocabulary where
        type Rs DeleteMedicalVocabulary = DeleteMedicalVocabularyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteMedicalVocabularyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMedicalVocabularyResponse' value with any optional fields omitted.
mkDeleteMedicalVocabularyResponse
    :: DeleteMedicalVocabularyResponse
mkDeleteMedicalVocabularyResponse
  = DeleteMedicalVocabularyResponse'
