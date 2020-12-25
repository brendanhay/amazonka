{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteMedicalVocabulary (..),
    mkDeleteMedicalVocabulary,

    -- ** Request lenses
    dmvVocabularyName,

    -- * Destructuring the response
    DeleteMedicalVocabularyResponse (..),
    mkDeleteMedicalVocabularyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDeleteMedicalVocabulary' smart constructor.
newtype DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { -- | The name of the vocabulary that you want to delete.
    vocabularyName :: Types.VocabularyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMedicalVocabulary' value with any optional fields omitted.
mkDeleteMedicalVocabulary ::
  -- | 'vocabularyName'
  Types.VocabularyName ->
  DeleteMedicalVocabulary
mkDeleteMedicalVocabulary vocabularyName =
  DeleteMedicalVocabulary' {vocabularyName}

-- | The name of the vocabulary that you want to delete.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmvVocabularyName :: Lens.Lens' DeleteMedicalVocabulary Types.VocabularyName
dmvVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED dmvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Core.FromJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VocabularyName" Core..= vocabularyName)]
      )

instance Core.AWSRequest DeleteMedicalVocabulary where
  type Rs DeleteMedicalVocabulary = DeleteMedicalVocabularyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.DeleteMedicalVocabulary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteMedicalVocabularyResponse'

-- | /See:/ 'mkDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMedicalVocabularyResponse' value with any optional fields omitted.
mkDeleteMedicalVocabularyResponse ::
  DeleteMedicalVocabularyResponse
mkDeleteMedicalVocabularyResponse =
  DeleteMedicalVocabularyResponse'
