{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.JobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.JobDetails
  ( JobDetails (..),

    -- * Smart constructor
    mkJobDetails,

    -- * Lenses
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,
    jdTranslatedDocumentsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of documents successfully and unsuccessfully processed during a translation job.
--
-- /See:/ 'mkJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | The number of documents that could not be processed during a translation job.
    documentsWithErrorsCount :: Core.Maybe Core.Int,
    -- | The number of documents used as input in a translation job.
    inputDocumentsCount :: Core.Maybe Core.Int,
    -- | The number of documents successfully processed during a translation job.
    translatedDocumentsCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDetails' value with any optional fields omitted.
mkJobDetails ::
  JobDetails
mkJobDetails =
  JobDetails'
    { documentsWithErrorsCount = Core.Nothing,
      inputDocumentsCount = Core.Nothing,
      translatedDocumentsCount = Core.Nothing
    }

-- | The number of documents that could not be processed during a translation job.
--
-- /Note:/ Consider using 'documentsWithErrorsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDocumentsWithErrorsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jdDocumentsWithErrorsCount = Lens.field @"documentsWithErrorsCount"
{-# DEPRECATED jdDocumentsWithErrorsCount "Use generic-lens or generic-optics with 'documentsWithErrorsCount' instead." #-}

-- | The number of documents used as input in a translation job.
--
-- /Note:/ Consider using 'inputDocumentsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdInputDocumentsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jdInputDocumentsCount = Lens.field @"inputDocumentsCount"
{-# DEPRECATED jdInputDocumentsCount "Use generic-lens or generic-optics with 'inputDocumentsCount' instead." #-}

-- | The number of documents successfully processed during a translation job.
--
-- /Note:/ Consider using 'translatedDocumentsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTranslatedDocumentsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jdTranslatedDocumentsCount = Lens.field @"translatedDocumentsCount"
{-# DEPRECATED jdTranslatedDocumentsCount "Use generic-lens or generic-optics with 'translatedDocumentsCount' instead." #-}

instance Core.FromJSON JobDetails where
  parseJSON =
    Core.withObject "JobDetails" Core.$
      \x ->
        JobDetails'
          Core.<$> (x Core..:? "DocumentsWithErrorsCount")
          Core.<*> (x Core..:? "InputDocumentsCount")
          Core.<*> (x Core..:? "TranslatedDocumentsCount")
