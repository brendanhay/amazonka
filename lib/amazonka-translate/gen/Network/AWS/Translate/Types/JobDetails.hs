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
    jdTranslatedDocumentsCount,
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of documents successfully and unsuccessfully processed during a translation job.
--
-- /See:/ 'mkJobDetails' smart constructor.
data JobDetails = JobDetails'
  { translatedDocumentsCount ::
      Lude.Maybe Lude.Int,
    documentsWithErrorsCount :: Lude.Maybe Lude.Int,
    inputDocumentsCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDetails' with the minimum fields required to make a request.
--
-- * 'documentsWithErrorsCount' - The number of documents that could not be processed during a translation job.
-- * 'inputDocumentsCount' - The number of documents used as input in a translation job.
-- * 'translatedDocumentsCount' - The number of documents successfully processed during a translation job.
mkJobDetails ::
  JobDetails
mkJobDetails =
  JobDetails'
    { translatedDocumentsCount = Lude.Nothing,
      documentsWithErrorsCount = Lude.Nothing,
      inputDocumentsCount = Lude.Nothing
    }

-- | The number of documents successfully processed during a translation job.
--
-- /Note:/ Consider using 'translatedDocumentsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTranslatedDocumentsCount :: Lens.Lens' JobDetails (Lude.Maybe Lude.Int)
jdTranslatedDocumentsCount = Lens.lens (translatedDocumentsCount :: JobDetails -> Lude.Maybe Lude.Int) (\s a -> s {translatedDocumentsCount = a} :: JobDetails)
{-# DEPRECATED jdTranslatedDocumentsCount "Use generic-lens or generic-optics with 'translatedDocumentsCount' instead." #-}

-- | The number of documents that could not be processed during a translation job.
--
-- /Note:/ Consider using 'documentsWithErrorsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDocumentsWithErrorsCount :: Lens.Lens' JobDetails (Lude.Maybe Lude.Int)
jdDocumentsWithErrorsCount = Lens.lens (documentsWithErrorsCount :: JobDetails -> Lude.Maybe Lude.Int) (\s a -> s {documentsWithErrorsCount = a} :: JobDetails)
{-# DEPRECATED jdDocumentsWithErrorsCount "Use generic-lens or generic-optics with 'documentsWithErrorsCount' instead." #-}

-- | The number of documents used as input in a translation job.
--
-- /Note:/ Consider using 'inputDocumentsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdInputDocumentsCount :: Lens.Lens' JobDetails (Lude.Maybe Lude.Int)
jdInputDocumentsCount = Lens.lens (inputDocumentsCount :: JobDetails -> Lude.Maybe Lude.Int) (\s a -> s {inputDocumentsCount = a} :: JobDetails)
{-# DEPRECATED jdInputDocumentsCount "Use generic-lens or generic-optics with 'inputDocumentsCount' instead." #-}

instance Lude.FromJSON JobDetails where
  parseJSON =
    Lude.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Lude.<$> (x Lude..:? "TranslatedDocumentsCount")
            Lude.<*> (x Lude..:? "DocumentsWithErrorsCount")
            Lude.<*> (x Lude..:? "InputDocumentsCount")
      )
