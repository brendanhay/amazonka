{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDeleteMedicalVocabulary' smart constructor.
newtype DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { vocabularyName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMedicalVocabulary' with the minimum fields required to make a request.
--
-- * 'vocabularyName' - The name of the vocabulary that you want to delete.
mkDeleteMedicalVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  DeleteMedicalVocabulary
mkDeleteMedicalVocabulary pVocabularyName_ =
  DeleteMedicalVocabulary' {vocabularyName = pVocabularyName_}

-- | The name of the vocabulary that you want to delete.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmvVocabularyName :: Lens.Lens' DeleteMedicalVocabulary Lude.Text
dmvVocabularyName = Lens.lens (vocabularyName :: DeleteMedicalVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: DeleteMedicalVocabulary)
{-# DEPRECATED dmvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Lude.AWSRequest DeleteMedicalVocabulary where
  type Rs DeleteMedicalVocabulary = DeleteMedicalVocabularyResponse
  request = Req.postJSON transcribeService
  response = Res.receiveNull DeleteMedicalVocabularyResponse'

instance Lude.ToHeaders DeleteMedicalVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DeleteMedicalVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyName" Lude..= vocabularyName)]
      )

instance Lude.ToPath DeleteMedicalVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMedicalVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMedicalVocabularyResponse' with the minimum fields required to make a request.
mkDeleteMedicalVocabularyResponse ::
  DeleteMedicalVocabularyResponse
mkDeleteMedicalVocabularyResponse =
  DeleteMedicalVocabularyResponse'
