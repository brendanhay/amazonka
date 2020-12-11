{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteVocabulary (..),
    mkDeleteVocabulary,

    -- ** Request lenses
    dvVocabularyName,

    -- * Destructuring the response
    DeleteVocabularyResponse (..),
    mkDeleteVocabularyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDeleteVocabulary' smart constructor.
newtype DeleteVocabulary = DeleteVocabulary'
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

-- | Creates a value of 'DeleteVocabulary' with the minimum fields required to make a request.
--
-- * 'vocabularyName' - The name of the vocabulary to delete.
mkDeleteVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  DeleteVocabulary
mkDeleteVocabulary pVocabularyName_ =
  DeleteVocabulary' {vocabularyName = pVocabularyName_}

-- | The name of the vocabulary to delete.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVocabularyName :: Lens.Lens' DeleteVocabulary Lude.Text
dvVocabularyName = Lens.lens (vocabularyName :: DeleteVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: DeleteVocabulary)
{-# DEPRECATED dvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Lude.AWSRequest DeleteVocabulary where
  type Rs DeleteVocabulary = DeleteVocabularyResponse
  request = Req.postJSON transcribeService
  response = Res.receiveNull DeleteVocabularyResponse'

instance Lude.ToHeaders DeleteVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DeleteVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVocabulary where
  toJSON DeleteVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyName" Lude..= vocabularyName)]
      )

instance Lude.ToPath DeleteVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVocabularyResponse' with the minimum fields required to make a request.
mkDeleteVocabularyResponse ::
  DeleteVocabularyResponse
mkDeleteVocabularyResponse = DeleteVocabularyResponse'
