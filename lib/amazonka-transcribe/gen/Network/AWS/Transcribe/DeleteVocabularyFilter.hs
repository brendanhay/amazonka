{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a vocabulary filter.
module Network.AWS.Transcribe.DeleteVocabularyFilter
  ( -- * Creating a request
    DeleteVocabularyFilter (..),
    mkDeleteVocabularyFilter,

    -- ** Request lenses
    dvfVocabularyFilterName,

    -- * Destructuring the response
    DeleteVocabularyFilterResponse (..),
    mkDeleteVocabularyFilterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDeleteVocabularyFilter' smart constructor.
newtype DeleteVocabularyFilter = DeleteVocabularyFilter'
  { vocabularyFilterName ::
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

-- | Creates a value of 'DeleteVocabularyFilter' with the minimum fields required to make a request.
--
-- * 'vocabularyFilterName' - The name of the vocabulary filter to remove.
mkDeleteVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Lude.Text ->
  DeleteVocabularyFilter
mkDeleteVocabularyFilter pVocabularyFilterName_ =
  DeleteVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter to remove.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfVocabularyFilterName :: Lens.Lens' DeleteVocabularyFilter Lude.Text
dvfVocabularyFilterName = Lens.lens (vocabularyFilterName :: DeleteVocabularyFilter -> Lude.Text) (\s a -> s {vocabularyFilterName = a} :: DeleteVocabularyFilter)
{-# DEPRECATED dvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

instance Lude.AWSRequest DeleteVocabularyFilter where
  type Rs DeleteVocabularyFilter = DeleteVocabularyFilterResponse
  request = Req.postJSON transcribeService
  response = Res.receiveNull DeleteVocabularyFilterResponse'

instance Lude.ToHeaders DeleteVocabularyFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DeleteVocabularyFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVocabularyFilter where
  toJSON DeleteVocabularyFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyFilterName" Lude..= vocabularyFilterName)]
      )

instance Lude.ToPath DeleteVocabularyFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVocabularyFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVocabularyFilterResponse' smart constructor.
data DeleteVocabularyFilterResponse = DeleteVocabularyFilterResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVocabularyFilterResponse' with the minimum fields required to make a request.
mkDeleteVocabularyFilterResponse ::
  DeleteVocabularyFilterResponse
mkDeleteVocabularyFilterResponse = DeleteVocabularyFilterResponse'
