{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteLexicon (..),
    mkDeleteLexicon,

    -- ** Request lenses
    dlName,

    -- * Destructuring the response
    DeleteLexiconResponse (..),
    mkDeleteLexiconResponse,

    -- ** Response lenses
    dlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLexicon' smart constructor.
newtype DeleteLexicon = DeleteLexicon' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLexicon' with the minimum fields required to make a request.
--
-- * 'name' - The name of the lexicon to delete. Must be an existing lexicon in the region.
mkDeleteLexicon ::
  -- | 'name'
  Lude.Text ->
  DeleteLexicon
mkDeleteLexicon pName_ = DeleteLexicon' {name = pName_}

-- | The name of the lexicon to delete. Must be an existing lexicon in the region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlName :: Lens.Lens' DeleteLexicon Lude.Text
dlName = Lens.lens (name :: DeleteLexicon -> Lude.Text) (\s a -> s {name = a} :: DeleteLexicon)
{-# DEPRECATED dlName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteLexicon where
  type Rs DeleteLexicon = DeleteLexiconResponse
  request = Req.delete pollyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteLexiconResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLexicon where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLexicon where
  toPath DeleteLexicon' {..} =
    Lude.mconcat ["/v1/lexicons/", Lude.toBS name]

instance Lude.ToQuery DeleteLexicon where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLexiconResponse' smart constructor.
newtype DeleteLexiconResponse = DeleteLexiconResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLexiconResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLexiconResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLexiconResponse
mkDeleteLexiconResponse pResponseStatus_ =
  DeleteLexiconResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DeleteLexiconResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DeleteLexiconResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLexiconResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
