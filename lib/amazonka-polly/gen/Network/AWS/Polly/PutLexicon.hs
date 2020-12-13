{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutLexicon (..),
    mkPutLexicon,

    -- ** Request lenses
    plContent,
    plName,

    -- * Destructuring the response
    PutLexiconResponse (..),
    mkPutLexiconResponse,

    -- ** Response lenses
    plrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { -- | Content of the PLS lexicon as string data.
    content :: Lude.Sensitive Lude.Text,
    -- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLexicon' with the minimum fields required to make a request.
--
-- * 'content' - Content of the PLS lexicon as string data.
-- * 'name' - Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
mkPutLexicon ::
  -- | 'content'
  Lude.Sensitive Lude.Text ->
  -- | 'name'
  Lude.Text ->
  PutLexicon
mkPutLexicon pContent_ pName_ =
  PutLexicon' {content = pContent_, name = pName_}

-- | Content of the PLS lexicon as string data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plContent :: Lens.Lens' PutLexicon (Lude.Sensitive Lude.Text)
plContent = Lens.lens (content :: PutLexicon -> Lude.Sensitive Lude.Text) (\s a -> s {content = a} :: PutLexicon)
{-# DEPRECATED plContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plName :: Lens.Lens' PutLexicon Lude.Text
plName = Lens.lens (name :: PutLexicon -> Lude.Text) (\s a -> s {name = a} :: PutLexicon)
{-# DEPRECATED plName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest PutLexicon where
  type Rs PutLexicon = PutLexiconResponse
  request = Req.putJSON pollyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutLexiconResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLexicon where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutLexicon where
  toJSON PutLexicon' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Content" Lude..= content)])

instance Lude.ToPath PutLexicon where
  toPath PutLexicon' {..} =
    Lude.mconcat ["/v1/lexicons/", Lude.toBS name]

instance Lude.ToQuery PutLexicon where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLexiconResponse' smart constructor.
newtype PutLexiconResponse = PutLexiconResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLexiconResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutLexiconResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLexiconResponse
mkPutLexiconResponse pResponseStatus_ =
  PutLexiconResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plrsResponseStatus :: Lens.Lens' PutLexiconResponse Lude.Int
plrsResponseStatus = Lens.lens (responseStatus :: PutLexiconResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLexiconResponse)
{-# DEPRECATED plrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
