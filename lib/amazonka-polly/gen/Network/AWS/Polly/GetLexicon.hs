{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.GetLexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
module Network.AWS.Polly.GetLexicon
  ( -- * Creating a request
    GetLexicon (..),
    mkGetLexicon,

    -- ** Request lenses
    glName,

    -- * Destructuring the response
    GetLexiconResponse (..),
    mkGetLexiconResponse,

    -- ** Response lenses
    glrsLexiconAttributes,
    glrsLexicon,
    glrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLexicon' smart constructor.
newtype GetLexicon = GetLexicon' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLexicon' with the minimum fields required to make a request.
--
-- * 'name' - Name of the lexicon.
mkGetLexicon ::
  -- | 'name'
  Lude.Text ->
  GetLexicon
mkGetLexicon pName_ = GetLexicon' {name = pName_}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glName :: Lens.Lens' GetLexicon Lude.Text
glName = Lens.lens (name :: GetLexicon -> Lude.Text) (\s a -> s {name = a} :: GetLexicon)
{-# DEPRECATED glName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetLexicon where
  type Rs GetLexicon = GetLexiconResponse
  request = Req.get pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLexiconResponse'
            Lude.<$> (x Lude..?> "LexiconAttributes")
            Lude.<*> (x Lude..?> "Lexicon")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLexicon where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLexicon where
  toPath GetLexicon' {..} =
    Lude.mconcat ["/v1/lexicons/", Lude.toBS name]

instance Lude.ToQuery GetLexicon where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLexiconResponse' smart constructor.
data GetLexiconResponse = GetLexiconResponse'
  { lexiconAttributes ::
      Lude.Maybe LexiconAttributes,
    lexicon :: Lude.Maybe Lexicon,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLexiconResponse' with the minimum fields required to make a request.
--
-- * 'lexicon' - Lexicon object that provides name and the string content of the lexicon.
-- * 'lexiconAttributes' - Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
-- * 'responseStatus' - The response status code.
mkGetLexiconResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLexiconResponse
mkGetLexiconResponse pResponseStatus_ =
  GetLexiconResponse'
    { lexiconAttributes = Lude.Nothing,
      lexicon = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
--
-- /Note:/ Consider using 'lexiconAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrsLexiconAttributes :: Lens.Lens' GetLexiconResponse (Lude.Maybe LexiconAttributes)
glrsLexiconAttributes = Lens.lens (lexiconAttributes :: GetLexiconResponse -> Lude.Maybe LexiconAttributes) (\s a -> s {lexiconAttributes = a} :: GetLexiconResponse)
{-# DEPRECATED glrsLexiconAttributes "Use generic-lens or generic-optics with 'lexiconAttributes' instead." #-}

-- | Lexicon object that provides name and the string content of the lexicon.
--
-- /Note:/ Consider using 'lexicon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrsLexicon :: Lens.Lens' GetLexiconResponse (Lude.Maybe Lexicon)
glrsLexicon = Lens.lens (lexicon :: GetLexiconResponse -> Lude.Maybe Lexicon) (\s a -> s {lexicon = a} :: GetLexiconResponse)
{-# DEPRECATED glrsLexicon "Use generic-lens or generic-optics with 'lexicon' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrsResponseStatus :: Lens.Lens' GetLexiconResponse Lude.Int
glrsResponseStatus = Lens.lens (responseStatus :: GetLexiconResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLexiconResponse)
{-# DEPRECATED glrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
