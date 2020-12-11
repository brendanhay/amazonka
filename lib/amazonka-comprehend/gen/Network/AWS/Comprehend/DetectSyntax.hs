{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectSyntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for syntax and the part of speech of words in the document. For more information, 'how-syntax' .
module Network.AWS.Comprehend.DetectSyntax
  ( -- * Creating a request
    DetectSyntax (..),
    mkDetectSyntax,

    -- ** Request lenses
    detText,
    detLanguageCode,

    -- * Destructuring the response
    DetectSyntaxResponse (..),
    mkDetectSyntaxResponse,

    -- ** Response lenses
    dsrsSyntaxTokens,
    dsrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectSyntax' smart constructor.
data DetectSyntax = DetectSyntax'
  { text :: Lude.Sensitive Lude.Text,
    languageCode :: SyntaxLanguageCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectSyntax' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
-- * 'text' - A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
mkDetectSyntax ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  -- | 'languageCode'
  SyntaxLanguageCode ->
  DetectSyntax
mkDetectSyntax pText_ pLanguageCode_ =
  DetectSyntax' {text = pText_, languageCode = pLanguageCode_}

-- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detText :: Lens.Lens' DetectSyntax (Lude.Sensitive Lude.Text)
detText = Lens.lens (text :: DetectSyntax -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: DetectSyntax)
{-# DEPRECATED detText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLanguageCode :: Lens.Lens' DetectSyntax SyntaxLanguageCode
detLanguageCode = Lens.lens (languageCode :: DetectSyntax -> SyntaxLanguageCode) (\s a -> s {languageCode = a} :: DetectSyntax)
{-# DEPRECATED detLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest DetectSyntax where
  type Rs DetectSyntax = DetectSyntaxResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectSyntaxResponse'
            Lude.<$> (x Lude..?> "SyntaxTokens" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectSyntax where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectSyntax" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectSyntax where
  toJSON DetectSyntax' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Text" Lude..= text),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath DetectSyntax where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectSyntax where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectSyntaxResponse' smart constructor.
data DetectSyntaxResponse = DetectSyntaxResponse'
  { syntaxTokens ::
      Lude.Maybe [SyntaxToken],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectSyntaxResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'syntaxTokens' - A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
mkDetectSyntaxResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectSyntaxResponse
mkDetectSyntaxResponse pResponseStatus_ =
  DetectSyntaxResponse'
    { syntaxTokens = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
--
-- /Note:/ Consider using 'syntaxTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSyntaxTokens :: Lens.Lens' DetectSyntaxResponse (Lude.Maybe [SyntaxToken])
dsrsSyntaxTokens = Lens.lens (syntaxTokens :: DetectSyntaxResponse -> Lude.Maybe [SyntaxToken]) (\s a -> s {syntaxTokens = a} :: DetectSyntaxResponse)
{-# DEPRECATED dsrsSyntaxTokens "Use generic-lens or generic-optics with 'syntaxTokens' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DetectSyntaxResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DetectSyntaxResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectSyntaxResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
