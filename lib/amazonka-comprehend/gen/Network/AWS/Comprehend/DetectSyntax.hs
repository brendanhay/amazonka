{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dsfText,
    dsfLanguageCode,

    -- * Destructuring the response
    DetectSyntaxResponse (..),
    mkDetectSyntaxResponse,

    -- ** Response lenses
    dsrrsSyntaxTokens,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectSyntax' smart constructor.
data DetectSyntax = DetectSyntax'
  { -- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
    text :: Types.CustomerInputString,
    -- | The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
    languageCode :: Types.SyntaxLanguageCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectSyntax' value with any optional fields omitted.
mkDetectSyntax ::
  -- | 'text'
  Types.CustomerInputString ->
  -- | 'languageCode'
  Types.SyntaxLanguageCode ->
  DetectSyntax
mkDetectSyntax text languageCode =
  DetectSyntax' {text, languageCode}

-- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfText :: Lens.Lens' DetectSyntax Types.CustomerInputString
dsfText = Lens.field @"text"
{-# DEPRECATED dsfText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLanguageCode :: Lens.Lens' DetectSyntax Types.SyntaxLanguageCode
dsfLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED dsfLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Core.FromJSON DetectSyntax where
  toJSON DetectSyntax {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.AWSRequest DetectSyntax where
  type Rs DetectSyntax = DetectSyntaxResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.DetectSyntax")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectSyntaxResponse'
            Core.<$> (x Core..:? "SyntaxTokens") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectSyntaxResponse' smart constructor.
data DetectSyntaxResponse = DetectSyntaxResponse'
  { -- | A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
    syntaxTokens :: Core.Maybe [Types.SyntaxToken],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectSyntaxResponse' value with any optional fields omitted.
mkDetectSyntaxResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectSyntaxResponse
mkDetectSyntaxResponse responseStatus =
  DetectSyntaxResponse'
    { syntaxTokens = Core.Nothing,
      responseStatus
    }

-- | A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
--
-- /Note:/ Consider using 'syntaxTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSyntaxTokens :: Lens.Lens' DetectSyntaxResponse (Core.Maybe [Types.SyntaxToken])
dsrrsSyntaxTokens = Lens.field @"syntaxTokens"
{-# DEPRECATED dsrrsSyntaxTokens "Use generic-lens or generic-optics with 'syntaxTokens' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DetectSyntaxResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
