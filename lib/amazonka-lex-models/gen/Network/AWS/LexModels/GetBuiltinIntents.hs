{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinIntents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in intents that meet the specified criteria.
--
-- This operation requires permission for the @lex:GetBuiltinIntents@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinIntents
  ( -- * Creating a request
    GetBuiltinIntents (..),
    mkGetBuiltinIntents,

    -- ** Request lenses
    gbiLocale,
    gbiNextToken,
    gbiSignatureContains,
    gbiMaxResults,

    -- * Destructuring the response
    GetBuiltinIntentsResponse (..),
    mkGetBuiltinIntentsResponse,

    -- ** Response lenses
    grsIntents,
    grsNextToken,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBuiltinIntents' smart constructor.
data GetBuiltinIntents = GetBuiltinIntents'
  { locale ::
      Lude.Maybe Locale,
    nextToken :: Lude.Maybe Lude.Text,
    signatureContains :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBuiltinIntents' with the minimum fields required to make a request.
--
-- * 'locale' - A list of locales that the intent supports.
-- * 'maxResults' - The maximum number of intents to return in the response. The default is 10.
-- * 'nextToken' - A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
-- * 'signatureContains' - Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
mkGetBuiltinIntents ::
  GetBuiltinIntents
mkGetBuiltinIntents =
  GetBuiltinIntents'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      signatureContains = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of locales that the intent supports.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiLocale :: Lens.Lens' GetBuiltinIntents (Lude.Maybe Locale)
gbiLocale = Lens.lens (locale :: GetBuiltinIntents -> Lude.Maybe Locale) (\s a -> s {locale = a} :: GetBuiltinIntents)
{-# DEPRECATED gbiLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiNextToken :: Lens.Lens' GetBuiltinIntents (Lude.Maybe Lude.Text)
gbiNextToken = Lens.lens (nextToken :: GetBuiltinIntents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBuiltinIntents)
{-# DEPRECATED gbiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signatureContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiSignatureContains :: Lens.Lens' GetBuiltinIntents (Lude.Maybe Lude.Text)
gbiSignatureContains = Lens.lens (signatureContains :: GetBuiltinIntents -> Lude.Maybe Lude.Text) (\s a -> s {signatureContains = a} :: GetBuiltinIntents)
{-# DEPRECATED gbiSignatureContains "Use generic-lens or generic-optics with 'signatureContains' instead." #-}

-- | The maximum number of intents to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiMaxResults :: Lens.Lens' GetBuiltinIntents (Lude.Maybe Lude.Natural)
gbiMaxResults = Lens.lens (maxResults :: GetBuiltinIntents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBuiltinIntents)
{-# DEPRECATED gbiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetBuiltinIntents where
  page rq rs
    | Page.stop (rs Lens.^. grsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grsIntents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbiNextToken Lens..~ rs Lens.^. grsNextToken

instance Lude.AWSRequest GetBuiltinIntents where
  type Rs GetBuiltinIntents = GetBuiltinIntentsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBuiltinIntentsResponse'
            Lude.<$> (x Lude..?> "intents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBuiltinIntents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBuiltinIntents where
  toPath = Lude.const "/builtins/intents/"

instance Lude.ToQuery GetBuiltinIntents where
  toQuery GetBuiltinIntents' {..} =
    Lude.mconcat
      [ "locale" Lude.=: locale,
        "nextToken" Lude.=: nextToken,
        "signatureContains" Lude.=: signatureContains,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetBuiltinIntentsResponse' smart constructor.
data GetBuiltinIntentsResponse = GetBuiltinIntentsResponse'
  { intents ::
      Lude.Maybe [BuiltinIntentMetadata],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetBuiltinIntentsResponse' with the minimum fields required to make a request.
--
-- * 'intents' - An array of @builtinIntentMetadata@ objects, one for each intent in the response.
-- * 'nextToken' - A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
mkGetBuiltinIntentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBuiltinIntentsResponse
mkGetBuiltinIntentsResponse pResponseStatus_ =
  GetBuiltinIntentsResponse'
    { intents = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @builtinIntentMetadata@ objects, one for each intent in the response.
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsIntents :: Lens.Lens' GetBuiltinIntentsResponse (Lude.Maybe [BuiltinIntentMetadata])
grsIntents = Lens.lens (intents :: GetBuiltinIntentsResponse -> Lude.Maybe [BuiltinIntentMetadata]) (\s a -> s {intents = a} :: GetBuiltinIntentsResponse)
{-# DEPRECATED grsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextToken :: Lens.Lens' GetBuiltinIntentsResponse (Lude.Maybe Lude.Text)
grsNextToken = Lens.lens (nextToken :: GetBuiltinIntentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBuiltinIntentsResponse)
{-# DEPRECATED grsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetBuiltinIntentsResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetBuiltinIntentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBuiltinIntentsResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
