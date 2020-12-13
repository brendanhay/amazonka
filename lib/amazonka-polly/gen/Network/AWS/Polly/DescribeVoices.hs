{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.DescribeVoices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name.
--
-- When synthesizing speech ( @SynthesizeSpeech@ ), you provide the voice ID for the voice you want from the list of voices returned by @DescribeVoices@ .
-- For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the @DescribeVoices@ operation you can provide the user with a list of available voices to select from.
-- You can optionally specify a language code to filter the available voices. For example, if you specify @en-US@ , the operation returns a list of all available US English voices.
-- This operation requires permissions to perform the @polly:DescribeVoices@ action.
--
-- This operation returns paginated results.
module Network.AWS.Polly.DescribeVoices
  ( -- * Creating a request
    DescribeVoices (..),
    mkDescribeVoices,

    -- ** Request lenses
    dvLanguageCode,
    dvEngine,
    dvNextToken,
    dvIncludeAdditionalLanguageCodes,

    -- * Destructuring the response
    DescribeVoicesResponse (..),
    mkDescribeVoicesResponse,

    -- ** Response lenses
    dvrsNextToken,
    dvrsVoices,
    dvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVoices' smart constructor.
data DescribeVoices = DescribeVoices'
  { -- | The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Specifies the engine (@standard@ or @neural@ ) used by Amazon Polly when processing input text for speech synthesis.
    engine :: Lude.Maybe Engine,
    -- | An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Boolean value indicating whether to return any bilingual voices that use the specified language as an additional language. For instance, if you request all languages that use US English (es-US), and there is an Italian voice that speaks both Italian (it-IT) and US English, that voice will be included if you specify @yes@ but not if you specify @no@ .
    includeAdditionalLanguageCodes :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVoices' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned.
-- * 'engine' - Specifies the engine (@standard@ or @neural@ ) used by Amazon Polly when processing input text for speech synthesis.
-- * 'nextToken' - An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
-- * 'includeAdditionalLanguageCodes' - Boolean value indicating whether to return any bilingual voices that use the specified language as an additional language. For instance, if you request all languages that use US English (es-US), and there is an Italian voice that speaks both Italian (it-IT) and US English, that voice will be included if you specify @yes@ but not if you specify @no@ .
mkDescribeVoices ::
  DescribeVoices
mkDescribeVoices =
  DescribeVoices'
    { languageCode = Lude.Nothing,
      engine = Lude.Nothing,
      nextToken = Lude.Nothing,
      includeAdditionalLanguageCodes = Lude.Nothing
    }

-- | The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvLanguageCode :: Lens.Lens' DescribeVoices (Lude.Maybe LanguageCode)
dvLanguageCode = Lens.lens (languageCode :: DescribeVoices -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: DescribeVoices)
{-# DEPRECATED dvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specifies the engine (@standard@ or @neural@ ) used by Amazon Polly when processing input text for speech synthesis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvEngine :: Lens.Lens' DescribeVoices (Lude.Maybe Engine)
dvEngine = Lens.lens (engine :: DescribeVoices -> Lude.Maybe Engine) (\s a -> s {engine = a} :: DescribeVoices)
{-# DEPRECATED dvEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvNextToken :: Lens.Lens' DescribeVoices (Lude.Maybe Lude.Text)
dvNextToken = Lens.lens (nextToken :: DescribeVoices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVoices)
{-# DEPRECATED dvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Boolean value indicating whether to return any bilingual voices that use the specified language as an additional language. For instance, if you request all languages that use US English (es-US), and there is an Italian voice that speaks both Italian (it-IT) and US English, that voice will be included if you specify @yes@ but not if you specify @no@ .
--
-- /Note:/ Consider using 'includeAdditionalLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvIncludeAdditionalLanguageCodes :: Lens.Lens' DescribeVoices (Lude.Maybe Lude.Bool)
dvIncludeAdditionalLanguageCodes = Lens.lens (includeAdditionalLanguageCodes :: DescribeVoices -> Lude.Maybe Lude.Bool) (\s a -> s {includeAdditionalLanguageCodes = a} :: DescribeVoices)
{-# DEPRECATED dvIncludeAdditionalLanguageCodes "Use generic-lens or generic-optics with 'includeAdditionalLanguageCodes' instead." #-}

instance Page.AWSPager DescribeVoices where
  page rq rs
    | Page.stop (rs Lens.^. dvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvrsVoices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvNextToken Lens..~ rs Lens.^. dvrsNextToken

instance Lude.AWSRequest DescribeVoices where
  type Rs DescribeVoices = DescribeVoicesResponse
  request = Req.get pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVoicesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Voices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVoices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVoices where
  toPath = Lude.const "/v1/voices"

instance Lude.ToQuery DescribeVoices where
  toQuery DescribeVoices' {..} =
    Lude.mconcat
      [ "LanguageCode" Lude.=: languageCode,
        "Engine" Lude.=: engine,
        "NextToken" Lude.=: nextToken,
        "IncludeAdditionalLanguageCodes"
          Lude.=: includeAdditionalLanguageCodes
      ]

-- | /See:/ 'mkDescribeVoicesResponse' smart constructor.
data DescribeVoicesResponse = DescribeVoicesResponse'
  { -- | The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of voices with their properties.
    voices :: Lude.Maybe [Voice],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVoicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
-- * 'voices' - A list of voices with their properties.
-- * 'responseStatus' - The response status code.
mkDescribeVoicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVoicesResponse
mkDescribeVoicesResponse pResponseStatus_ =
  DescribeVoicesResponse'
    { nextToken = Lude.Nothing,
      voices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsNextToken :: Lens.Lens' DescribeVoicesResponse (Lude.Maybe Lude.Text)
dvrsNextToken = Lens.lens (nextToken :: DescribeVoicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVoicesResponse)
{-# DEPRECATED dvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of voices with their properties.
--
-- /Note:/ Consider using 'voices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsVoices :: Lens.Lens' DescribeVoicesResponse (Lude.Maybe [Voice])
dvrsVoices = Lens.lens (voices :: DescribeVoicesResponse -> Lude.Maybe [Voice]) (\s a -> s {voices = a} :: DescribeVoicesResponse)
{-# DEPRECATED dvrsVoices "Use generic-lens or generic-optics with 'voices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsResponseStatus :: Lens.Lens' DescribeVoicesResponse Lude.Int
dvrsResponseStatus = Lens.lens (responseStatus :: DescribeVoicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVoicesResponse)
{-# DEPRECATED dvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
