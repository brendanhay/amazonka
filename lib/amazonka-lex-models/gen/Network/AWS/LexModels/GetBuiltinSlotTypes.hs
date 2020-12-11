{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinSlotTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in slot types that meet the specified criteria.
--
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
-- This operation requires permission for the @lex:GetBuiltInSlotTypes@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinSlotTypes
  ( -- * Creating a request
    GetBuiltinSlotTypes (..),
    mkGetBuiltinSlotTypes,

    -- ** Request lenses
    gbstLocale,
    gbstNextToken,
    gbstSignatureContains,
    gbstMaxResults,

    -- * Destructuring the response
    GetBuiltinSlotTypesResponse (..),
    mkGetBuiltinSlotTypesResponse,

    -- ** Response lenses
    gbstrsNextToken,
    gbstrsSlotTypes,
    gbstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBuiltinSlotTypes' smart constructor.
data GetBuiltinSlotTypes = GetBuiltinSlotTypes'
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

-- | Creates a value of 'GetBuiltinSlotTypes' with the minimum fields required to make a request.
--
-- * 'locale' - A list of locales that the slot type supports.
-- * 'maxResults' - The maximum number of slot types to return in the response. The default is 10.
-- * 'nextToken' - A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
-- * 'signatureContains' - Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
mkGetBuiltinSlotTypes ::
  GetBuiltinSlotTypes
mkGetBuiltinSlotTypes =
  GetBuiltinSlotTypes'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      signatureContains = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of locales that the slot type supports.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstLocale :: Lens.Lens' GetBuiltinSlotTypes (Lude.Maybe Locale)
gbstLocale = Lens.lens (locale :: GetBuiltinSlotTypes -> Lude.Maybe Locale) (\s a -> s {locale = a} :: GetBuiltinSlotTypes)
{-# DEPRECATED gbstLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstNextToken :: Lens.Lens' GetBuiltinSlotTypes (Lude.Maybe Lude.Text)
gbstNextToken = Lens.lens (nextToken :: GetBuiltinSlotTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBuiltinSlotTypes)
{-# DEPRECATED gbstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'signatureContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstSignatureContains :: Lens.Lens' GetBuiltinSlotTypes (Lude.Maybe Lude.Text)
gbstSignatureContains = Lens.lens (signatureContains :: GetBuiltinSlotTypes -> Lude.Maybe Lude.Text) (\s a -> s {signatureContains = a} :: GetBuiltinSlotTypes)
{-# DEPRECATED gbstSignatureContains "Use generic-lens or generic-optics with 'signatureContains' instead." #-}

-- | The maximum number of slot types to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstMaxResults :: Lens.Lens' GetBuiltinSlotTypes (Lude.Maybe Lude.Natural)
gbstMaxResults = Lens.lens (maxResults :: GetBuiltinSlotTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBuiltinSlotTypes)
{-# DEPRECATED gbstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetBuiltinSlotTypes where
  page rq rs
    | Page.stop (rs Lens.^. gbstrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbstrsSlotTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbstNextToken Lens..~ rs Lens.^. gbstrsNextToken

instance Lude.AWSRequest GetBuiltinSlotTypes where
  type Rs GetBuiltinSlotTypes = GetBuiltinSlotTypesResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBuiltinSlotTypesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "slotTypes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBuiltinSlotTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBuiltinSlotTypes where
  toPath = Lude.const "/builtins/slottypes/"

instance Lude.ToQuery GetBuiltinSlotTypes where
  toQuery GetBuiltinSlotTypes' {..} =
    Lude.mconcat
      [ "locale" Lude.=: locale,
        "nextToken" Lude.=: nextToken,
        "signatureContains" Lude.=: signatureContains,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetBuiltinSlotTypesResponse' smart constructor.
data GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    slotTypes ::
      Lude.Maybe
        [BuiltinSlotTypeMetadata],
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

-- | Creates a value of 'GetBuiltinSlotTypesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
-- * 'responseStatus' - The response status code.
-- * 'slotTypes' - An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
mkGetBuiltinSlotTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBuiltinSlotTypesResponse
mkGetBuiltinSlotTypesResponse pResponseStatus_ =
  GetBuiltinSlotTypesResponse'
    { nextToken = Lude.Nothing,
      slotTypes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrsNextToken :: Lens.Lens' GetBuiltinSlotTypesResponse (Lude.Maybe Lude.Text)
gbstrsNextToken = Lens.lens (nextToken :: GetBuiltinSlotTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBuiltinSlotTypesResponse)
{-# DEPRECATED gbstrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrsSlotTypes :: Lens.Lens' GetBuiltinSlotTypesResponse (Lude.Maybe [BuiltinSlotTypeMetadata])
gbstrsSlotTypes = Lens.lens (slotTypes :: GetBuiltinSlotTypesResponse -> Lude.Maybe [BuiltinSlotTypeMetadata]) (\s a -> s {slotTypes = a} :: GetBuiltinSlotTypesResponse)
{-# DEPRECATED gbstrsSlotTypes "Use generic-lens or generic-optics with 'slotTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrsResponseStatus :: Lens.Lens' GetBuiltinSlotTypesResponse Lude.Int
gbstrsResponseStatus = Lens.lens (responseStatus :: GetBuiltinSlotTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBuiltinSlotTypesResponse)
{-# DEPRECATED gbstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
