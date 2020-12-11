{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns slot type information as follows:
--
--
--     * If you specify the @nameContains@ field, returns the @> LATEST@ version of all slot types that contain the specified string.
--
--
--     * If you don't specify the @nameContains@ field, returns information about the @> LATEST@ version of all slot types.
--
--
-- The operation requires permission for the @lex:GetSlotTypes@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetSlotTypes
  ( -- * Creating a request
    GetSlotTypes (..),
    mkGetSlotTypes,

    -- ** Request lenses
    gstNameContains,
    gstNextToken,
    gstMaxResults,

    -- * Destructuring the response
    GetSlotTypesResponse (..),
    mkGetSlotTypesResponse,

    -- ** Response lenses
    gstrsNextToken,
    gstrsSlotTypes,
    gstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetSlotTypes' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of slot types to return in the response. The default is 10.
-- * 'nameContains' - Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
-- * 'nextToken' - A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
mkGetSlotTypes ::
  GetSlotTypes
mkGetSlotTypes =
  GetSlotTypes'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstNameContains :: Lens.Lens' GetSlotTypes (Lude.Maybe Lude.Text)
gstNameContains = Lens.lens (nameContains :: GetSlotTypes -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: GetSlotTypes)
{-# DEPRECATED gstNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstNextToken :: Lens.Lens' GetSlotTypes (Lude.Maybe Lude.Text)
gstNextToken = Lens.lens (nextToken :: GetSlotTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSlotTypes)
{-# DEPRECATED gstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of slot types to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstMaxResults :: Lens.Lens' GetSlotTypes (Lude.Maybe Lude.Natural)
gstMaxResults = Lens.lens (maxResults :: GetSlotTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSlotTypes)
{-# DEPRECATED gstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetSlotTypes where
  page rq rs
    | Page.stop (rs Lens.^. gstrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gstrsSlotTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gstNextToken Lens..~ rs Lens.^. gstrsNextToken

instance Lude.AWSRequest GetSlotTypes where
  type Rs GetSlotTypes = GetSlotTypesResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSlotTypesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "slotTypes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSlotTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSlotTypes where
  toPath = Lude.const "/slottypes/"

instance Lude.ToQuery GetSlotTypes where
  toQuery GetSlotTypes' {..} =
    Lude.mconcat
      [ "nameContains" Lude.=: nameContains,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    slotTypes :: Lude.Maybe [SlotTypeMetadata],
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

-- | Creates a value of 'GetSlotTypesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
-- * 'responseStatus' - The response status code.
-- * 'slotTypes' - An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
mkGetSlotTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSlotTypesResponse
mkGetSlotTypesResponse pResponseStatus_ =
  GetSlotTypesResponse'
    { nextToken = Lude.Nothing,
      slotTypes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsNextToken :: Lens.Lens' GetSlotTypesResponse (Lude.Maybe Lude.Text)
gstrsNextToken = Lens.lens (nextToken :: GetSlotTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSlotTypesResponse)
{-# DEPRECATED gstrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsSlotTypes :: Lens.Lens' GetSlotTypesResponse (Lude.Maybe [SlotTypeMetadata])
gstrsSlotTypes = Lens.lens (slotTypes :: GetSlotTypesResponse -> Lude.Maybe [SlotTypeMetadata]) (\s a -> s {slotTypes = a} :: GetSlotTypesResponse)
{-# DEPRECATED gstrsSlotTypes "Use generic-lens or generic-optics with 'slotTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsResponseStatus :: Lens.Lens' GetSlotTypesResponse Lude.Int
gstrsResponseStatus = Lens.lens (responseStatus :: GetSlotTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSlotTypesResponse)
{-# DEPRECATED gstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
