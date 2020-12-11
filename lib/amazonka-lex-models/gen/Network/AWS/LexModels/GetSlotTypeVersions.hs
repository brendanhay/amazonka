{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotTypeVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all versions of a slot type.
--
-- The @GetSlotTypeVersions@ operation returns a @SlotTypeMetadata@ object for each version of a slot type. For example, if a slot type has three numbered versions, the @GetSlotTypeVersions@ operation returns four @SlotTypeMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
-- The @GetSlotTypeVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetSlotTypeVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetSlotTypeVersions
  ( -- * Creating a request
    GetSlotTypeVersions (..),
    mkGetSlotTypeVersions,

    -- ** Request lenses
    gstvNextToken,
    gstvMaxResults,
    gstvName,

    -- * Destructuring the response
    GetSlotTypeVersionsResponse (..),
    mkGetSlotTypeVersionsResponse,

    -- ** Response lenses
    gstvrsNextToken,
    gstvrsSlotTypes,
    gstvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSlotTypeVersions' smart constructor.
data GetSlotTypeVersions = GetSlotTypeVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSlotTypeVersions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of slot type versions to return in the response. The default is 10.
-- * 'name' - The name of the slot type for which versions should be returned.
-- * 'nextToken' - A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
mkGetSlotTypeVersions ::
  -- | 'name'
  Lude.Text ->
  GetSlotTypeVersions
mkGetSlotTypeVersions pName_ =
  GetSlotTypeVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvNextToken :: Lens.Lens' GetSlotTypeVersions (Lude.Maybe Lude.Text)
gstvNextToken = Lens.lens (nextToken :: GetSlotTypeVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSlotTypeVersions)
{-# DEPRECATED gstvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of slot type versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvMaxResults :: Lens.Lens' GetSlotTypeVersions (Lude.Maybe Lude.Natural)
gstvMaxResults = Lens.lens (maxResults :: GetSlotTypeVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSlotTypeVersions)
{-# DEPRECATED gstvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the slot type for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvName :: Lens.Lens' GetSlotTypeVersions Lude.Text
gstvName = Lens.lens (name :: GetSlotTypeVersions -> Lude.Text) (\s a -> s {name = a} :: GetSlotTypeVersions)
{-# DEPRECATED gstvName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Page.AWSPager GetSlotTypeVersions where
  page rq rs
    | Page.stop (rs Lens.^. gstvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gstvrsSlotTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gstvNextToken Lens..~ rs Lens.^. gstvrsNextToken

instance Lude.AWSRequest GetSlotTypeVersions where
  type Rs GetSlotTypeVersions = GetSlotTypeVersionsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSlotTypeVersionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "slotTypes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSlotTypeVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSlotTypeVersions where
  toPath GetSlotTypeVersions' {..} =
    Lude.mconcat ["/slottypes/", Lude.toBS name, "/versions/"]

instance Lude.ToQuery GetSlotTypeVersions where
  toQuery GetSlotTypeVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkGetSlotTypeVersionsResponse' smart constructor.
data GetSlotTypeVersionsResponse = GetSlotTypeVersionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    slotTypes ::
      Lude.Maybe [SlotTypeMetadata],
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

-- | Creates a value of 'GetSlotTypeVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
-- * 'slotTypes' - An array of @SlotTypeMetadata@ objects, one for each numbered version of the slot type plus one for the @> LATEST@ version.
mkGetSlotTypeVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSlotTypeVersionsResponse
mkGetSlotTypeVersionsResponse pResponseStatus_ =
  GetSlotTypeVersionsResponse'
    { nextToken = Lude.Nothing,
      slotTypes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrsNextToken :: Lens.Lens' GetSlotTypeVersionsResponse (Lude.Maybe Lude.Text)
gstvrsNextToken = Lens.lens (nextToken :: GetSlotTypeVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSlotTypeVersionsResponse)
{-# DEPRECATED gstvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @SlotTypeMetadata@ objects, one for each numbered version of the slot type plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrsSlotTypes :: Lens.Lens' GetSlotTypeVersionsResponse (Lude.Maybe [SlotTypeMetadata])
gstvrsSlotTypes = Lens.lens (slotTypes :: GetSlotTypeVersionsResponse -> Lude.Maybe [SlotTypeMetadata]) (\s a -> s {slotTypes = a} :: GetSlotTypeVersionsResponse)
{-# DEPRECATED gstvrsSlotTypes "Use generic-lens or generic-optics with 'slotTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrsResponseStatus :: Lens.Lens' GetSlotTypeVersionsResponse Lude.Int
gstvrsResponseStatus = Lens.lens (responseStatus :: GetSlotTypeVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSlotTypeVersionsResponse)
{-# DEPRECATED gstvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
