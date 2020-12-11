{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListHITsForQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITsForQualificationType@ operation returns the HITs that use the given Qualification type for a Qualification requirement. The operation returns HITs of any status, except for HITs that have been deleted with the @DeleteHIT@ operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITsForQualificationType
  ( -- * Creating a request
    ListHITsForQualificationType (..),
    mkListHITsForQualificationType,

    -- ** Request lenses
    lhitfqtNextToken,
    lhitfqtMaxResults,
    lhitfqtQualificationTypeId,

    -- * Destructuring the response
    ListHITsForQualificationTypeResponse (..),
    mkListHITsForQualificationTypeResponse,

    -- ** Response lenses
    lhitfqtrsNextToken,
    lhitfqtrsNumResults,
    lhitfqtrsHITs,
    lhitfqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListHITsForQualificationType' smart constructor.
data ListHITsForQualificationType = ListHITsForQualificationType'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    qualificationTypeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHITsForQualificationType' with the minimum fields required to make a request.
--
-- * 'maxResults' - Limit the number of results returned.
-- * 'nextToken' - Pagination Token
-- * 'qualificationTypeId' - The ID of the Qualification type to use when querying HITs.
mkListHITsForQualificationType ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  ListHITsForQualificationType
mkListHITsForQualificationType pQualificationTypeId_ =
  ListHITsForQualificationType'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | Pagination Token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtNextToken :: Lens.Lens' ListHITsForQualificationType (Lude.Maybe Lude.Text)
lhitfqtNextToken = Lens.lens (nextToken :: ListHITsForQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHITsForQualificationType)
{-# DEPRECATED lhitfqtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtMaxResults :: Lens.Lens' ListHITsForQualificationType (Lude.Maybe Lude.Natural)
lhitfqtMaxResults = Lens.lens (maxResults :: ListHITsForQualificationType -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHITsForQualificationType)
{-# DEPRECATED lhitfqtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Qualification type to use when querying HITs.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtQualificationTypeId :: Lens.Lens' ListHITsForQualificationType Lude.Text
lhitfqtQualificationTypeId = Lens.lens (qualificationTypeId :: ListHITsForQualificationType -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: ListHITsForQualificationType)
{-# DEPRECATED lhitfqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Page.AWSPager ListHITsForQualificationType where
  page rq rs
    | Page.stop (rs Lens.^. lhitfqtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhitfqtrsHITs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhitfqtNextToken Lens..~ rs Lens.^. lhitfqtrsNextToken

instance Lude.AWSRequest ListHITsForQualificationType where
  type
    Rs ListHITsForQualificationType =
      ListHITsForQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHITsForQualificationTypeResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (x Lude..?> "HITs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHITsForQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListHITsForQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListHITsForQualificationType where
  toJSON ListHITsForQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId)
          ]
      )

instance Lude.ToPath ListHITsForQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery ListHITsForQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListHITsForQualificationTypeResponse' smart constructor.
data ListHITsForQualificationTypeResponse = ListHITsForQualificationTypeResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    numResults ::
      Lude.Maybe
        Lude.Int,
    hITs ::
      Lude.Maybe [HIT],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHITsForQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'hITs' - The list of HIT elements returned by the query.
-- * 'nextToken' - Undocumented field.
-- * 'numResults' - The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
-- * 'responseStatus' - The response status code.
mkListHITsForQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHITsForQualificationTypeResponse
mkListHITsForQualificationTypeResponse pResponseStatus_ =
  ListHITsForQualificationTypeResponse'
    { nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      hITs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtrsNextToken :: Lens.Lens' ListHITsForQualificationTypeResponse (Lude.Maybe Lude.Text)
lhitfqtrsNextToken = Lens.lens (nextToken :: ListHITsForQualificationTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHITsForQualificationTypeResponse)
{-# DEPRECATED lhitfqtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtrsNumResults :: Lens.Lens' ListHITsForQualificationTypeResponse (Lude.Maybe Lude.Int)
lhitfqtrsNumResults = Lens.lens (numResults :: ListHITsForQualificationTypeResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListHITsForQualificationTypeResponse)
{-# DEPRECATED lhitfqtrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of HIT elements returned by the query.
--
-- /Note:/ Consider using 'hITs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtrsHITs :: Lens.Lens' ListHITsForQualificationTypeResponse (Lude.Maybe [HIT])
lhitfqtrsHITs = Lens.lens (hITs :: ListHITsForQualificationTypeResponse -> Lude.Maybe [HIT]) (\s a -> s {hITs = a} :: ListHITsForQualificationTypeResponse)
{-# DEPRECATED lhitfqtrsHITs "Use generic-lens or generic-optics with 'hITs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitfqtrsResponseStatus :: Lens.Lens' ListHITsForQualificationTypeResponse Lude.Int
lhitfqtrsResponseStatus = Lens.lens (responseStatus :: ListHITsForQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHITsForQualificationTypeResponse)
{-# DEPRECATED lhitfqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
