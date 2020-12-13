{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersWithQualificationType@ operation returns all of the Workers that have been associated with a given Qualification type.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
  ( -- * Creating a request
    ListWorkersWithQualificationType (..),
    mkListWorkersWithQualificationType,

    -- ** Request lenses
    lwwqtStatus,
    lwwqtNextToken,
    lwwqtQualificationTypeId,
    lwwqtMaxResults,

    -- * Destructuring the response
    ListWorkersWithQualificationTypeResponse (..),
    mkListWorkersWithQualificationTypeResponse,

    -- ** Response lenses
    lwwqtrsNextToken,
    lwwqtrsNumResults,
    lwwqtrsQualifications,
    lwwqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListWorkersWithQualificationType' smart constructor.
data ListWorkersWithQualificationType = ListWorkersWithQualificationType'
  { -- | The status of the Qualifications to return. Can be @Granted | Revoked@ .
    status :: Lude.Maybe QualificationStatus,
    -- | Pagination Token
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Qualification type of the Qualifications to return.
    qualificationTypeId :: Lude.Text,
    -- | Limit the number of results returned.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkersWithQualificationType' with the minimum fields required to make a request.
--
-- * 'status' - The status of the Qualifications to return. Can be @Granted | Revoked@ .
-- * 'nextToken' - Pagination Token
-- * 'qualificationTypeId' - The ID of the Qualification type of the Qualifications to return.
-- * 'maxResults' - Limit the number of results returned.
mkListWorkersWithQualificationType ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  ListWorkersWithQualificationType
mkListWorkersWithQualificationType pQualificationTypeId_ =
  ListWorkersWithQualificationType'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      qualificationTypeId = pQualificationTypeId_,
      maxResults = Lude.Nothing
    }

-- | The status of the Qualifications to return. Can be @Granted | Revoked@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtStatus :: Lens.Lens' ListWorkersWithQualificationType (Lude.Maybe QualificationStatus)
lwwqtStatus = Lens.lens (status :: ListWorkersWithQualificationType -> Lude.Maybe QualificationStatus) (\s a -> s {status = a} :: ListWorkersWithQualificationType)
{-# DEPRECATED lwwqtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Pagination Token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtNextToken :: Lens.Lens' ListWorkersWithQualificationType (Lude.Maybe Lude.Text)
lwwqtNextToken = Lens.lens (nextToken :: ListWorkersWithQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkersWithQualificationType)
{-# DEPRECATED lwwqtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Qualification type of the Qualifications to return.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtQualificationTypeId :: Lens.Lens' ListWorkersWithQualificationType Lude.Text
lwwqtQualificationTypeId = Lens.lens (qualificationTypeId :: ListWorkersWithQualificationType -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: ListWorkersWithQualificationType)
{-# DEPRECATED lwwqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtMaxResults :: Lens.Lens' ListWorkersWithQualificationType (Lude.Maybe Lude.Natural)
lwwqtMaxResults = Lens.lens (maxResults :: ListWorkersWithQualificationType -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkersWithQualificationType)
{-# DEPRECATED lwwqtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListWorkersWithQualificationType where
  page rq rs
    | Page.stop (rs Lens.^. lwwqtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwwqtrsQualifications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwwqtNextToken Lens..~ rs Lens.^. lwwqtrsNextToken

instance Lude.AWSRequest ListWorkersWithQualificationType where
  type
    Rs ListWorkersWithQualificationType =
      ListWorkersWithQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkersWithQualificationTypeResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (x Lude..?> "Qualifications" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListWorkersWithQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListWorkersWithQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkersWithQualificationType where
  toJSON ListWorkersWithQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListWorkersWithQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkersWithQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWorkersWithQualificationTypeResponse' smart constructor.
data ListWorkersWithQualificationTypeResponse = ListWorkersWithQualificationTypeResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | The number of Qualifications on this page in the filtered results list, equivalent to the number of Qualifications being returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The list of Qualification elements returned by this call.
    qualifications :: Lude.Maybe [Qualification],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkersWithQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'numResults' - The number of Qualifications on this page in the filtered results list, equivalent to the number of Qualifications being returned by this call.
-- * 'qualifications' - The list of Qualification elements returned by this call.
-- * 'responseStatus' - The response status code.
mkListWorkersWithQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkersWithQualificationTypeResponse
mkListWorkersWithQualificationTypeResponse pResponseStatus_ =
  ListWorkersWithQualificationTypeResponse'
    { nextToken =
        Lude.Nothing,
      numResults = Lude.Nothing,
      qualifications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrsNextToken :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Lude.Maybe Lude.Text)
lwwqtrsNextToken = Lens.lens (nextToken :: ListWorkersWithQualificationTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkersWithQualificationTypeResponse)
{-# DEPRECATED lwwqtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of Qualifications on this page in the filtered results list, equivalent to the number of Qualifications being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrsNumResults :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Lude.Maybe Lude.Int)
lwwqtrsNumResults = Lens.lens (numResults :: ListWorkersWithQualificationTypeResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListWorkersWithQualificationTypeResponse)
{-# DEPRECATED lwwqtrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of Qualification elements returned by this call.
--
-- /Note:/ Consider using 'qualifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrsQualifications :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Lude.Maybe [Qualification])
lwwqtrsQualifications = Lens.lens (qualifications :: ListWorkersWithQualificationTypeResponse -> Lude.Maybe [Qualification]) (\s a -> s {qualifications = a} :: ListWorkersWithQualificationTypeResponse)
{-# DEPRECATED lwwqtrsQualifications "Use generic-lens or generic-optics with 'qualifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrsResponseStatus :: Lens.Lens' ListWorkersWithQualificationTypeResponse Lude.Int
lwwqtrsResponseStatus = Lens.lens (responseStatus :: ListWorkersWithQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkersWithQualificationTypeResponse)
{-# DEPRECATED lwwqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
