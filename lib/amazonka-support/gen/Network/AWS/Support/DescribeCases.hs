{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cases that you specify by passing one or more case IDs. You can use the @afterTime@ and @beforeTime@ parameters to filter the cases by date. You can set values for the @includeResolvedCases@ and @includeCommunications@ parameters to specify how much information to return.
--
-- The response returns the following in JSON format:
--
--     * One or more <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails> data types.
--
--
--     * One or more @nextToken@ values, which specify where to paginate the returned records represented by the @CaseDetails@ objects.
--
--
-- Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request might return an error.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCases
  ( -- * Creating a request
    DescribeCases (..),
    mkDescribeCases,

    -- ** Request lenses
    dcIncludeResolvedCases,
    dcCaseIdList,
    dcAfterTime,
    dcBeforeTime,
    dcNextToken,
    dcIncludeCommunications,
    dcDisplayId,
    dcLanguage,
    dcMaxResults,

    -- * Destructuring the response
    DescribeCasesResponse (..),
    mkDescribeCasesResponse,

    -- ** Response lenses
    drsCases,
    drsNextToken,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeCases' smart constructor.
data DescribeCases = DescribeCases'
  { includeResolvedCases ::
      Lude.Maybe Lude.Bool,
    caseIdList :: Lude.Maybe [Lude.Text],
    afterTime :: Lude.Maybe Lude.Text,
    beforeTime :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    includeCommunications :: Lude.Maybe Lude.Bool,
    displayId :: Lude.Maybe Lude.Text,
    language :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeCases' with the minimum fields required to make a request.
--
-- * 'afterTime' - The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
-- * 'beforeTime' - The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
-- * 'caseIdList' - A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
-- * 'displayId' - The ID displayed for a case in the AWS Support Center user interface.
-- * 'includeCommunications' - Specifies whether to include communications in the @DescribeCases@ response. By default, communications are incuded.
-- * 'includeResolvedCases' - Specifies whether to include resolved support cases in the @DescribeCases@ response. By default, resolved cases aren't included.
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
-- * 'maxResults' - The maximum number of results to return before paginating.
-- * 'nextToken' - A resumption point for pagination.
mkDescribeCases ::
  DescribeCases
mkDescribeCases =
  DescribeCases'
    { includeResolvedCases = Lude.Nothing,
      caseIdList = Lude.Nothing,
      afterTime = Lude.Nothing,
      beforeTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      includeCommunications = Lude.Nothing,
      displayId = Lude.Nothing,
      language = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies whether to include resolved support cases in the @DescribeCases@ response. By default, resolved cases aren't included.
--
-- /Note:/ Consider using 'includeResolvedCases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIncludeResolvedCases :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Bool)
dcIncludeResolvedCases = Lens.lens (includeResolvedCases :: DescribeCases -> Lude.Maybe Lude.Bool) (\s a -> s {includeResolvedCases = a} :: DescribeCases)
{-# DEPRECATED dcIncludeResolvedCases "Use generic-lens or generic-optics with 'includeResolvedCases' instead." #-}

-- | A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
--
-- /Note:/ Consider using 'caseIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCaseIdList :: Lens.Lens' DescribeCases (Lude.Maybe [Lude.Text])
dcCaseIdList = Lens.lens (caseIdList :: DescribeCases -> Lude.Maybe [Lude.Text]) (\s a -> s {caseIdList = a} :: DescribeCases)
{-# DEPRECATED dcCaseIdList "Use generic-lens or generic-optics with 'caseIdList' instead." #-}

-- | The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'afterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAfterTime :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Text)
dcAfterTime = Lens.lens (afterTime :: DescribeCases -> Lude.Maybe Lude.Text) (\s a -> s {afterTime = a} :: DescribeCases)
{-# DEPRECATED dcAfterTime "Use generic-lens or generic-optics with 'afterTime' instead." #-}

-- | The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'beforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcBeforeTime :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Text)
dcBeforeTime = Lens.lens (beforeTime :: DescribeCases -> Lude.Maybe Lude.Text) (\s a -> s {beforeTime = a} :: DescribeCases)
{-# DEPRECATED dcBeforeTime "Use generic-lens or generic-optics with 'beforeTime' instead." #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Text)
dcNextToken = Lens.lens (nextToken :: DescribeCases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCases)
{-# DEPRECATED dcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies whether to include communications in the @DescribeCases@ response. By default, communications are incuded.
--
-- /Note:/ Consider using 'includeCommunications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIncludeCommunications :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Bool)
dcIncludeCommunications = Lens.lens (includeCommunications :: DescribeCases -> Lude.Maybe Lude.Bool) (\s a -> s {includeCommunications = a} :: DescribeCases)
{-# DEPRECATED dcIncludeCommunications "Use generic-lens or generic-optics with 'includeCommunications' instead." #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
--
-- /Note:/ Consider using 'displayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDisplayId :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Text)
dcDisplayId = Lens.lens (displayId :: DescribeCases -> Lude.Maybe Lude.Text) (\s a -> s {displayId = a} :: DescribeCases)
{-# DEPRECATED dcDisplayId "Use generic-lens or generic-optics with 'displayId' instead." #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLanguage :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Text)
dcLanguage = Lens.lens (language :: DescribeCases -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: DescribeCases)
{-# DEPRECATED dcLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The maximum number of results to return before paginating.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeCases (Lude.Maybe Lude.Natural)
dcMaxResults = Lens.lens (maxResults :: DescribeCases -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCases)
{-# DEPRECATED dcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeCases where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsCases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeCases where
  type Rs DescribeCases = DescribeCasesResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCasesResponse'
            Lude.<$> (x Lude..?> "cases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.DescribeCases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCases where
  toJSON DescribeCases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeResolvedCases" Lude..=) Lude.<$> includeResolvedCases,
            ("caseIdList" Lude..=) Lude.<$> caseIdList,
            ("afterTime" Lude..=) Lude.<$> afterTime,
            ("beforeTime" Lude..=) Lude.<$> beforeTime,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("includeCommunications" Lude..=) Lude.<$> includeCommunications,
            ("displayId" Lude..=) Lude.<$> displayId,
            ("language" Lude..=) Lude.<$> language,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeCases where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCases where
  toQuery = Lude.const Lude.mempty

-- | Returns an array of <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails> objects and a @nextToken@ that defines a point for pagination in the result set.
--
-- /See:/ 'mkDescribeCasesResponse' smart constructor.
data DescribeCasesResponse = DescribeCasesResponse'
  { cases ::
      Lude.Maybe [CaseDetails],
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

-- | Creates a value of 'DescribeCasesResponse' with the minimum fields required to make a request.
--
-- * 'cases' - The details for the cases that match the request.
-- * 'nextToken' - A resumption point for pagination.
-- * 'responseStatus' - The response status code.
mkDescribeCasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCasesResponse
mkDescribeCasesResponse pResponseStatus_ =
  DescribeCasesResponse'
    { cases = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details for the cases that match the request.
--
-- /Note:/ Consider using 'cases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCases :: Lens.Lens' DescribeCasesResponse (Lude.Maybe [CaseDetails])
drsCases = Lens.lens (cases :: DescribeCasesResponse -> Lude.Maybe [CaseDetails]) (\s a -> s {cases = a} :: DescribeCasesResponse)
{-# DEPRECATED drsCases "Use generic-lens or generic-optics with 'cases' instead." #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeCasesResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeCasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCasesResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCasesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeCasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCasesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
