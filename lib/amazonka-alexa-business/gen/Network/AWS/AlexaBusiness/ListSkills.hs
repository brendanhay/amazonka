{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkills
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all enabled skills in a specific skill group.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkills
  ( -- * Creating a request
    ListSkills (..),
    mkListSkills,

    -- ** Request lenses
    lsSkillGroupARN,
    lsSkillType,
    lsNextToken,
    lsEnablementType,
    lsMaxResults,

    -- * Destructuring the response
    ListSkillsResponse (..),
    mkListSkillsResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsSkillSummaries,
    lsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSkills' smart constructor.
data ListSkills = ListSkills'
  { -- | The ARN of the skill group for which to list enabled skills.
    skillGroupARN :: Lude.Maybe Lude.Text,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Lude.Maybe SkillTypeFilter,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | Whether the skill is enabled under the user's account.
    enablementType :: Lude.Maybe EnablementTypeFilter,
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkills' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The ARN of the skill group for which to list enabled skills.
-- * 'skillType' - Whether the skill is publicly available or is a private skill.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'enablementType' - Whether the skill is enabled under the user's account.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
mkListSkills ::
  ListSkills
mkListSkills =
  ListSkills'
    { skillGroupARN = Lude.Nothing,
      skillType = Lude.Nothing,
      nextToken = Lude.Nothing,
      enablementType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ARN of the skill group for which to list enabled skills.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSkillGroupARN :: Lens.Lens' ListSkills (Lude.Maybe Lude.Text)
lsSkillGroupARN = Lens.lens (skillGroupARN :: ListSkills -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: ListSkills)
{-# DEPRECATED lsSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | Whether the skill is publicly available or is a private skill.
--
-- /Note:/ Consider using 'skillType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSkillType :: Lens.Lens' ListSkills (Lude.Maybe SkillTypeFilter)
lsSkillType = Lens.lens (skillType :: ListSkills -> Lude.Maybe SkillTypeFilter) (\s a -> s {skillType = a} :: ListSkills)
{-# DEPRECATED lsSkillType "Use generic-lens or generic-optics with 'skillType' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSkills (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListSkills -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkills)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Whether the skill is enabled under the user's account.
--
-- /Note:/ Consider using 'enablementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsEnablementType :: Lens.Lens' ListSkills (Lude.Maybe EnablementTypeFilter)
lsEnablementType = Lens.lens (enablementType :: ListSkills -> Lude.Maybe EnablementTypeFilter) (\s a -> s {enablementType = a} :: ListSkills)
{-# DEPRECATED lsEnablementType "Use generic-lens or generic-optics with 'enablementType' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListSkills (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListSkills -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSkills)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSkills where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsSkillSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListSkills where
  type Rs ListSkills = ListSkillsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSkillsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SkillSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSkills where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListSkills" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSkills where
  toJSON ListSkills' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            ("SkillType" Lude..=) Lude.<$> skillType,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EnablementType" Lude..=) Lude.<$> enablementType,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSkills where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSkills where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSkillsResponse' smart constructor.
data ListSkillsResponse = ListSkillsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of enabled skills requested. Required.
    skillSummaries :: Lude.Maybe [SkillSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkillsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'skillSummaries' - The list of enabled skills requested. Required.
-- * 'responseStatus' - The response status code.
mkListSkillsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSkillsResponse
mkListSkillsResponse pResponseStatus_ =
  ListSkillsResponse'
    { nextToken = Lude.Nothing,
      skillSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListSkillsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListSkillsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkillsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of enabled skills requested. Required.
--
-- /Note:/ Consider using 'skillSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsSkillSummaries :: Lens.Lens' ListSkillsResponse (Lude.Maybe [SkillSummary])
lsrsSkillSummaries = Lens.lens (skillSummaries :: ListSkillsResponse -> Lude.Maybe [SkillSummary]) (\s a -> s {skillSummaries = a} :: ListSkillsResponse)
{-# DEPRECATED lsrsSkillSummaries "Use generic-lens or generic-optics with 'skillSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListSkillsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListSkillsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSkillsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
