{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkills
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all enabled skills in a specific skill group.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkills
  ( -- * Creating a Request
    ListSkills (..),
    newListSkills,

    -- * Request Lenses
    listSkills_nextToken,
    listSkills_maxResults,
    listSkills_skillType,
    listSkills_skillGroupArn,
    listSkills_enablementType,

    -- * Destructuring the Response
    ListSkillsResponse (..),
    newListSkillsResponse,

    -- * Response Lenses
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSkills' smart constructor.
data ListSkills = ListSkills'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Core.Maybe SkillTypeFilter,
    -- | The ARN of the skill group for which to list enabled skills.
    skillGroupArn :: Core.Maybe Core.Text,
    -- | Whether the skill is enabled under the user\'s account.
    enablementType :: Core.Maybe EnablementTypeFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSkills' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkills_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'listSkills_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'skillType', 'listSkills_skillType' - Whether the skill is publicly available or is a private skill.
--
-- 'skillGroupArn', 'listSkills_skillGroupArn' - The ARN of the skill group for which to list enabled skills.
--
-- 'enablementType', 'listSkills_enablementType' - Whether the skill is enabled under the user\'s account.
newListSkills ::
  ListSkills
newListSkills =
  ListSkills'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      skillType = Core.Nothing,
      skillGroupArn = Core.Nothing,
      enablementType = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
listSkills_nextToken :: Lens.Lens' ListSkills (Core.Maybe Core.Text)
listSkills_nextToken = Lens.lens (\ListSkills' {nextToken} -> nextToken) (\s@ListSkills' {} a -> s {nextToken = a} :: ListSkills)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listSkills_maxResults :: Lens.Lens' ListSkills (Core.Maybe Core.Natural)
listSkills_maxResults = Lens.lens (\ListSkills' {maxResults} -> maxResults) (\s@ListSkills' {} a -> s {maxResults = a} :: ListSkills)

-- | Whether the skill is publicly available or is a private skill.
listSkills_skillType :: Lens.Lens' ListSkills (Core.Maybe SkillTypeFilter)
listSkills_skillType = Lens.lens (\ListSkills' {skillType} -> skillType) (\s@ListSkills' {} a -> s {skillType = a} :: ListSkills)

-- | The ARN of the skill group for which to list enabled skills.
listSkills_skillGroupArn :: Lens.Lens' ListSkills (Core.Maybe Core.Text)
listSkills_skillGroupArn = Lens.lens (\ListSkills' {skillGroupArn} -> skillGroupArn) (\s@ListSkills' {} a -> s {skillGroupArn = a} :: ListSkills)

-- | Whether the skill is enabled under the user\'s account.
listSkills_enablementType :: Lens.Lens' ListSkills (Core.Maybe EnablementTypeFilter)
listSkills_enablementType = Lens.lens (\ListSkills' {enablementType} -> enablementType) (\s@ListSkills' {} a -> s {enablementType = a} :: ListSkills)

instance Core.AWSPager ListSkills where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsResponse_skillSummaries Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSkills_nextToken
          Lens..~ rs
          Lens.^? listSkillsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSkills where
  type AWSResponse ListSkills = ListSkillsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "SkillSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSkills

instance Core.NFData ListSkills

instance Core.ToHeaders ListSkills where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.ListSkills" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSkills where
  toJSON ListSkills' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SkillType" Core..=) Core.<$> skillType,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn,
            ("EnablementType" Core..=) Core.<$> enablementType
          ]
      )

instance Core.ToPath ListSkills where
  toPath = Core.const "/"

instance Core.ToQuery ListSkills where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSkillsResponse' smart constructor.
data ListSkillsResponse = ListSkillsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of enabled skills requested. Required.
    skillSummaries :: Core.Maybe [SkillSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSkillsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkillsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'skillSummaries', 'listSkillsResponse_skillSummaries' - The list of enabled skills requested. Required.
--
-- 'httpStatus', 'listSkillsResponse_httpStatus' - The response's http status code.
newListSkillsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSkillsResponse
newListSkillsResponse pHttpStatus_ =
  ListSkillsResponse'
    { nextToken = Core.Nothing,
      skillSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
listSkillsResponse_nextToken :: Lens.Lens' ListSkillsResponse (Core.Maybe Core.Text)
listSkillsResponse_nextToken = Lens.lens (\ListSkillsResponse' {nextToken} -> nextToken) (\s@ListSkillsResponse' {} a -> s {nextToken = a} :: ListSkillsResponse)

-- | The list of enabled skills requested. Required.
listSkillsResponse_skillSummaries :: Lens.Lens' ListSkillsResponse (Core.Maybe [SkillSummary])
listSkillsResponse_skillSummaries = Lens.lens (\ListSkillsResponse' {skillSummaries} -> skillSummaries) (\s@ListSkillsResponse' {} a -> s {skillSummaries = a} :: ListSkillsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSkillsResponse_httpStatus :: Lens.Lens' ListSkillsResponse Core.Int
listSkillsResponse_httpStatus = Lens.lens (\ListSkillsResponse' {httpStatus} -> httpStatus) (\s@ListSkillsResponse' {} a -> s {httpStatus = a} :: ListSkillsResponse)

instance Core.NFData ListSkillsResponse
