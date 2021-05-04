{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSkills' smart constructor.
data ListSkills = ListSkills'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Prelude.Maybe SkillTypeFilter,
    -- | The ARN of the skill group for which to list enabled skills.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the skill is enabled under the user\'s account.
    enablementType :: Prelude.Maybe EnablementTypeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      skillType = Prelude.Nothing,
      skillGroupArn = Prelude.Nothing,
      enablementType = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
listSkills_nextToken :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Text)
listSkills_nextToken = Lens.lens (\ListSkills' {nextToken} -> nextToken) (\s@ListSkills' {} a -> s {nextToken = a} :: ListSkills)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listSkills_maxResults :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Natural)
listSkills_maxResults = Lens.lens (\ListSkills' {maxResults} -> maxResults) (\s@ListSkills' {} a -> s {maxResults = a} :: ListSkills)

-- | Whether the skill is publicly available or is a private skill.
listSkills_skillType :: Lens.Lens' ListSkills (Prelude.Maybe SkillTypeFilter)
listSkills_skillType = Lens.lens (\ListSkills' {skillType} -> skillType) (\s@ListSkills' {} a -> s {skillType = a} :: ListSkills)

-- | The ARN of the skill group for which to list enabled skills.
listSkills_skillGroupArn :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Text)
listSkills_skillGroupArn = Lens.lens (\ListSkills' {skillGroupArn} -> skillGroupArn) (\s@ListSkills' {} a -> s {skillGroupArn = a} :: ListSkills)

-- | Whether the skill is enabled under the user\'s account.
listSkills_enablementType :: Lens.Lens' ListSkills (Prelude.Maybe EnablementTypeFilter)
listSkills_enablementType = Lens.lens (\ListSkills' {enablementType} -> enablementType) (\s@ListSkills' {} a -> s {enablementType = a} :: ListSkills)

instance Pager.AWSPager ListSkills where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSkillsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSkillsResponse_skillSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSkills_nextToken
          Lens..~ rs
          Lens.^? listSkillsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListSkills where
  type Rs ListSkills = ListSkillsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "SkillSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSkills

instance Prelude.NFData ListSkills

instance Prelude.ToHeaders ListSkills where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.ListSkills" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListSkills where
  toJSON ListSkills' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SkillType" Prelude..=) Prelude.<$> skillType,
            ("SkillGroupArn" Prelude..=)
              Prelude.<$> skillGroupArn,
            ("EnablementType" Prelude..=)
              Prelude.<$> enablementType
          ]
      )

instance Prelude.ToPath ListSkills where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListSkills where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSkillsResponse' smart constructor.
data ListSkillsResponse = ListSkillsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of enabled skills requested. Required.
    skillSummaries :: Prelude.Maybe [SkillSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListSkillsResponse
newListSkillsResponse pHttpStatus_ =
  ListSkillsResponse'
    { nextToken = Prelude.Nothing,
      skillSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
listSkillsResponse_nextToken :: Lens.Lens' ListSkillsResponse (Prelude.Maybe Prelude.Text)
listSkillsResponse_nextToken = Lens.lens (\ListSkillsResponse' {nextToken} -> nextToken) (\s@ListSkillsResponse' {} a -> s {nextToken = a} :: ListSkillsResponse)

-- | The list of enabled skills requested. Required.
listSkillsResponse_skillSummaries :: Lens.Lens' ListSkillsResponse (Prelude.Maybe [SkillSummary])
listSkillsResponse_skillSummaries = Lens.lens (\ListSkillsResponse' {skillSummaries} -> skillSummaries) (\s@ListSkillsResponse' {} a -> s {skillSummaries = a} :: ListSkillsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listSkillsResponse_httpStatus :: Lens.Lens' ListSkillsResponse Prelude.Int
listSkillsResponse_httpStatus = Lens.lens (\ListSkillsResponse' {httpStatus} -> httpStatus) (\s@ListSkillsResponse' {} a -> s {httpStatus = a} :: ListSkillsResponse)

instance Prelude.NFData ListSkillsResponse
