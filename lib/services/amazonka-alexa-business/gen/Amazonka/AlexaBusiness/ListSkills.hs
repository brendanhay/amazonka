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
-- Module      : Amazonka.AlexaBusiness.ListSkills
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all enabled skills in a specific skill group.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListSkills
  ( -- * Creating a Request
    ListSkills (..),
    newListSkills,

    -- * Request Lenses
    listSkills_skillGroupArn,
    listSkills_skillType,
    listSkills_nextToken,
    listSkills_enablementType,
    listSkills_maxResults,

    -- * Destructuring the Response
    ListSkillsResponse (..),
    newListSkillsResponse,

    -- * Response Lenses
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSkills' smart constructor.
data ListSkills = ListSkills'
  { -- | The ARN of the skill group for which to list enabled skills.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Prelude.Maybe SkillTypeFilter,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Whether the skill is enabled under the user\'s account.
    enablementType :: Prelude.Maybe EnablementTypeFilter,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSkills' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'listSkills_skillGroupArn' - The ARN of the skill group for which to list enabled skills.
--
-- 'skillType', 'listSkills_skillType' - Whether the skill is publicly available or is a private skill.
--
-- 'nextToken', 'listSkills_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'enablementType', 'listSkills_enablementType' - Whether the skill is enabled under the user\'s account.
--
-- 'maxResults', 'listSkills_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
newListSkills ::
  ListSkills
newListSkills =
  ListSkills'
    { skillGroupArn = Prelude.Nothing,
      skillType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      enablementType = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The ARN of the skill group for which to list enabled skills.
listSkills_skillGroupArn :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Text)
listSkills_skillGroupArn = Lens.lens (\ListSkills' {skillGroupArn} -> skillGroupArn) (\s@ListSkills' {} a -> s {skillGroupArn = a} :: ListSkills)

-- | Whether the skill is publicly available or is a private skill.
listSkills_skillType :: Lens.Lens' ListSkills (Prelude.Maybe SkillTypeFilter)
listSkills_skillType = Lens.lens (\ListSkills' {skillType} -> skillType) (\s@ListSkills' {} a -> s {skillType = a} :: ListSkills)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
listSkills_nextToken :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Text)
listSkills_nextToken = Lens.lens (\ListSkills' {nextToken} -> nextToken) (\s@ListSkills' {} a -> s {nextToken = a} :: ListSkills)

-- | Whether the skill is enabled under the user\'s account.
listSkills_enablementType :: Lens.Lens' ListSkills (Prelude.Maybe EnablementTypeFilter)
listSkills_enablementType = Lens.lens (\ListSkills' {enablementType} -> enablementType) (\s@ListSkills' {} a -> s {enablementType = a} :: ListSkills)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listSkills_maxResults :: Lens.Lens' ListSkills (Prelude.Maybe Prelude.Natural)
listSkills_maxResults = Lens.lens (\ListSkills' {maxResults} -> maxResults) (\s@ListSkills' {} a -> s {maxResults = a} :: ListSkills)

instance Core.AWSPager ListSkills where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsResponse_skillSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSkills_nextToken
          Lens..~ rs
          Lens.^? listSkillsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSkills where
  type AWSResponse ListSkills = ListSkillsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SkillSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSkills where
  hashWithSalt _salt ListSkills' {..} =
    _salt `Prelude.hashWithSalt` skillGroupArn
      `Prelude.hashWithSalt` skillType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` enablementType
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSkills where
  rnf ListSkills' {..} =
    Prelude.rnf skillGroupArn
      `Prelude.seq` Prelude.rnf skillType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf enablementType
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSkills where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListSkills" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSkills where
  toJSON ListSkills' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Core..=) Prelude.<$> skillGroupArn,
            ("SkillType" Core..=) Prelude.<$> skillType,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EnablementType" Core..=)
              Prelude.<$> enablementType,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListSkills where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSkills where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listSkillsResponse_skillSummaries = Lens.lens (\ListSkillsResponse' {skillSummaries} -> skillSummaries) (\s@ListSkillsResponse' {} a -> s {skillSummaries = a} :: ListSkillsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSkillsResponse_httpStatus :: Lens.Lens' ListSkillsResponse Prelude.Int
listSkillsResponse_httpStatus = Lens.lens (\ListSkillsResponse' {httpStatus} -> httpStatus) (\s@ListSkillsResponse' {} a -> s {httpStatus = a} :: ListSkillsResponse)

instance Prelude.NFData ListSkillsResponse where
  rnf ListSkillsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf skillSummaries
      `Prelude.seq` Prelude.rnf httpStatus
