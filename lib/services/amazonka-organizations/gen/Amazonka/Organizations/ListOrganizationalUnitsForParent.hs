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
-- Module      : Amazonka.Organizations.ListOrganizationalUnitsForParent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organizational units (OUs) in a parent organizational unit or
-- root.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a @List*@ operation. These operations can occasionally return an
-- empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListOrganizationalUnitsForParent
  ( -- * Creating a Request
    ListOrganizationalUnitsForParent (..),
    newListOrganizationalUnitsForParent,

    -- * Request Lenses
    listOrganizationalUnitsForParent_maxResults,
    listOrganizationalUnitsForParent_nextToken,
    listOrganizationalUnitsForParent_parentId,

    -- * Destructuring the Response
    ListOrganizationalUnitsForParentResponse (..),
    newListOrganizationalUnitsForParentResponse,

    -- * Response Lenses
    listOrganizationalUnitsForParentResponse_nextToken,
    listOrganizationalUnitsForParentResponse_organizationalUnits,
    listOrganizationalUnitsForParentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrganizationalUnitsForParent' smart constructor.
data ListOrganizationalUnitsForParent = ListOrganizationalUnitsForParent'
  { -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the root or OU whose child OUs you want to
    -- list.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    parentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationalUnitsForParent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOrganizationalUnitsForParent_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'nextToken', 'listOrganizationalUnitsForParent_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'parentId', 'listOrganizationalUnitsForParent_parentId' - The unique identifier (ID) of the root or OU whose child OUs you want to
-- list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
newListOrganizationalUnitsForParent ::
  -- | 'parentId'
  Prelude.Text ->
  ListOrganizationalUnitsForParent
newListOrganizationalUnitsForParent pParentId_ =
  ListOrganizationalUnitsForParent'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parentId = pParentId_
    }

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listOrganizationalUnitsForParent_maxResults :: Lens.Lens' ListOrganizationalUnitsForParent (Prelude.Maybe Prelude.Natural)
listOrganizationalUnitsForParent_maxResults = Lens.lens (\ListOrganizationalUnitsForParent' {maxResults} -> maxResults) (\s@ListOrganizationalUnitsForParent' {} a -> s {maxResults = a} :: ListOrganizationalUnitsForParent)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listOrganizationalUnitsForParent_nextToken :: Lens.Lens' ListOrganizationalUnitsForParent (Prelude.Maybe Prelude.Text)
listOrganizationalUnitsForParent_nextToken = Lens.lens (\ListOrganizationalUnitsForParent' {nextToken} -> nextToken) (\s@ListOrganizationalUnitsForParent' {} a -> s {nextToken = a} :: ListOrganizationalUnitsForParent)

-- | The unique identifier (ID) of the root or OU whose child OUs you want to
-- list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
listOrganizationalUnitsForParent_parentId :: Lens.Lens' ListOrganizationalUnitsForParent Prelude.Text
listOrganizationalUnitsForParent_parentId = Lens.lens (\ListOrganizationalUnitsForParent' {parentId} -> parentId) (\s@ListOrganizationalUnitsForParent' {} a -> s {parentId = a} :: ListOrganizationalUnitsForParent)

instance
  Core.AWSPager
    ListOrganizationalUnitsForParent
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationalUnitsForParentResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationalUnitsForParentResponse_organizationalUnits
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOrganizationalUnitsForParent_nextToken
          Lens..~ rs
          Lens.^? listOrganizationalUnitsForParentResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListOrganizationalUnitsForParent
  where
  type
    AWSResponse ListOrganizationalUnitsForParent =
      ListOrganizationalUnitsForParentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationalUnitsForParentResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "OrganizationalUnits"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListOrganizationalUnitsForParent
  where
  hashWithSalt
    _salt
    ListOrganizationalUnitsForParent' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` parentId

instance
  Prelude.NFData
    ListOrganizationalUnitsForParent
  where
  rnf ListOrganizationalUnitsForParent' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parentId

instance
  Data.ToHeaders
    ListOrganizationalUnitsForParent
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListOrganizationalUnitsForParent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOrganizationalUnitsForParent where
  toJSON ListOrganizationalUnitsForParent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ParentId" Data..= parentId)
          ]
      )

instance Data.ToPath ListOrganizationalUnitsForParent where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListOrganizationalUnitsForParent
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOrganizationalUnitsForParentResponse' smart constructor.
data ListOrganizationalUnitsForParentResponse = ListOrganizationalUnitsForParentResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the OUs in the specified root or parent OU.
    organizationalUnits :: Prelude.Maybe [OrganizationalUnit],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationalUnitsForParentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationalUnitsForParentResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'organizationalUnits', 'listOrganizationalUnitsForParentResponse_organizationalUnits' - A list of the OUs in the specified root or parent OU.
--
-- 'httpStatus', 'listOrganizationalUnitsForParentResponse_httpStatus' - The response's http status code.
newListOrganizationalUnitsForParentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationalUnitsForParentResponse
newListOrganizationalUnitsForParentResponse
  pHttpStatus_ =
    ListOrganizationalUnitsForParentResponse'
      { nextToken =
          Prelude.Nothing,
        organizationalUnits =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listOrganizationalUnitsForParentResponse_nextToken :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Prelude.Maybe Prelude.Text)
listOrganizationalUnitsForParentResponse_nextToken = Lens.lens (\ListOrganizationalUnitsForParentResponse' {nextToken} -> nextToken) (\s@ListOrganizationalUnitsForParentResponse' {} a -> s {nextToken = a} :: ListOrganizationalUnitsForParentResponse)

-- | A list of the OUs in the specified root or parent OU.
listOrganizationalUnitsForParentResponse_organizationalUnits :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Prelude.Maybe [OrganizationalUnit])
listOrganizationalUnitsForParentResponse_organizationalUnits = Lens.lens (\ListOrganizationalUnitsForParentResponse' {organizationalUnits} -> organizationalUnits) (\s@ListOrganizationalUnitsForParentResponse' {} a -> s {organizationalUnits = a} :: ListOrganizationalUnitsForParentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOrganizationalUnitsForParentResponse_httpStatus :: Lens.Lens' ListOrganizationalUnitsForParentResponse Prelude.Int
listOrganizationalUnitsForParentResponse_httpStatus = Lens.lens (\ListOrganizationalUnitsForParentResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationalUnitsForParentResponse' {} a -> s {httpStatus = a} :: ListOrganizationalUnitsForParentResponse)

instance
  Prelude.NFData
    ListOrganizationalUnitsForParentResponse
  where
  rnf ListOrganizationalUnitsForParentResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationalUnits
      `Prelude.seq` Prelude.rnf httpStatus
