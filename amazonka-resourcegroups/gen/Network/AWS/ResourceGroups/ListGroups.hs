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
-- Module      : Network.AWS.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing resource groups in your account.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:ListGroups@
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroups
  ( -- * Creating a Request
    ListGroups (..),
    newListGroups,

    -- * Request Lenses
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_filters,

    -- * Destructuring the Response
    ListGroupsResponse (..),
    newListGroupsResponse,

    -- * Response Lenses
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_groupIdentifiers,
    listGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- provided by a previous call\'s @NextToken@ response to indicate where
    -- the output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that the service might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters, formatted as GroupFilter objects, that you want to apply to a
    -- @ListGroups@ operation.
    --
    -- -   @resource-type@ - Filter the results to include only those of the
    --     specified resource types. Specify up to five resource types in the
    --     format @AWS::ServiceCode::ResourceType @. For example,
    --     @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
    --
    -- -   @configuration-type@ - Filter the results to include only those
    --     groups that have the specified configuration types attached. The
    --     current supported values are:
    --
    --     -   @AWS:EC2::CapacityReservationPool@
    --
    --     -   @AWS:EC2::HostManagement@
    filters :: Prelude.Maybe [GroupFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroups_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
--
-- 'maxResults', 'listGroups_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'filters', 'listGroups_filters' - Filters, formatted as GroupFilter objects, that you want to apply to a
-- @ListGroups@ operation.
--
-- -   @resource-type@ - Filter the results to include only those of the
--     specified resource types. Specify up to five resource types in the
--     format @AWS::ServiceCode::ResourceType @. For example,
--     @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
--
-- -   @configuration-type@ - Filter the results to include only those
--     groups that have the specified configuration types attached. The
--     current supported values are:
--
--     -   @AWS:EC2::CapacityReservationPool@
--
--     -   @AWS:EC2::HostManagement@
newListGroups ::
  ListGroups
newListGroups =
  ListGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
listGroups_nextToken :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Text)
listGroups_nextToken = Lens.lens (\ListGroups' {nextToken} -> nextToken) (\s@ListGroups' {} a -> s {nextToken = a} :: ListGroups)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listGroups_maxResults :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Natural)
listGroups_maxResults = Lens.lens (\ListGroups' {maxResults} -> maxResults) (\s@ListGroups' {} a -> s {maxResults = a} :: ListGroups)

-- | Filters, formatted as GroupFilter objects, that you want to apply to a
-- @ListGroups@ operation.
--
-- -   @resource-type@ - Filter the results to include only those of the
--     specified resource types. Specify up to five resource types in the
--     format @AWS::ServiceCode::ResourceType @. For example,
--     @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
--
-- -   @configuration-type@ - Filter the results to include only those
--     groups that have the specified configuration types attached. The
--     current supported values are:
--
--     -   @AWS:EC2::CapacityReservationPool@
--
--     -   @AWS:EC2::HostManagement@
listGroups_filters :: Lens.Lens' ListGroups (Prelude.Maybe [GroupFilter])
listGroups_filters = Lens.lens (\ListGroups' {filters} -> filters) (\s@ListGroups' {} a -> s {filters = a} :: ListGroups) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_groupIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_groups Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGroups_nextToken
          Lens..~ rs
          Lens.^? listGroupsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListGroups where
  type AWSResponse ListGroups = ListGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Prelude.<$> (x Core..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "GroupIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroups

instance Prelude.NFData ListGroups

instance Core.ToHeaders ListGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Filters" Core..=) Prelude.<$> filters]
      )

instance Core.ToPath ListGroups where
  toPath = Prelude.const "/groups-list"

instance Core.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | /__Deprecated - don\'t use this field. Use the @GroupIdentifiers@
    -- response field instead.__/
    groups :: Prelude.Maybe [Group],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of GroupIdentifier objects. Each identifier is an object that
    -- contains both the @Name@ and the @GroupArn@.
    groupIdentifiers :: Prelude.Maybe [GroupIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'listGroupsResponse_groups' - /__Deprecated - don\'t use this field. Use the @GroupIdentifiers@
-- response field instead.__/
--
-- 'nextToken', 'listGroupsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'groupIdentifiers', 'listGroupsResponse_groupIdentifiers' - A list of GroupIdentifier objects. Each identifier is an object that
-- contains both the @Name@ and the @GroupArn@.
--
-- 'httpStatus', 'listGroupsResponse_httpStatus' - The response's http status code.
newListGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsResponse
newListGroupsResponse pHttpStatus_ =
  ListGroupsResponse'
    { groups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      groupIdentifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | /__Deprecated - don\'t use this field. Use the @GroupIdentifiers@
-- response field instead.__/
listGroupsResponse_groups :: Lens.Lens' ListGroupsResponse (Prelude.Maybe [Group])
listGroupsResponse_groups = Lens.lens (\ListGroupsResponse' {groups} -> groups) (\s@ListGroupsResponse' {} a -> s {groups = a} :: ListGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listGroupsResponse_nextToken :: Lens.Lens' ListGroupsResponse (Prelude.Maybe Prelude.Text)
listGroupsResponse_nextToken = Lens.lens (\ListGroupsResponse' {nextToken} -> nextToken) (\s@ListGroupsResponse' {} a -> s {nextToken = a} :: ListGroupsResponse)

-- | A list of GroupIdentifier objects. Each identifier is an object that
-- contains both the @Name@ and the @GroupArn@.
listGroupsResponse_groupIdentifiers :: Lens.Lens' ListGroupsResponse (Prelude.Maybe [GroupIdentifier])
listGroupsResponse_groupIdentifiers = Lens.lens (\ListGroupsResponse' {groupIdentifiers} -> groupIdentifiers) (\s@ListGroupsResponse' {} a -> s {groupIdentifiers = a} :: ListGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGroupsResponse_httpStatus :: Lens.Lens' ListGroupsResponse Prelude.Int
listGroupsResponse_httpStatus = Lens.lens (\ListGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGroupsResponse' {} a -> s {httpStatus = a} :: ListGroupsResponse)

instance Prelude.NFData ListGroupsResponse
