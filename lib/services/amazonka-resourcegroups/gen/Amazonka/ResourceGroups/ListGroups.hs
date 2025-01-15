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
-- Module      : Amazonka.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ResourceGroups.ListGroups
  ( -- * Creating a Request
    ListGroups (..),
    newListGroups,

    -- * Request Lenses
    listGroups_filters,
    listGroups_maxResults,
    listGroups_nextToken,

    -- * Destructuring the Response
    ListGroupsResponse (..),
    newListGroupsResponse,

    -- * Response Lenses
    listGroupsResponse_groupIdentifiers,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | Filters, formatted as GroupFilter objects, that you want to apply to a
    -- @ListGroups@ operation.
    --
    -- -   @resource-type@ - Filter the results to include only those of the
    --     specified resource types. Specify up to five resource types in the
    --     format @AWS::@/@ServiceCode@/@::@/@ResourceType@/@ @. For example,
    --     @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
    --
    -- -   @configuration-type@ - Filter the results to include only those
    --     groups that have the specified configuration types attached. The
    --     current supported values are:
    --
    --     -   @AWS:EC2::CapacityReservationPool@
    --
    --     -   @AWS:EC2::HostManagement@
    filters :: Prelude.Maybe [GroupFilter],
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
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- provided by a previous call\'s @NextToken@ response to indicate where
    -- the output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'listGroups_filters' - Filters, formatted as GroupFilter objects, that you want to apply to a
-- @ListGroups@ operation.
--
-- -   @resource-type@ - Filter the results to include only those of the
--     specified resource types. Specify up to five resource types in the
--     format @AWS::@/@ServiceCode@/@::@/@ResourceType@/@ @. For example,
--     @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
--
-- -   @configuration-type@ - Filter the results to include only those
--     groups that have the specified configuration types attached. The
--     current supported values are:
--
--     -   @AWS:EC2::CapacityReservationPool@
--
--     -   @AWS:EC2::HostManagement@
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
-- 'nextToken', 'listGroups_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
newListGroups ::
  ListGroups
newListGroups =
  ListGroups'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters, formatted as GroupFilter objects, that you want to apply to a
-- @ListGroups@ operation.
--
-- -   @resource-type@ - Filter the results to include only those of the
--     specified resource types. Specify up to five resource types in the
--     format @AWS::@/@ServiceCode@/@::@/@ResourceType@/@ @. For example,
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
listGroups_filters = Lens.lens (\ListGroups' {filters} -> filters) (\s@ListGroups' {} a -> s {filters = a} :: ListGroups) Prelude.. Lens.mapping Lens.coerced

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

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
listGroups_nextToken :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Text)
listGroups_nextToken = Lens.lens (\ListGroups' {nextToken} -> nextToken) (\s@ListGroups' {} a -> s {nextToken = a} :: ListGroups)

instance Core.AWSPager ListGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_nextToken
            Prelude.. Lens._Just
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
            Lens.^? listGroupsResponse_groups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listGroups_nextToken
              Lens..~ rs
              Lens.^? listGroupsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListGroups where
  type AWSResponse ListGroups = ListGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Prelude.<$> ( x
                            Data..?> "GroupIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroups where
  hashWithSalt _salt ListGroups' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGroups where
  rnf ListGroups' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Filters" Data..=) Prelude.<$> filters]
      )

instance Data.ToPath ListGroups where
  toPath = Prelude.const "/groups-list"

instance Data.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | A list of GroupIdentifier objects. Each identifier is an object that
    -- contains both the @Name@ and the @GroupArn@.
    groupIdentifiers :: Prelude.Maybe [GroupIdentifier],
    -- | /__Deprecated - don\'t use this field. Use the @GroupIdentifiers@
    -- response field instead.__/
    groups :: Prelude.Maybe [Group],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'groupIdentifiers', 'listGroupsResponse_groupIdentifiers' - A list of GroupIdentifier objects. Each identifier is an object that
-- contains both the @Name@ and the @GroupArn@.
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
-- 'httpStatus', 'listGroupsResponse_httpStatus' - The response's http status code.
newListGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsResponse
newListGroupsResponse pHttpStatus_ =
  ListGroupsResponse'
    { groupIdentifiers =
        Prelude.Nothing,
      groups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of GroupIdentifier objects. Each identifier is an object that
-- contains both the @Name@ and the @GroupArn@.
listGroupsResponse_groupIdentifiers :: Lens.Lens' ListGroupsResponse (Prelude.Maybe [GroupIdentifier])
listGroupsResponse_groupIdentifiers = Lens.lens (\ListGroupsResponse' {groupIdentifiers} -> groupIdentifiers) (\s@ListGroupsResponse' {} a -> s {groupIdentifiers = a} :: ListGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | /__Deprecated - don\'t use this field. Use the @GroupIdentifiers@
-- response field instead.__/
listGroupsResponse_groups :: Lens.Lens' ListGroupsResponse (Prelude.Maybe [Group])
listGroupsResponse_groups = Lens.lens (\ListGroupsResponse' {groups} -> groups) (\s@ListGroupsResponse' {} a -> s {groups = a} :: ListGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listGroupsResponse_nextToken :: Lens.Lens' ListGroupsResponse (Prelude.Maybe Prelude.Text)
listGroupsResponse_nextToken = Lens.lens (\ListGroupsResponse' {nextToken} -> nextToken) (\s@ListGroupsResponse' {} a -> s {nextToken = a} :: ListGroupsResponse)

-- | The response's http status code.
listGroupsResponse_httpStatus :: Lens.Lens' ListGroupsResponse Prelude.Int
listGroupsResponse_httpStatus = Lens.lens (\ListGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGroupsResponse' {} a -> s {httpStatus = a} :: ListGroupsResponse)

instance Prelude.NFData ListGroupsResponse where
  rnf ListGroupsResponse' {..} =
    Prelude.rnf groupIdentifiers `Prelude.seq`
      Prelude.rnf groups `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
