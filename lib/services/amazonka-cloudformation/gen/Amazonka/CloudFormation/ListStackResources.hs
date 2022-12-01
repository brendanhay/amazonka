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
-- Module      : Amazonka.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for
-- up to 90 days after the stack has been deleted.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.ListStackResources
  ( -- * Creating a Request
    ListStackResources (..),
    newListStackResources,

    -- * Request Lenses
    listStackResources_nextToken,
    listStackResources_stackName,

    -- * Destructuring the Response
    ListStackResourcesResponse (..),
    newListStackResourcesResponse,

    -- * Response Lenses
    listStackResourcesResponse_nextToken,
    listStackResourcesResponse_stackResourceSummaries,
    listStackResourcesResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListStackResource action.
--
-- /See:/ 'newListStackResources' smart constructor.
data ListStackResources = ListStackResources'
  { -- | A string that identifies the next page of stack resources that you want
    -- to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that is associated with the stack, which
    -- aren\'t always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackResources_nextToken' - A string that identifies the next page of stack resources that you want
-- to retrieve.
--
-- 'stackName', 'listStackResources_stackName' - The name or the unique stack ID that is associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
newListStackResources ::
  -- | 'stackName'
  Prelude.Text ->
  ListStackResources
newListStackResources pStackName_ =
  ListStackResources'
    { nextToken = Prelude.Nothing,
      stackName = pStackName_
    }

-- | A string that identifies the next page of stack resources that you want
-- to retrieve.
listStackResources_nextToken :: Lens.Lens' ListStackResources (Prelude.Maybe Prelude.Text)
listStackResources_nextToken = Lens.lens (\ListStackResources' {nextToken} -> nextToken) (\s@ListStackResources' {} a -> s {nextToken = a} :: ListStackResources)

-- | The name or the unique stack ID that is associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
listStackResources_stackName :: Lens.Lens' ListStackResources Prelude.Text
listStackResources_stackName = Lens.lens (\ListStackResources' {stackName} -> stackName) (\s@ListStackResources' {} a -> s {stackName = a} :: ListStackResources)

instance Core.AWSPager ListStackResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStackResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStackResourcesResponse_stackResourceSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStackResources_nextToken
          Lens..~ rs
          Lens.^? listStackResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStackResources where
  type
    AWSResponse ListStackResources =
      ListStackResourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListStackResourcesResult"
      ( \s h x ->
          ListStackResourcesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "StackResourceSummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStackResources where
  hashWithSalt _salt ListStackResources' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData ListStackResources where
  rnf ListStackResources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackName

instance Core.ToHeaders ListStackResources where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListStackResources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStackResources where
  toQuery ListStackResources' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListStackResources" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "StackName" Core.=: stackName
      ]

-- | The output for a ListStackResources action.
--
-- /See:/ 'newListStackResourcesResponse' smart constructor.
data ListStackResourcesResponse = ListStackResourcesResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- stack resources. If no additional page exists, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @StackResourceSummary@ structures.
    stackResourceSummaries :: Prelude.Maybe [StackResourceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackResourcesResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- stack resources. If no additional page exists, this value is null.
--
-- 'stackResourceSummaries', 'listStackResourcesResponse_stackResourceSummaries' - A list of @StackResourceSummary@ structures.
--
-- 'httpStatus', 'listStackResourcesResponse_httpStatus' - The response's http status code.
newListStackResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStackResourcesResponse
newListStackResourcesResponse pHttpStatus_ =
  ListStackResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      stackResourceSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- stack resources. If no additional page exists, this value is null.
listStackResourcesResponse_nextToken :: Lens.Lens' ListStackResourcesResponse (Prelude.Maybe Prelude.Text)
listStackResourcesResponse_nextToken = Lens.lens (\ListStackResourcesResponse' {nextToken} -> nextToken) (\s@ListStackResourcesResponse' {} a -> s {nextToken = a} :: ListStackResourcesResponse)

-- | A list of @StackResourceSummary@ structures.
listStackResourcesResponse_stackResourceSummaries :: Lens.Lens' ListStackResourcesResponse (Prelude.Maybe [StackResourceSummary])
listStackResourcesResponse_stackResourceSummaries = Lens.lens (\ListStackResourcesResponse' {stackResourceSummaries} -> stackResourceSummaries) (\s@ListStackResourcesResponse' {} a -> s {stackResourceSummaries = a} :: ListStackResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStackResourcesResponse_httpStatus :: Lens.Lens' ListStackResourcesResponse Prelude.Int
listStackResourcesResponse_httpStatus = Lens.lens (\ListStackResourcesResponse' {httpStatus} -> httpStatus) (\s@ListStackResourcesResponse' {} a -> s {httpStatus = a} :: ListStackResourcesResponse)

instance Prelude.NFData ListStackResourcesResponse where
  rnf ListStackResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackResourceSummaries
      `Prelude.seq` Prelude.rnf httpStatus
