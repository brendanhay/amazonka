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
-- Module      : Amazonka.CloudFormation.ListChangeSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the ID and status of each active change set for a stack. For
-- example, CloudFormation lists change sets that are in the
-- @CREATE_IN_PROGRESS@ or @CREATE_PENDING@ state.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.ListChangeSets
  ( -- * Creating a Request
    ListChangeSets (..),
    newListChangeSets,

    -- * Request Lenses
    listChangeSets_nextToken,
    listChangeSets_stackName,

    -- * Destructuring the Response
    ListChangeSetsResponse (..),
    newListChangeSetsResponse,

    -- * Response Lenses
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_summaries,
    listChangeSetsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListChangeSets action.
--
-- /See:/ 'newListChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { -- | A string (provided by the ListChangeSets response output) that
    -- identifies the next page of change sets that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the stack for which you
    -- want to list change sets.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangeSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChangeSets_nextToken' - A string (provided by the ListChangeSets response output) that
-- identifies the next page of change sets that you want to retrieve.
--
-- 'stackName', 'listChangeSets_stackName' - The name or the Amazon Resource Name (ARN) of the stack for which you
-- want to list change sets.
newListChangeSets ::
  -- | 'stackName'
  Prelude.Text ->
  ListChangeSets
newListChangeSets pStackName_ =
  ListChangeSets'
    { nextToken = Prelude.Nothing,
      stackName = pStackName_
    }

-- | A string (provided by the ListChangeSets response output) that
-- identifies the next page of change sets that you want to retrieve.
listChangeSets_nextToken :: Lens.Lens' ListChangeSets (Prelude.Maybe Prelude.Text)
listChangeSets_nextToken = Lens.lens (\ListChangeSets' {nextToken} -> nextToken) (\s@ListChangeSets' {} a -> s {nextToken = a} :: ListChangeSets)

-- | The name or the Amazon Resource Name (ARN) of the stack for which you
-- want to list change sets.
listChangeSets_stackName :: Lens.Lens' ListChangeSets Prelude.Text
listChangeSets_stackName = Lens.lens (\ListChangeSets' {stackName} -> stackName) (\s@ListChangeSets' {} a -> s {stackName = a} :: ListChangeSets)

instance Core.AWSPager ListChangeSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChangeSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listChangeSetsResponse_summaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listChangeSets_nextToken
          Lens..~ rs
          Lens.^? listChangeSetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListChangeSets where
  type
    AWSResponse ListChangeSets =
      ListChangeSetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListChangeSetsResult"
      ( \s h x ->
          ListChangeSetsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "Summaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChangeSets where
  hashWithSalt _salt ListChangeSets' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData ListChangeSets where
  rnf ListChangeSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders ListChangeSets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListChangeSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListChangeSets where
  toQuery ListChangeSets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListChangeSets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "StackName" Data.=: stackName
      ]

-- | The output for the ListChangeSets action.
--
-- /See:/ 'newListChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- change sets. If there is no additional page, this value is @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @ChangeSetSummary@ structures that provides the ID and status
    -- of each change set for the specified stack.
    summaries :: Prelude.Maybe [ChangeSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangeSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChangeSetsResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- change sets. If there is no additional page, this value is @null@.
--
-- 'summaries', 'listChangeSetsResponse_summaries' - A list of @ChangeSetSummary@ structures that provides the ID and status
-- of each change set for the specified stack.
--
-- 'httpStatus', 'listChangeSetsResponse_httpStatus' - The response's http status code.
newListChangeSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChangeSetsResponse
newListChangeSetsResponse pHttpStatus_ =
  ListChangeSetsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- change sets. If there is no additional page, this value is @null@.
listChangeSetsResponse_nextToken :: Lens.Lens' ListChangeSetsResponse (Prelude.Maybe Prelude.Text)
listChangeSetsResponse_nextToken = Lens.lens (\ListChangeSetsResponse' {nextToken} -> nextToken) (\s@ListChangeSetsResponse' {} a -> s {nextToken = a} :: ListChangeSetsResponse)

-- | A list of @ChangeSetSummary@ structures that provides the ID and status
-- of each change set for the specified stack.
listChangeSetsResponse_summaries :: Lens.Lens' ListChangeSetsResponse (Prelude.Maybe [ChangeSetSummary])
listChangeSetsResponse_summaries = Lens.lens (\ListChangeSetsResponse' {summaries} -> summaries) (\s@ListChangeSetsResponse' {} a -> s {summaries = a} :: ListChangeSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listChangeSetsResponse_httpStatus :: Lens.Lens' ListChangeSetsResponse Prelude.Int
listChangeSetsResponse_httpStatus = Lens.lens (\ListChangeSetsResponse' {httpStatus} -> httpStatus) (\s@ListChangeSetsResponse' {} a -> s {httpStatus = a} :: ListChangeSetsResponse)

instance Prelude.NFData ListChangeSetsResponse where
  rnf ListChangeSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus
