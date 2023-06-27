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
-- Module      : Amazonka.CodePipeline.ListActionTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all CodePipeline action types associated with your
-- account.
--
-- This operation returns paginated results.
module Amazonka.CodePipeline.ListActionTypes
  ( -- * Creating a Request
    ListActionTypes (..),
    newListActionTypes,

    -- * Request Lenses
    listActionTypes_actionOwnerFilter,
    listActionTypes_nextToken,
    listActionTypes_regionFilter,

    -- * Destructuring the Response
    ListActionTypesResponse (..),
    newListActionTypesResponse,

    -- * Response Lenses
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListActionTypes@ action.
--
-- /See:/ 'newListActionTypes' smart constructor.
data ListActionTypes = ListActionTypes'
  { -- | Filters the list of action types to those created by a specified entity.
    actionOwnerFilter :: Prelude.Maybe ActionOwner,
    -- | An identifier that was returned from the previous list action types
    -- call, which can be used to return the next set of action types in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Region to filter on for the list of action types.
    regionFilter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActionTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionOwnerFilter', 'listActionTypes_actionOwnerFilter' - Filters the list of action types to those created by a specified entity.
--
-- 'nextToken', 'listActionTypes_nextToken' - An identifier that was returned from the previous list action types
-- call, which can be used to return the next set of action types in the
-- list.
--
-- 'regionFilter', 'listActionTypes_regionFilter' - The Region to filter on for the list of action types.
newListActionTypes ::
  ListActionTypes
newListActionTypes =
  ListActionTypes'
    { actionOwnerFilter =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regionFilter = Prelude.Nothing
    }

-- | Filters the list of action types to those created by a specified entity.
listActionTypes_actionOwnerFilter :: Lens.Lens' ListActionTypes (Prelude.Maybe ActionOwner)
listActionTypes_actionOwnerFilter = Lens.lens (\ListActionTypes' {actionOwnerFilter} -> actionOwnerFilter) (\s@ListActionTypes' {} a -> s {actionOwnerFilter = a} :: ListActionTypes)

-- | An identifier that was returned from the previous list action types
-- call, which can be used to return the next set of action types in the
-- list.
listActionTypes_nextToken :: Lens.Lens' ListActionTypes (Prelude.Maybe Prelude.Text)
listActionTypes_nextToken = Lens.lens (\ListActionTypes' {nextToken} -> nextToken) (\s@ListActionTypes' {} a -> s {nextToken = a} :: ListActionTypes)

-- | The Region to filter on for the list of action types.
listActionTypes_regionFilter :: Lens.Lens' ListActionTypes (Prelude.Maybe Prelude.Text)
listActionTypes_regionFilter = Lens.lens (\ListActionTypes' {regionFilter} -> regionFilter) (\s@ListActionTypes' {} a -> s {regionFilter = a} :: ListActionTypes)

instance Core.AWSPager ListActionTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActionTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listActionTypesResponse_actionTypes) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listActionTypes_nextToken
          Lens..~ rs
          Lens.^? listActionTypesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListActionTypes where
  type
    AWSResponse ListActionTypes =
      ListActionTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "actionTypes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListActionTypes where
  hashWithSalt _salt ListActionTypes' {..} =
    _salt
      `Prelude.hashWithSalt` actionOwnerFilter
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regionFilter

instance Prelude.NFData ListActionTypes where
  rnf ListActionTypes' {..} =
    Prelude.rnf actionOwnerFilter
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regionFilter

instance Data.ToHeaders ListActionTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.ListActionTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListActionTypes where
  toJSON ListActionTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actionOwnerFilter" Data..=)
              Prelude.<$> actionOwnerFilter,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("regionFilter" Data..=) Prelude.<$> regionFilter
          ]
      )

instance Data.ToPath ListActionTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListActionTypes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListActionTypes@ action.
--
-- /See:/ 'newListActionTypesResponse' smart constructor.
data ListActionTypesResponse = ListActionTypesResponse'
  { -- | If the amount of returned information is significantly large, an
    -- identifier is also returned. It can be used in a subsequent list action
    -- types call to return the next set of action types in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides details of the action types.
    actionTypes :: [ActionType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActionTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActionTypesResponse_nextToken' - If the amount of returned information is significantly large, an
-- identifier is also returned. It can be used in a subsequent list action
-- types call to return the next set of action types in the list.
--
-- 'httpStatus', 'listActionTypesResponse_httpStatus' - The response's http status code.
--
-- 'actionTypes', 'listActionTypesResponse_actionTypes' - Provides details of the action types.
newListActionTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActionTypesResponse
newListActionTypesResponse pHttpStatus_ =
  ListActionTypesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      actionTypes = Prelude.mempty
    }

-- | If the amount of returned information is significantly large, an
-- identifier is also returned. It can be used in a subsequent list action
-- types call to return the next set of action types in the list.
listActionTypesResponse_nextToken :: Lens.Lens' ListActionTypesResponse (Prelude.Maybe Prelude.Text)
listActionTypesResponse_nextToken = Lens.lens (\ListActionTypesResponse' {nextToken} -> nextToken) (\s@ListActionTypesResponse' {} a -> s {nextToken = a} :: ListActionTypesResponse)

-- | The response's http status code.
listActionTypesResponse_httpStatus :: Lens.Lens' ListActionTypesResponse Prelude.Int
listActionTypesResponse_httpStatus = Lens.lens (\ListActionTypesResponse' {httpStatus} -> httpStatus) (\s@ListActionTypesResponse' {} a -> s {httpStatus = a} :: ListActionTypesResponse)

-- | Provides details of the action types.
listActionTypesResponse_actionTypes :: Lens.Lens' ListActionTypesResponse [ActionType]
listActionTypesResponse_actionTypes = Lens.lens (\ListActionTypesResponse' {actionTypes} -> actionTypes) (\s@ListActionTypesResponse' {} a -> s {actionTypes = a} :: ListActionTypesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListActionTypesResponse where
  rnf ListActionTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf actionTypes
