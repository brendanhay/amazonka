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
-- Module      : Network.AWS.CodePipeline.ListActionTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all AWS CodePipeline action types associated with your
-- account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionTypes
  ( -- * Creating a Request
    ListActionTypes (..),
    newListActionTypes,

    -- * Request Lenses
    listActionTypes_nextToken,
    listActionTypes_regionFilter,
    listActionTypes_actionOwnerFilter,

    -- * Destructuring the Response
    ListActionTypesResponse (..),
    newListActionTypesResponse,

    -- * Response Lenses
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListActionTypes@ action.
--
-- /See:/ 'newListActionTypes' smart constructor.
data ListActionTypes = ListActionTypes'
  { -- | An identifier that was returned from the previous list action types
    -- call, which can be used to return the next set of action types in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Region to filter on for the list of action types.
    regionFilter :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of action types to those created by a specified entity.
    actionOwnerFilter :: Prelude.Maybe ActionOwner
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
-- 'nextToken', 'listActionTypes_nextToken' - An identifier that was returned from the previous list action types
-- call, which can be used to return the next set of action types in the
-- list.
--
-- 'regionFilter', 'listActionTypes_regionFilter' - The Region to filter on for the list of action types.
--
-- 'actionOwnerFilter', 'listActionTypes_actionOwnerFilter' - Filters the list of action types to those created by a specified entity.
newListActionTypes ::
  ListActionTypes
newListActionTypes =
  ListActionTypes'
    { nextToken = Prelude.Nothing,
      regionFilter = Prelude.Nothing,
      actionOwnerFilter = Prelude.Nothing
    }

-- | An identifier that was returned from the previous list action types
-- call, which can be used to return the next set of action types in the
-- list.
listActionTypes_nextToken :: Lens.Lens' ListActionTypes (Prelude.Maybe Prelude.Text)
listActionTypes_nextToken = Lens.lens (\ListActionTypes' {nextToken} -> nextToken) (\s@ListActionTypes' {} a -> s {nextToken = a} :: ListActionTypes)

-- | The Region to filter on for the list of action types.
listActionTypes_regionFilter :: Lens.Lens' ListActionTypes (Prelude.Maybe Prelude.Text)
listActionTypes_regionFilter = Lens.lens (\ListActionTypes' {regionFilter} -> regionFilter) (\s@ListActionTypes' {} a -> s {regionFilter = a} :: ListActionTypes)

-- | Filters the list of action types to those created by a specified entity.
listActionTypes_actionOwnerFilter :: Lens.Lens' ListActionTypes (Prelude.Maybe ActionOwner)
listActionTypes_actionOwnerFilter = Lens.lens (\ListActionTypes' {actionOwnerFilter} -> actionOwnerFilter) (\s@ListActionTypes' {} a -> s {actionOwnerFilter = a} :: ListActionTypes)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listActionTypes_nextToken
          Lens..~ rs
          Lens.^? listActionTypesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListActionTypes where
  type
    AWSResponse ListActionTypes =
      ListActionTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionTypesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "actionTypes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListActionTypes

instance Prelude.NFData ListActionTypes

instance Core.ToHeaders ListActionTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.ListActionTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListActionTypes where
  toJSON ListActionTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("regionFilter" Core..=) Prelude.<$> regionFilter,
            ("actionOwnerFilter" Core..=)
              Prelude.<$> actionOwnerFilter
          ]
      )

instance Core.ToPath ListActionTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListActionTypes where
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
listActionTypesResponse_actionTypes = Lens.lens (\ListActionTypesResponse' {actionTypes} -> actionTypes) (\s@ListActionTypesResponse' {} a -> s {actionTypes = a} :: ListActionTypesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListActionTypesResponse
