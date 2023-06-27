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
-- Module      : Amazonka.RAM.ListReplacePermissionAssociationsWork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of the asynchronous tasks performed by RAM
-- when you perform the ReplacePermissionAssociationsWork operation.
module Amazonka.RAM.ListReplacePermissionAssociationsWork
  ( -- * Creating a Request
    ListReplacePermissionAssociationsWork (..),
    newListReplacePermissionAssociationsWork,

    -- * Request Lenses
    listReplacePermissionAssociationsWork_maxResults,
    listReplacePermissionAssociationsWork_nextToken,
    listReplacePermissionAssociationsWork_status,
    listReplacePermissionAssociationsWork_workIds,

    -- * Destructuring the Response
    ListReplacePermissionAssociationsWorkResponse (..),
    newListReplacePermissionAssociationsWorkResponse,

    -- * Response Lenses
    listReplacePermissionAssociationsWorkResponse_nextToken,
    listReplacePermissionAssociationsWorkResponse_replacePermissionAssociationsWorks,
    listReplacePermissionAssociationsWorkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReplacePermissionAssociationsWork' smart constructor.
data ListReplacePermissionAssociationsWork = ListReplacePermissionAssociationsWork'
  { -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to see only the details about requests with a
    -- status that matches this value.
    status :: Prelude.Maybe ReplacePermissionAssociationsWorkStatus,
    -- | A list of IDs. These values come from the @id@field of the
    -- @replacePermissionAssociationsWork@structure returned by the
    -- ReplacePermissionAssociations operation.
    workIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReplacePermissionAssociationsWork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listReplacePermissionAssociationsWork_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'listReplacePermissionAssociationsWork_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'status', 'listReplacePermissionAssociationsWork_status' - Specifies that you want to see only the details about requests with a
-- status that matches this value.
--
-- 'workIds', 'listReplacePermissionAssociationsWork_workIds' - A list of IDs. These values come from the @id@field of the
-- @replacePermissionAssociationsWork@structure returned by the
-- ReplacePermissionAssociations operation.
newListReplacePermissionAssociationsWork ::
  ListReplacePermissionAssociationsWork
newListReplacePermissionAssociationsWork =
  ListReplacePermissionAssociationsWork'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      workIds = Prelude.Nothing
    }

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listReplacePermissionAssociationsWork_maxResults :: Lens.Lens' ListReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Natural)
listReplacePermissionAssociationsWork_maxResults = Lens.lens (\ListReplacePermissionAssociationsWork' {maxResults} -> maxResults) (\s@ListReplacePermissionAssociationsWork' {} a -> s {maxResults = a} :: ListReplacePermissionAssociationsWork)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listReplacePermissionAssociationsWork_nextToken :: Lens.Lens' ListReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
listReplacePermissionAssociationsWork_nextToken = Lens.lens (\ListReplacePermissionAssociationsWork' {nextToken} -> nextToken) (\s@ListReplacePermissionAssociationsWork' {} a -> s {nextToken = a} :: ListReplacePermissionAssociationsWork)

-- | Specifies that you want to see only the details about requests with a
-- status that matches this value.
listReplacePermissionAssociationsWork_status :: Lens.Lens' ListReplacePermissionAssociationsWork (Prelude.Maybe ReplacePermissionAssociationsWorkStatus)
listReplacePermissionAssociationsWork_status = Lens.lens (\ListReplacePermissionAssociationsWork' {status} -> status) (\s@ListReplacePermissionAssociationsWork' {} a -> s {status = a} :: ListReplacePermissionAssociationsWork)

-- | A list of IDs. These values come from the @id@field of the
-- @replacePermissionAssociationsWork@structure returned by the
-- ReplacePermissionAssociations operation.
listReplacePermissionAssociationsWork_workIds :: Lens.Lens' ListReplacePermissionAssociationsWork (Prelude.Maybe [Prelude.Text])
listReplacePermissionAssociationsWork_workIds = Lens.lens (\ListReplacePermissionAssociationsWork' {workIds} -> workIds) (\s@ListReplacePermissionAssociationsWork' {} a -> s {workIds = a} :: ListReplacePermissionAssociationsWork) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    ListReplacePermissionAssociationsWork
  where
  type
    AWSResponse
      ListReplacePermissionAssociationsWork =
      ListReplacePermissionAssociationsWorkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReplacePermissionAssociationsWorkResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "replacePermissionAssociationsWorks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListReplacePermissionAssociationsWork
  where
  hashWithSalt
    _salt
    ListReplacePermissionAssociationsWork' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` workIds

instance
  Prelude.NFData
    ListReplacePermissionAssociationsWork
  where
  rnf ListReplacePermissionAssociationsWork' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workIds

instance
  Data.ToHeaders
    ListReplacePermissionAssociationsWork
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListReplacePermissionAssociationsWork
  where
  toJSON ListReplacePermissionAssociationsWork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("status" Data..=) Prelude.<$> status,
            ("workIds" Data..=) Prelude.<$> workIds
          ]
      )

instance
  Data.ToPath
    ListReplacePermissionAssociationsWork
  where
  toPath =
    Prelude.const
      "/listreplacepermissionassociationswork"

instance
  Data.ToQuery
    ListReplacePermissionAssociationsWork
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReplacePermissionAssociationsWorkResponse' smart constructor.
data ListReplacePermissionAssociationsWorkResponse = ListReplacePermissionAssociationsWorkResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of data structures that provide details of the matching work
    -- IDs.
    replacePermissionAssociationsWorks :: Prelude.Maybe [ReplacePermissionAssociationsWork],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReplacePermissionAssociationsWorkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReplacePermissionAssociationsWorkResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'replacePermissionAssociationsWorks', 'listReplacePermissionAssociationsWorkResponse_replacePermissionAssociationsWorks' - An array of data structures that provide details of the matching work
-- IDs.
--
-- 'httpStatus', 'listReplacePermissionAssociationsWorkResponse_httpStatus' - The response's http status code.
newListReplacePermissionAssociationsWorkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReplacePermissionAssociationsWorkResponse
newListReplacePermissionAssociationsWorkResponse
  pHttpStatus_ =
    ListReplacePermissionAssociationsWorkResponse'
      { nextToken =
          Prelude.Nothing,
        replacePermissionAssociationsWorks =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listReplacePermissionAssociationsWorkResponse_nextToken :: Lens.Lens' ListReplacePermissionAssociationsWorkResponse (Prelude.Maybe Prelude.Text)
listReplacePermissionAssociationsWorkResponse_nextToken = Lens.lens (\ListReplacePermissionAssociationsWorkResponse' {nextToken} -> nextToken) (\s@ListReplacePermissionAssociationsWorkResponse' {} a -> s {nextToken = a} :: ListReplacePermissionAssociationsWorkResponse)

-- | An array of data structures that provide details of the matching work
-- IDs.
listReplacePermissionAssociationsWorkResponse_replacePermissionAssociationsWorks :: Lens.Lens' ListReplacePermissionAssociationsWorkResponse (Prelude.Maybe [ReplacePermissionAssociationsWork])
listReplacePermissionAssociationsWorkResponse_replacePermissionAssociationsWorks = Lens.lens (\ListReplacePermissionAssociationsWorkResponse' {replacePermissionAssociationsWorks} -> replacePermissionAssociationsWorks) (\s@ListReplacePermissionAssociationsWorkResponse' {} a -> s {replacePermissionAssociationsWorks = a} :: ListReplacePermissionAssociationsWorkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReplacePermissionAssociationsWorkResponse_httpStatus :: Lens.Lens' ListReplacePermissionAssociationsWorkResponse Prelude.Int
listReplacePermissionAssociationsWorkResponse_httpStatus = Lens.lens (\ListReplacePermissionAssociationsWorkResponse' {httpStatus} -> httpStatus) (\s@ListReplacePermissionAssociationsWorkResponse' {} a -> s {httpStatus = a} :: ListReplacePermissionAssociationsWorkResponse)

instance
  Prelude.NFData
    ListReplacePermissionAssociationsWorkResponse
  where
  rnf
    ListReplacePermissionAssociationsWorkResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf replacePermissionAssociationsWorks
        `Prelude.seq` Prelude.rnf httpStatus
