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
-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Select@ operation returns a set of attributes for @ItemNames@ that
-- match the select expression. @Select@ is similar to the standard SQL
-- SELECT statement.
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon
-- SimpleDB automatically adjusts the number of items returned per page to
-- enforce this limit. For example, if the client asks to retrieve 2500
-- items, but each individual item is 10 kB in size, the system returns 100
-- items and an appropriate @NextToken@ so the client can access the next
-- page of results.
--
-- For information on how to construct select expressions, see Using Select
-- to Create Amazon SimpleDB Queries in the Developer Guide.
--
-- This operation returns paginated results.
module Network.AWS.SDB.Select
  ( -- * Creating a Request
    Select (..),
    newSelect,

    -- * Request Lenses
    select_nextToken,
    select_consistentRead,
    select_selectExpression,

    -- * Destructuring the Response
    SelectResponse (..),
    newSelectResponse,

    -- * Response Lenses
    selectResponse_nextToken,
    selectResponse_items,
    selectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newSelect' smart constructor.
data Select = Select'
  { -- | A string informing Amazon SimpleDB where to start the next list of
    -- @ItemNames@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Determines whether or not strong consistency should be enforced when
    -- data is read from SimpleDB. If @true@, any data previously written to
    -- SimpleDB will be returned. Otherwise, results will be consistent
    -- eventually, and the client may not see data that was written immediately
    -- before your read.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | The expression used to query the domain.
    selectExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Select' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'select_nextToken' - A string informing Amazon SimpleDB where to start the next list of
-- @ItemNames@.
--
-- 'consistentRead', 'select_consistentRead' - Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
--
-- 'selectExpression', 'select_selectExpression' - The expression used to query the domain.
newSelect ::
  -- | 'selectExpression'
  Prelude.Text ->
  Select
newSelect pSelectExpression_ =
  Select'
    { nextToken = Prelude.Nothing,
      consistentRead = Prelude.Nothing,
      selectExpression = pSelectExpression_
    }

-- | A string informing Amazon SimpleDB where to start the next list of
-- @ItemNames@.
select_nextToken :: Lens.Lens' Select (Prelude.Maybe Prelude.Text)
select_nextToken = Lens.lens (\Select' {nextToken} -> nextToken) (\s@Select' {} a -> s {nextToken = a} :: Select)

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
select_consistentRead :: Lens.Lens' Select (Prelude.Maybe Prelude.Bool)
select_consistentRead = Lens.lens (\Select' {consistentRead} -> consistentRead) (\s@Select' {} a -> s {consistentRead = a} :: Select)

-- | The expression used to query the domain.
select_selectExpression :: Lens.Lens' Select Prelude.Text
select_selectExpression = Lens.lens (\Select' {selectExpression} -> selectExpression) (\s@Select' {} a -> s {selectExpression = a} :: Select)

instance Core.AWSPager Select where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? selectResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? selectResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& select_nextToken
          Lens..~ rs
          Lens.^? selectResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest Select where
  type AWSResponse Select = SelectResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SelectResult"
      ( \s h x ->
          SelectResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> (Core.may (Core.parseXMLList "Item") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Select

instance Prelude.NFData Select

instance Core.ToHeaders Select where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath Select where
  toPath = Prelude.const "/"

instance Core.ToQuery Select where
  toQuery Select' {..} =
    Prelude.mconcat
      [ "Action" Core.=: ("Select" :: Prelude.ByteString),
        "Version"
          Core.=: ("2009-04-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "ConsistentRead" Core.=: consistentRead,
        "SelectExpression" Core.=: selectExpression
      ]

-- | /See:/ 'newSelectResponse' smart constructor.
data SelectResponse = SelectResponse'
  { -- | An opaque token indicating that more items than @MaxNumberOfItems@ were
    -- matched, the response size exceeded 1 megabyte, or the execution time
    -- exceeded 5 seconds.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of items that match the select expression.
    items :: Prelude.Maybe [Item],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'selectResponse_nextToken' - An opaque token indicating that more items than @MaxNumberOfItems@ were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
--
-- 'items', 'selectResponse_items' - A list of items that match the select expression.
--
-- 'httpStatus', 'selectResponse_httpStatus' - The response's http status code.
newSelectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SelectResponse
newSelectResponse pHttpStatus_ =
  SelectResponse'
    { nextToken = Prelude.Nothing,
      items = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque token indicating that more items than @MaxNumberOfItems@ were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
selectResponse_nextToken :: Lens.Lens' SelectResponse (Prelude.Maybe Prelude.Text)
selectResponse_nextToken = Lens.lens (\SelectResponse' {nextToken} -> nextToken) (\s@SelectResponse' {} a -> s {nextToken = a} :: SelectResponse)

-- | A list of items that match the select expression.
selectResponse_items :: Lens.Lens' SelectResponse (Prelude.Maybe [Item])
selectResponse_items = Lens.lens (\SelectResponse' {items} -> items) (\s@SelectResponse' {} a -> s {items = a} :: SelectResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
selectResponse_httpStatus :: Lens.Lens' SelectResponse Prelude.Int
selectResponse_httpStatus = Lens.lens (\SelectResponse' {httpStatus} -> httpStatus) (\s@SelectResponse' {} a -> s {httpStatus = a} :: SelectResponse)

instance Prelude.NFData SelectResponse
