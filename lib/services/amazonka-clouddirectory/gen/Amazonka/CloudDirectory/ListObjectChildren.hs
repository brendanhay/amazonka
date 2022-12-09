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
-- Module      : Amazonka.CloudDirectory.ListObjectChildren
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of child objects that are associated with a
-- given object.
module Amazonka.CloudDirectory.ListObjectChildren
  ( -- * Creating a Request
    ListObjectChildren (..),
    newListObjectChildren,

    -- * Request Lenses
    listObjectChildren_consistencyLevel,
    listObjectChildren_maxResults,
    listObjectChildren_nextToken,
    listObjectChildren_directoryArn,
    listObjectChildren_objectReference,

    -- * Destructuring the Response
    ListObjectChildrenResponse (..),
    newListObjectChildrenResponse,

    -- * Response Lenses
    listObjectChildrenResponse_children,
    listObjectChildrenResponse_nextToken,
    listObjectChildrenResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that identifies the object for which child objects are
    -- being listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'listObjectChildren_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'maxResults', 'listObjectChildren_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'nextToken', 'listObjectChildren_nextToken' - The pagination token.
--
-- 'directoryArn', 'listObjectChildren_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'listObjectChildren_objectReference' - The reference that identifies the object for which child objects are
-- being listed.
newListObjectChildren ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectChildren
newListObjectChildren
  pDirectoryArn_
  pObjectReference_ =
    ListObjectChildren'
      { consistencyLevel =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectChildren_consistencyLevel :: Lens.Lens' ListObjectChildren (Prelude.Maybe ConsistencyLevel)
listObjectChildren_consistencyLevel = Lens.lens (\ListObjectChildren' {consistencyLevel} -> consistencyLevel) (\s@ListObjectChildren' {} a -> s {consistencyLevel = a} :: ListObjectChildren)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectChildren_maxResults :: Lens.Lens' ListObjectChildren (Prelude.Maybe Prelude.Natural)
listObjectChildren_maxResults = Lens.lens (\ListObjectChildren' {maxResults} -> maxResults) (\s@ListObjectChildren' {} a -> s {maxResults = a} :: ListObjectChildren)

-- | The pagination token.
listObjectChildren_nextToken :: Lens.Lens' ListObjectChildren (Prelude.Maybe Prelude.Text)
listObjectChildren_nextToken = Lens.lens (\ListObjectChildren' {nextToken} -> nextToken) (\s@ListObjectChildren' {} a -> s {nextToken = a} :: ListObjectChildren)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectChildren_directoryArn :: Lens.Lens' ListObjectChildren Prelude.Text
listObjectChildren_directoryArn = Lens.lens (\ListObjectChildren' {directoryArn} -> directoryArn) (\s@ListObjectChildren' {} a -> s {directoryArn = a} :: ListObjectChildren)

-- | The reference that identifies the object for which child objects are
-- being listed.
listObjectChildren_objectReference :: Lens.Lens' ListObjectChildren ObjectReference
listObjectChildren_objectReference = Lens.lens (\ListObjectChildren' {objectReference} -> objectReference) (\s@ListObjectChildren' {} a -> s {objectReference = a} :: ListObjectChildren)

instance Core.AWSRequest ListObjectChildren where
  type
    AWSResponse ListObjectChildren =
      ListObjectChildrenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectChildrenResponse'
            Prelude.<$> (x Data..?> "Children" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectChildren where
  hashWithSalt _salt ListObjectChildren' {..} =
    _salt `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData ListObjectChildren where
  rnf ListObjectChildren' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToHeaders ListObjectChildren where
  toHeaders ListObjectChildren' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Data.=# consistencyLevel,
        "x-amz-data-partition" Data.=# directoryArn
      ]

instance Data.ToJSON ListObjectChildren where
  toJSON ListObjectChildren' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )

instance Data.ToPath ListObjectChildren where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/children"

instance Data.ToQuery ListObjectChildren where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { -- | Children structure, which is a map with key as the @LinkName@ and
    -- @ObjectIdentifier@ as the value.
    children :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'listObjectChildrenResponse_children' - Children structure, which is a map with key as the @LinkName@ and
-- @ObjectIdentifier@ as the value.
--
-- 'nextToken', 'listObjectChildrenResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listObjectChildrenResponse_httpStatus' - The response's http status code.
newListObjectChildrenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectChildrenResponse
newListObjectChildrenResponse pHttpStatus_ =
  ListObjectChildrenResponse'
    { children =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Children structure, which is a map with key as the @LinkName@ and
-- @ObjectIdentifier@ as the value.
listObjectChildrenResponse_children :: Lens.Lens' ListObjectChildrenResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listObjectChildrenResponse_children = Lens.lens (\ListObjectChildrenResponse' {children} -> children) (\s@ListObjectChildrenResponse' {} a -> s {children = a} :: ListObjectChildrenResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listObjectChildrenResponse_nextToken :: Lens.Lens' ListObjectChildrenResponse (Prelude.Maybe Prelude.Text)
listObjectChildrenResponse_nextToken = Lens.lens (\ListObjectChildrenResponse' {nextToken} -> nextToken) (\s@ListObjectChildrenResponse' {} a -> s {nextToken = a} :: ListObjectChildrenResponse)

-- | The response's http status code.
listObjectChildrenResponse_httpStatus :: Lens.Lens' ListObjectChildrenResponse Prelude.Int
listObjectChildrenResponse_httpStatus = Lens.lens (\ListObjectChildrenResponse' {httpStatus} -> httpStatus) (\s@ListObjectChildrenResponse' {} a -> s {httpStatus = a} :: ListObjectChildrenResponse)

instance Prelude.NFData ListObjectChildrenResponse where
  rnf ListObjectChildrenResponse' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
