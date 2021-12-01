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
-- Module      : Amazonka.CloudDirectory.ListObjectParents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists parent objects that are associated with a given object in
-- pagination fashion.
module Amazonka.CloudDirectory.ListObjectParents
  ( -- * Creating a Request
    ListObjectParents (..),
    newListObjectParents,

    -- * Request Lenses
    listObjectParents_consistencyLevel,
    listObjectParents_includeAllLinksToEachParent,
    listObjectParents_nextToken,
    listObjectParents_maxResults,
    listObjectParents_directoryArn,
    listObjectParents_objectReference,

    -- * Destructuring the Response
    ListObjectParentsResponse (..),
    newListObjectParentsResponse,

    -- * Response Lenses
    listObjectParentsResponse_nextToken,
    listObjectParentsResponse_parents,
    listObjectParentsResponse_parentLinks,
    listObjectParentsResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObjectParents' smart constructor.
data ListObjectParents = ListObjectParents'
  { -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | When set to True, returns all ListObjectParentsResponse$ParentLinks.
    -- There could be multiple links between a parent-child pair.
    includeAllLinksToEachParent :: Prelude.Maybe Prelude.Bool,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that identifies the object for which parent objects are
    -- being listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectParents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'listObjectParents_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'includeAllLinksToEachParent', 'listObjectParents_includeAllLinksToEachParent' - When set to True, returns all ListObjectParentsResponse$ParentLinks.
-- There could be multiple links between a parent-child pair.
--
-- 'nextToken', 'listObjectParents_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectParents_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'directoryArn', 'listObjectParents_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'listObjectParents_objectReference' - The reference that identifies the object for which parent objects are
-- being listed.
newListObjectParents ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectParents
newListObjectParents pDirectoryArn_ pObjectReference_ =
  ListObjectParents'
    { consistencyLevel =
        Prelude.Nothing,
      includeAllLinksToEachParent = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      objectReference = pObjectReference_
    }

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectParents_consistencyLevel :: Lens.Lens' ListObjectParents (Prelude.Maybe ConsistencyLevel)
listObjectParents_consistencyLevel = Lens.lens (\ListObjectParents' {consistencyLevel} -> consistencyLevel) (\s@ListObjectParents' {} a -> s {consistencyLevel = a} :: ListObjectParents)

-- | When set to True, returns all ListObjectParentsResponse$ParentLinks.
-- There could be multiple links between a parent-child pair.
listObjectParents_includeAllLinksToEachParent :: Lens.Lens' ListObjectParents (Prelude.Maybe Prelude.Bool)
listObjectParents_includeAllLinksToEachParent = Lens.lens (\ListObjectParents' {includeAllLinksToEachParent} -> includeAllLinksToEachParent) (\s@ListObjectParents' {} a -> s {includeAllLinksToEachParent = a} :: ListObjectParents)

-- | The pagination token.
listObjectParents_nextToken :: Lens.Lens' ListObjectParents (Prelude.Maybe Prelude.Text)
listObjectParents_nextToken = Lens.lens (\ListObjectParents' {nextToken} -> nextToken) (\s@ListObjectParents' {} a -> s {nextToken = a} :: ListObjectParents)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectParents_maxResults :: Lens.Lens' ListObjectParents (Prelude.Maybe Prelude.Natural)
listObjectParents_maxResults = Lens.lens (\ListObjectParents' {maxResults} -> maxResults) (\s@ListObjectParents' {} a -> s {maxResults = a} :: ListObjectParents)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectParents_directoryArn :: Lens.Lens' ListObjectParents Prelude.Text
listObjectParents_directoryArn = Lens.lens (\ListObjectParents' {directoryArn} -> directoryArn) (\s@ListObjectParents' {} a -> s {directoryArn = a} :: ListObjectParents)

-- | The reference that identifies the object for which parent objects are
-- being listed.
listObjectParents_objectReference :: Lens.Lens' ListObjectParents ObjectReference
listObjectParents_objectReference = Lens.lens (\ListObjectParents' {objectReference} -> objectReference) (\s@ListObjectParents' {} a -> s {objectReference = a} :: ListObjectParents)

instance Core.AWSRequest ListObjectParents where
  type
    AWSResponse ListObjectParents =
      ListObjectParentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectParentsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Parents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ParentLinks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectParents where
  hashWithSalt salt' ListObjectParents' {..} =
    salt' `Prelude.hashWithSalt` objectReference
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` includeAllLinksToEachParent
      `Prelude.hashWithSalt` consistencyLevel

instance Prelude.NFData ListObjectParents where
  rnf ListObjectParents' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf objectReference
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf includeAllLinksToEachParent

instance Core.ToHeaders ListObjectParents where
  toHeaders ListObjectParents' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectParents where
  toJSON ListObjectParents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeAllLinksToEachParent" Core..=)
              Prelude.<$> includeAllLinksToEachParent,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectParents where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/parent"

instance Core.ToQuery ListObjectParents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObjectParentsResponse' smart constructor.
data ListObjectParentsResponse = ListObjectParentsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parent structure, which is a map with key as the @ObjectIdentifier@
    -- and LinkName as the value.
    parents :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Returns a list of parent reference and LinkName Tuples.
    parentLinks :: Prelude.Maybe [ObjectIdentifierAndLinkNameTuple],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectParentsResponse_nextToken' - The pagination token.
--
-- 'parents', 'listObjectParentsResponse_parents' - The parent structure, which is a map with key as the @ObjectIdentifier@
-- and LinkName as the value.
--
-- 'parentLinks', 'listObjectParentsResponse_parentLinks' - Returns a list of parent reference and LinkName Tuples.
--
-- 'httpStatus', 'listObjectParentsResponse_httpStatus' - The response's http status code.
newListObjectParentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectParentsResponse
newListObjectParentsResponse pHttpStatus_ =
  ListObjectParentsResponse'
    { nextToken =
        Prelude.Nothing,
      parents = Prelude.Nothing,
      parentLinks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectParentsResponse_nextToken :: Lens.Lens' ListObjectParentsResponse (Prelude.Maybe Prelude.Text)
listObjectParentsResponse_nextToken = Lens.lens (\ListObjectParentsResponse' {nextToken} -> nextToken) (\s@ListObjectParentsResponse' {} a -> s {nextToken = a} :: ListObjectParentsResponse)

-- | The parent structure, which is a map with key as the @ObjectIdentifier@
-- and LinkName as the value.
listObjectParentsResponse_parents :: Lens.Lens' ListObjectParentsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listObjectParentsResponse_parents = Lens.lens (\ListObjectParentsResponse' {parents} -> parents) (\s@ListObjectParentsResponse' {} a -> s {parents = a} :: ListObjectParentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns a list of parent reference and LinkName Tuples.
listObjectParentsResponse_parentLinks :: Lens.Lens' ListObjectParentsResponse (Prelude.Maybe [ObjectIdentifierAndLinkNameTuple])
listObjectParentsResponse_parentLinks = Lens.lens (\ListObjectParentsResponse' {parentLinks} -> parentLinks) (\s@ListObjectParentsResponse' {} a -> s {parentLinks = a} :: ListObjectParentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listObjectParentsResponse_httpStatus :: Lens.Lens' ListObjectParentsResponse Prelude.Int
listObjectParentsResponse_httpStatus = Lens.lens (\ListObjectParentsResponse' {httpStatus} -> httpStatus) (\s@ListObjectParentsResponse' {} a -> s {httpStatus = a} :: ListObjectParentsResponse)

instance Prelude.NFData ListObjectParentsResponse where
  rnf ListObjectParentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf parentLinks
      `Prelude.seq` Prelude.rnf parents
