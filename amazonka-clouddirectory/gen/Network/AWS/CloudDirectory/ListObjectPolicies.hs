{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectPolicies
  ( -- * Creating a Request
    ListObjectPolicies (..),
    newListObjectPolicies,

    -- * Request Lenses
    listObjectPolicies_nextToken,
    listObjectPolicies_maxResults,
    listObjectPolicies_consistencyLevel,
    listObjectPolicies_directoryArn,
    listObjectPolicies_objectReference,

    -- * Destructuring the Response
    ListObjectPoliciesResponse (..),
    newListObjectPoliciesResponse,

    -- * Response Lenses
    listObjectPoliciesResponse_nextToken,
    listObjectPoliciesResponse_attachedPolicyIds,
    listObjectPoliciesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | Reference that identifies the object for which policies will be listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListObjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectPolicies_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectPolicies_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'consistencyLevel', 'listObjectPolicies_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'listObjectPolicies_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
--
-- 'objectReference', 'listObjectPolicies_objectReference' - Reference that identifies the object for which policies will be listed.
newListObjectPolicies ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectPolicies
newListObjectPolicies
  pDirectoryArn_
  pObjectReference_ =
    ListObjectPolicies'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        consistencyLevel = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectPolicies_nextToken :: Lens.Lens' ListObjectPolicies (Prelude.Maybe Prelude.Text)
listObjectPolicies_nextToken = Lens.lens (\ListObjectPolicies' {nextToken} -> nextToken) (\s@ListObjectPolicies' {} a -> s {nextToken = a} :: ListObjectPolicies)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectPolicies_maxResults :: Lens.Lens' ListObjectPolicies (Prelude.Maybe Prelude.Natural)
listObjectPolicies_maxResults = Lens.lens (\ListObjectPolicies' {maxResults} -> maxResults) (\s@ListObjectPolicies' {} a -> s {maxResults = a} :: ListObjectPolicies)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectPolicies_consistencyLevel :: Lens.Lens' ListObjectPolicies (Prelude.Maybe ConsistencyLevel)
listObjectPolicies_consistencyLevel = Lens.lens (\ListObjectPolicies' {consistencyLevel} -> consistencyLevel) (\s@ListObjectPolicies' {} a -> s {consistencyLevel = a} :: ListObjectPolicies)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
listObjectPolicies_directoryArn :: Lens.Lens' ListObjectPolicies Prelude.Text
listObjectPolicies_directoryArn = Lens.lens (\ListObjectPolicies' {directoryArn} -> directoryArn) (\s@ListObjectPolicies' {} a -> s {directoryArn = a} :: ListObjectPolicies)

-- | Reference that identifies the object for which policies will be listed.
listObjectPolicies_objectReference :: Lens.Lens' ListObjectPolicies ObjectReference
listObjectPolicies_objectReference = Lens.lens (\ListObjectPolicies' {objectReference} -> objectReference) (\s@ListObjectPolicies' {} a -> s {objectReference = a} :: ListObjectPolicies)

instance Pager.AWSPager ListObjectPolicies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_attachedPolicyIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listObjectPolicies_nextToken
          Lens..~ rs
          Lens.^? listObjectPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListObjectPolicies where
  type
    Rs ListObjectPolicies =
      ListObjectPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectPoliciesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AttachedPolicyIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectPolicies

instance Prelude.NFData ListObjectPolicies

instance Prelude.ToHeaders ListObjectPolicies where
  toHeaders ListObjectPolicies' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level"
          Prelude.=# consistencyLevel,
        "x-amz-data-partition" Prelude.=# directoryArn
      ]

instance Prelude.ToJSON ListObjectPolicies where
  toJSON ListObjectPolicies' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Prelude..= objectReference)
          ]
      )

instance Prelude.ToPath ListObjectPolicies where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/policy"

instance Prelude.ToQuery ListObjectPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of policy @ObjectIdentifiers@, that are attached to the object.
    attachedPolicyIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListObjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectPoliciesResponse_nextToken' - The pagination token.
--
-- 'attachedPolicyIds', 'listObjectPoliciesResponse_attachedPolicyIds' - A list of policy @ObjectIdentifiers@, that are attached to the object.
--
-- 'httpStatus', 'listObjectPoliciesResponse_httpStatus' - The response's http status code.
newListObjectPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectPoliciesResponse
newListObjectPoliciesResponse pHttpStatus_ =
  ListObjectPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      attachedPolicyIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectPoliciesResponse_nextToken :: Lens.Lens' ListObjectPoliciesResponse (Prelude.Maybe Prelude.Text)
listObjectPoliciesResponse_nextToken = Lens.lens (\ListObjectPoliciesResponse' {nextToken} -> nextToken) (\s@ListObjectPoliciesResponse' {} a -> s {nextToken = a} :: ListObjectPoliciesResponse)

-- | A list of policy @ObjectIdentifiers@, that are attached to the object.
listObjectPoliciesResponse_attachedPolicyIds :: Lens.Lens' ListObjectPoliciesResponse (Prelude.Maybe [Prelude.Text])
listObjectPoliciesResponse_attachedPolicyIds = Lens.lens (\ListObjectPoliciesResponse' {attachedPolicyIds} -> attachedPolicyIds) (\s@ListObjectPoliciesResponse' {} a -> s {attachedPolicyIds = a} :: ListObjectPoliciesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listObjectPoliciesResponse_httpStatus :: Lens.Lens' ListObjectPoliciesResponse Prelude.Int
listObjectPoliciesResponse_httpStatus = Lens.lens (\ListObjectPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListObjectPoliciesResponse' {} a -> s {httpStatus = a} :: ListObjectPoliciesResponse)

instance Prelude.NFData ListObjectPoliciesResponse
