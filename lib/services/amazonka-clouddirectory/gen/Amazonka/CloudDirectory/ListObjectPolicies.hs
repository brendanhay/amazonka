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
-- Module      : Amazonka.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListObjectPolicies
  ( -- * Creating a Request
    ListObjectPolicies (..),
    newListObjectPolicies,

    -- * Request Lenses
    listObjectPolicies_consistencyLevel,
    listObjectPolicies_maxResults,
    listObjectPolicies_nextToken,
    listObjectPolicies_directoryArn,
    listObjectPolicies_objectReference,

    -- * Destructuring the Response
    ListObjectPoliciesResponse (..),
    newListObjectPoliciesResponse,

    -- * Response Lenses
    listObjectPoliciesResponse_attachedPolicyIds,
    listObjectPoliciesResponse_nextToken,
    listObjectPoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
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
    -- where objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | Reference that identifies the object for which policies will be listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'listObjectPolicies_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'maxResults', 'listObjectPolicies_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'nextToken', 'listObjectPolicies_nextToken' - The pagination token.
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
listObjectPolicies_consistencyLevel :: Lens.Lens' ListObjectPolicies (Prelude.Maybe ConsistencyLevel)
listObjectPolicies_consistencyLevel = Lens.lens (\ListObjectPolicies' {consistencyLevel} -> consistencyLevel) (\s@ListObjectPolicies' {} a -> s {consistencyLevel = a} :: ListObjectPolicies)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectPolicies_maxResults :: Lens.Lens' ListObjectPolicies (Prelude.Maybe Prelude.Natural)
listObjectPolicies_maxResults = Lens.lens (\ListObjectPolicies' {maxResults} -> maxResults) (\s@ListObjectPolicies' {} a -> s {maxResults = a} :: ListObjectPolicies)

-- | The pagination token.
listObjectPolicies_nextToken :: Lens.Lens' ListObjectPolicies (Prelude.Maybe Prelude.Text)
listObjectPolicies_nextToken = Lens.lens (\ListObjectPolicies' {nextToken} -> nextToken) (\s@ListObjectPolicies' {} a -> s {nextToken = a} :: ListObjectPolicies)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
listObjectPolicies_directoryArn :: Lens.Lens' ListObjectPolicies Prelude.Text
listObjectPolicies_directoryArn = Lens.lens (\ListObjectPolicies' {directoryArn} -> directoryArn) (\s@ListObjectPolicies' {} a -> s {directoryArn = a} :: ListObjectPolicies)

-- | Reference that identifies the object for which policies will be listed.
listObjectPolicies_objectReference :: Lens.Lens' ListObjectPolicies ObjectReference
listObjectPolicies_objectReference = Lens.lens (\ListObjectPolicies' {objectReference} -> objectReference) (\s@ListObjectPolicies' {} a -> s {objectReference = a} :: ListObjectPolicies)

instance Core.AWSPager ListObjectPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_attachedPolicyIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listObjectPolicies_nextToken
          Lens..~ rs
          Lens.^? listObjectPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListObjectPolicies where
  type
    AWSResponse ListObjectPolicies =
      ListObjectPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectPoliciesResponse'
            Prelude.<$> ( x Data..?> "AttachedPolicyIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectPolicies where
  hashWithSalt _salt ListObjectPolicies' {..} =
    _salt `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData ListObjectPolicies where
  rnf ListObjectPolicies' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToHeaders ListObjectPolicies where
  toHeaders ListObjectPolicies' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Data.=# consistencyLevel,
        "x-amz-data-partition" Data.=# directoryArn
      ]

instance Data.ToJSON ListObjectPolicies where
  toJSON ListObjectPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )

instance Data.ToPath ListObjectPolicies where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/policy"

instance Data.ToQuery ListObjectPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { -- | A list of policy @ObjectIdentifiers@, that are attached to the object.
    attachedPolicyIds :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedPolicyIds', 'listObjectPoliciesResponse_attachedPolicyIds' - A list of policy @ObjectIdentifiers@, that are attached to the object.
--
-- 'nextToken', 'listObjectPoliciesResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listObjectPoliciesResponse_httpStatus' - The response's http status code.
newListObjectPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectPoliciesResponse
newListObjectPoliciesResponse pHttpStatus_ =
  ListObjectPoliciesResponse'
    { attachedPolicyIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of policy @ObjectIdentifiers@, that are attached to the object.
listObjectPoliciesResponse_attachedPolicyIds :: Lens.Lens' ListObjectPoliciesResponse (Prelude.Maybe [Prelude.Text])
listObjectPoliciesResponse_attachedPolicyIds = Lens.lens (\ListObjectPoliciesResponse' {attachedPolicyIds} -> attachedPolicyIds) (\s@ListObjectPoliciesResponse' {} a -> s {attachedPolicyIds = a} :: ListObjectPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listObjectPoliciesResponse_nextToken :: Lens.Lens' ListObjectPoliciesResponse (Prelude.Maybe Prelude.Text)
listObjectPoliciesResponse_nextToken = Lens.lens (\ListObjectPoliciesResponse' {nextToken} -> nextToken) (\s@ListObjectPoliciesResponse' {} a -> s {nextToken = a} :: ListObjectPoliciesResponse)

-- | The response's http status code.
listObjectPoliciesResponse_httpStatus :: Lens.Lens' ListObjectPoliciesResponse Prelude.Int
listObjectPoliciesResponse_httpStatus = Lens.lens (\ListObjectPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListObjectPoliciesResponse' {} a -> s {httpStatus = a} :: ListObjectPoliciesResponse)

instance Prelude.NFData ListObjectPoliciesResponse where
  rnf ListObjectPoliciesResponse' {..} =
    Prelude.rnf attachedPolicyIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
