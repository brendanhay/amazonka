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
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
  ( -- * Creating a Request
    ListPolicyAttachments (..),
    newListPolicyAttachments,

    -- * Request Lenses
    listPolicyAttachments_consistencyLevel,
    listPolicyAttachments_nextToken,
    listPolicyAttachments_maxResults,
    listPolicyAttachments_directoryArn,
    listPolicyAttachments_policyReference,

    -- * Destructuring the Response
    ListPolicyAttachmentsResponse (..),
    newListPolicyAttachmentsResponse,

    -- * Response Lenses
    listPolicyAttachmentsResponse_objectIdentifiers,
    listPolicyAttachmentsResponse_nextToken,
    listPolicyAttachmentsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
  { -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that identifies the policy object.
    policyReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'listPolicyAttachments_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'nextToken', 'listPolicyAttachments_nextToken' - The pagination token.
--
-- 'maxResults', 'listPolicyAttachments_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'directoryArn', 'listPolicyAttachments_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
--
-- 'policyReference', 'listPolicyAttachments_policyReference' - The reference that identifies the policy object.
newListPolicyAttachments ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'policyReference'
  ObjectReference ->
  ListPolicyAttachments
newListPolicyAttachments
  pDirectoryArn_
  pPolicyReference_ =
    ListPolicyAttachments'
      { consistencyLevel =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        policyReference = pPolicyReference_
      }

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listPolicyAttachments_consistencyLevel :: Lens.Lens' ListPolicyAttachments (Prelude.Maybe ConsistencyLevel)
listPolicyAttachments_consistencyLevel = Lens.lens (\ListPolicyAttachments' {consistencyLevel} -> consistencyLevel) (\s@ListPolicyAttachments' {} a -> s {consistencyLevel = a} :: ListPolicyAttachments)

-- | The pagination token.
listPolicyAttachments_nextToken :: Lens.Lens' ListPolicyAttachments (Prelude.Maybe Prelude.Text)
listPolicyAttachments_nextToken = Lens.lens (\ListPolicyAttachments' {nextToken} -> nextToken) (\s@ListPolicyAttachments' {} a -> s {nextToken = a} :: ListPolicyAttachments)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listPolicyAttachments_maxResults :: Lens.Lens' ListPolicyAttachments (Prelude.Maybe Prelude.Natural)
listPolicyAttachments_maxResults = Lens.lens (\ListPolicyAttachments' {maxResults} -> maxResults) (\s@ListPolicyAttachments' {} a -> s {maxResults = a} :: ListPolicyAttachments)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
listPolicyAttachments_directoryArn :: Lens.Lens' ListPolicyAttachments Prelude.Text
listPolicyAttachments_directoryArn = Lens.lens (\ListPolicyAttachments' {directoryArn} -> directoryArn) (\s@ListPolicyAttachments' {} a -> s {directoryArn = a} :: ListPolicyAttachments)

-- | The reference that identifies the policy object.
listPolicyAttachments_policyReference :: Lens.Lens' ListPolicyAttachments ObjectReference
listPolicyAttachments_policyReference = Lens.lens (\ListPolicyAttachments' {policyReference} -> policyReference) (\s@ListPolicyAttachments' {} a -> s {policyReference = a} :: ListPolicyAttachments)

instance Core.AWSPager ListPolicyAttachments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyAttachmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPolicyAttachmentsResponse_objectIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicyAttachments_nextToken
          Lens..~ rs
          Lens.^? listPolicyAttachmentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyAttachments where
  type
    AWSResponse ListPolicyAttachments =
      ListPolicyAttachmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyAttachmentsResponse'
            Prelude.<$> ( x Core..?> "ObjectIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicyAttachments

instance Prelude.NFData ListPolicyAttachments

instance Core.ToHeaders ListPolicyAttachments where
  toHeaders ListPolicyAttachments' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListPolicyAttachments where
  toJSON ListPolicyAttachments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("PolicyReference" Core..= policyReference)
          ]
      )

instance Core.ToPath ListPolicyAttachments where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/policy/attachment"

instance Core.ToQuery ListPolicyAttachments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { -- | A list of @ObjectIdentifiers@ to which the policy is attached.
    objectIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifiers', 'listPolicyAttachmentsResponse_objectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- 'nextToken', 'listPolicyAttachmentsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listPolicyAttachmentsResponse_httpStatus' - The response's http status code.
newListPolicyAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyAttachmentsResponse
newListPolicyAttachmentsResponse pHttpStatus_ =
  ListPolicyAttachmentsResponse'
    { objectIdentifiers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
listPolicyAttachmentsResponse_objectIdentifiers :: Lens.Lens' ListPolicyAttachmentsResponse (Prelude.Maybe [Prelude.Text])
listPolicyAttachmentsResponse_objectIdentifiers = Lens.lens (\ListPolicyAttachmentsResponse' {objectIdentifiers} -> objectIdentifiers) (\s@ListPolicyAttachmentsResponse' {} a -> s {objectIdentifiers = a} :: ListPolicyAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listPolicyAttachmentsResponse_nextToken :: Lens.Lens' ListPolicyAttachmentsResponse (Prelude.Maybe Prelude.Text)
listPolicyAttachmentsResponse_nextToken = Lens.lens (\ListPolicyAttachmentsResponse' {nextToken} -> nextToken) (\s@ListPolicyAttachmentsResponse' {} a -> s {nextToken = a} :: ListPolicyAttachmentsResponse)

-- | The response's http status code.
listPolicyAttachmentsResponse_httpStatus :: Lens.Lens' ListPolicyAttachmentsResponse Prelude.Int
listPolicyAttachmentsResponse_httpStatus = Lens.lens (\ListPolicyAttachmentsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyAttachmentsResponse' {} a -> s {httpStatus = a} :: ListPolicyAttachmentsResponse)

instance Prelude.NFData ListPolicyAttachmentsResponse
