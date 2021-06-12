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
-- Module      : Network.AWS.CloudDirectory.LookupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all policies from the root of the Directory to the object
-- specified. If there are no policies present, an empty list is returned.
-- If policies are present, and if some objects don\'t have the policies
-- attached, it returns the @ObjectIdentifier@ for such objects. If
-- policies are present, it returns @ObjectIdentifier@, @policyId@, and
-- @policyType@. Paths that don\'t lead to the root from the target object
-- are ignored. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.LookupPolicy
  ( -- * Creating a Request
    LookupPolicy (..),
    newLookupPolicy,

    -- * Request Lenses
    lookupPolicy_nextToken,
    lookupPolicy_maxResults,
    lookupPolicy_directoryArn,
    lookupPolicy_objectReference,

    -- * Destructuring the Response
    LookupPolicyResponse (..),
    newLookupPolicyResponse,

    -- * Response Lenses
    lookupPolicyResponse_nextToken,
    lookupPolicyResponse_policyToPathList,
    lookupPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newLookupPolicy' smart constructor.
data LookupPolicy = LookupPolicy'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory.
    -- For more information, see arns.
    directoryArn :: Core.Text,
    -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LookupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupPolicy_nextToken' - The token to request the next page of results.
--
-- 'maxResults', 'lookupPolicy_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'directoryArn', 'lookupPolicy_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
--
-- 'objectReference', 'lookupPolicy_objectReference' - Reference that identifies the object whose policies will be looked up.
newLookupPolicy ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  LookupPolicy
newLookupPolicy pDirectoryArn_ pObjectReference_ =
  LookupPolicy'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      directoryArn = pDirectoryArn_,
      objectReference = pObjectReference_
    }

-- | The token to request the next page of results.
lookupPolicy_nextToken :: Lens.Lens' LookupPolicy (Core.Maybe Core.Text)
lookupPolicy_nextToken = Lens.lens (\LookupPolicy' {nextToken} -> nextToken) (\s@LookupPolicy' {} a -> s {nextToken = a} :: LookupPolicy)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
lookupPolicy_maxResults :: Lens.Lens' LookupPolicy (Core.Maybe Core.Natural)
lookupPolicy_maxResults = Lens.lens (\LookupPolicy' {maxResults} -> maxResults) (\s@LookupPolicy' {} a -> s {maxResults = a} :: LookupPolicy)

-- | The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
lookupPolicy_directoryArn :: Lens.Lens' LookupPolicy Core.Text
lookupPolicy_directoryArn = Lens.lens (\LookupPolicy' {directoryArn} -> directoryArn) (\s@LookupPolicy' {} a -> s {directoryArn = a} :: LookupPolicy)

-- | Reference that identifies the object whose policies will be looked up.
lookupPolicy_objectReference :: Lens.Lens' LookupPolicy ObjectReference
lookupPolicy_objectReference = Lens.lens (\LookupPolicy' {objectReference} -> objectReference) (\s@LookupPolicy' {} a -> s {objectReference = a} :: LookupPolicy)

instance Core.AWSPager LookupPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? lookupPolicyResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? lookupPolicyResponse_policyToPathList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& lookupPolicy_nextToken
          Lens..~ rs
          Lens.^? lookupPolicyResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest LookupPolicy where
  type AWSResponse LookupPolicy = LookupPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupPolicyResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "PolicyToPathList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable LookupPolicy

instance Core.NFData LookupPolicy

instance Core.ToHeaders LookupPolicy where
  toHeaders LookupPolicy' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON LookupPolicy where
  toJSON LookupPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath LookupPolicy where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/policy/lookup"

instance Core.ToQuery LookupPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newLookupPolicyResponse' smart constructor.
data LookupPolicyResponse = LookupPolicyResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Provides list of path to policies. Policies contain @PolicyId@,
    -- @ObjectIdentifier@, and @PolicyType@. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    policyToPathList :: Core.Maybe [PolicyToPath],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LookupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupPolicyResponse_nextToken' - The pagination token.
--
-- 'policyToPathList', 'lookupPolicyResponse_policyToPathList' - Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- 'httpStatus', 'lookupPolicyResponse_httpStatus' - The response's http status code.
newLookupPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  LookupPolicyResponse
newLookupPolicyResponse pHttpStatus_ =
  LookupPolicyResponse'
    { nextToken = Core.Nothing,
      policyToPathList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
lookupPolicyResponse_nextToken :: Lens.Lens' LookupPolicyResponse (Core.Maybe Core.Text)
lookupPolicyResponse_nextToken = Lens.lens (\LookupPolicyResponse' {nextToken} -> nextToken) (\s@LookupPolicyResponse' {} a -> s {nextToken = a} :: LookupPolicyResponse)

-- | Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
lookupPolicyResponse_policyToPathList :: Lens.Lens' LookupPolicyResponse (Core.Maybe [PolicyToPath])
lookupPolicyResponse_policyToPathList = Lens.lens (\LookupPolicyResponse' {policyToPathList} -> policyToPathList) (\s@LookupPolicyResponse' {} a -> s {policyToPathList = a} :: LookupPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
lookupPolicyResponse_httpStatus :: Lens.Lens' LookupPolicyResponse Core.Int
lookupPolicyResponse_httpStatus = Lens.lens (\LookupPolicyResponse' {httpStatus} -> httpStatus) (\s@LookupPolicyResponse' {} a -> s {httpStatus = a} :: LookupPolicyResponse)

instance Core.NFData LookupPolicyResponse
