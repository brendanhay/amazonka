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
-- Module      : Amazonka.CloudDirectory.LookupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudDirectory.LookupPolicy
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newLookupPolicy' smart constructor.
data LookupPolicy = LookupPolicy'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory.
    -- For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  LookupPolicy
newLookupPolicy pDirectoryArn_ pObjectReference_ =
  LookupPolicy'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      objectReference = pObjectReference_
    }

-- | The token to request the next page of results.
lookupPolicy_nextToken :: Lens.Lens' LookupPolicy (Prelude.Maybe Prelude.Text)
lookupPolicy_nextToken = Lens.lens (\LookupPolicy' {nextToken} -> nextToken) (\s@LookupPolicy' {} a -> s {nextToken = a} :: LookupPolicy)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
lookupPolicy_maxResults :: Lens.Lens' LookupPolicy (Prelude.Maybe Prelude.Natural)
lookupPolicy_maxResults = Lens.lens (\LookupPolicy' {maxResults} -> maxResults) (\s@LookupPolicy' {} a -> s {maxResults = a} :: LookupPolicy)

-- | The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
lookupPolicy_directoryArn :: Lens.Lens' LookupPolicy Prelude.Text
lookupPolicy_directoryArn = Lens.lens (\LookupPolicy' {directoryArn} -> directoryArn) (\s@LookupPolicy' {} a -> s {directoryArn = a} :: LookupPolicy)

-- | Reference that identifies the object whose policies will be looked up.
lookupPolicy_objectReference :: Lens.Lens' LookupPolicy ObjectReference
lookupPolicy_objectReference = Lens.lens (\LookupPolicy' {objectReference} -> objectReference) (\s@LookupPolicy' {} a -> s {objectReference = a} :: LookupPolicy)

instance Core.AWSPager LookupPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? lookupPolicyResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? lookupPolicyResponse_policyToPathList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& lookupPolicy_nextToken
          Lens..~ rs
          Lens.^? lookupPolicyResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest LookupPolicy where
  type AWSResponse LookupPolicy = LookupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupPolicyResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PolicyToPathList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LookupPolicy where
  hashWithSalt _salt LookupPolicy' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData LookupPolicy where
  rnf LookupPolicy' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Core.ToHeaders LookupPolicy where
  toHeaders LookupPolicy' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON LookupPolicy where
  toJSON LookupPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath LookupPolicy where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/policy/lookup"

instance Core.ToQuery LookupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLookupPolicyResponse' smart constructor.
data LookupPolicyResponse = LookupPolicyResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides list of path to policies. Policies contain @PolicyId@,
    -- @ObjectIdentifier@, and @PolicyType@. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    policyToPathList :: Prelude.Maybe [PolicyToPath],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  LookupPolicyResponse
newLookupPolicyResponse pHttpStatus_ =
  LookupPolicyResponse'
    { nextToken = Prelude.Nothing,
      policyToPathList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
lookupPolicyResponse_nextToken :: Lens.Lens' LookupPolicyResponse (Prelude.Maybe Prelude.Text)
lookupPolicyResponse_nextToken = Lens.lens (\LookupPolicyResponse' {nextToken} -> nextToken) (\s@LookupPolicyResponse' {} a -> s {nextToken = a} :: LookupPolicyResponse)

-- | Provides list of path to policies. Policies contain @PolicyId@,
-- @ObjectIdentifier@, and @PolicyType@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
lookupPolicyResponse_policyToPathList :: Lens.Lens' LookupPolicyResponse (Prelude.Maybe [PolicyToPath])
lookupPolicyResponse_policyToPathList = Lens.lens (\LookupPolicyResponse' {policyToPathList} -> policyToPathList) (\s@LookupPolicyResponse' {} a -> s {policyToPathList = a} :: LookupPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
lookupPolicyResponse_httpStatus :: Lens.Lens' LookupPolicyResponse Prelude.Int
lookupPolicyResponse_httpStatus = Lens.lens (\LookupPolicyResponse' {httpStatus} -> httpStatus) (\s@LookupPolicyResponse' {} a -> s {httpStatus = a} :: LookupPolicyResponse)

instance Prelude.NFData LookupPolicyResponse where
  rnf LookupPolicyResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyToPathList
      `Prelude.seq` Prelude.rnf httpStatus
