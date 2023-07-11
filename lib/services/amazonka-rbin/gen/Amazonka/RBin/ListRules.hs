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
-- Module      : Amazonka.RBin.ListRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Recycle Bin retention rules in the Region.
--
-- This operation returns paginated results.
module Amazonka.RBin.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_lockState,
    listRules_maxResults,
    listRules_nextToken,
    listRules_resourceTags,
    listRules_resourceType,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The lock state of the retention rules to list. Only retention rules with
    -- the specified lock state are returned.
    lockState :: Prelude.Maybe LockState,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource tags used to identify resources that are
    -- retained by the retention rule.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The resource type retained by the retention rule. Only retention rules
    -- that retain the specified resource type are listed. Currently, only
    -- Amazon EBS snapshots and EBS-backed AMIs are supported. To list
    -- retention rules that retain snapshots, specify @EBS_SNAPSHOT@. To list
    -- retention rules that retain EBS-backed AMIs, specify @EC2_IMAGE@.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockState', 'listRules_lockState' - The lock state of the retention rules to list. Only retention rules with
-- the specified lock state are returned.
--
-- 'maxResults', 'listRules_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'listRules_nextToken' - The token for the next page of results.
--
-- 'resourceTags', 'listRules_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'resourceType', 'listRules_resourceType' - The resource type retained by the retention rule. Only retention rules
-- that retain the specified resource type are listed. Currently, only
-- Amazon EBS snapshots and EBS-backed AMIs are supported. To list
-- retention rules that retain snapshots, specify @EBS_SNAPSHOT@. To list
-- retention rules that retain EBS-backed AMIs, specify @EC2_IMAGE@.
newListRules ::
  -- | 'resourceType'
  ResourceType ->
  ListRules
newListRules pResourceType_ =
  ListRules'
    { lockState = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | The lock state of the retention rules to list. Only retention rules with
-- the specified lock state are returned.
listRules_lockState :: Lens.Lens' ListRules (Prelude.Maybe LockState)
listRules_lockState = Lens.lens (\ListRules' {lockState} -> lockState) (\s@ListRules' {} a -> s {lockState = a} :: ListRules)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listRules_maxResults :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_maxResults = Lens.lens (\ListRules' {maxResults} -> maxResults) (\s@ListRules' {} a -> s {maxResults = a} :: ListRules)

-- | The token for the next page of results.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
listRules_resourceTags :: Lens.Lens' ListRules (Prelude.Maybe [ResourceTag])
listRules_resourceTags = Lens.lens (\ListRules' {resourceTags} -> resourceTags) (\s@ListRules' {} a -> s {resourceTags = a} :: ListRules) Prelude.. Lens.mapping Lens.coerced

-- | The resource type retained by the retention rule. Only retention rules
-- that retain the specified resource type are listed. Currently, only
-- Amazon EBS snapshots and EBS-backed AMIs are supported. To list
-- retention rules that retain snapshots, specify @EBS_SNAPSHOT@. To list
-- retention rules that retain EBS-backed AMIs, specify @EC2_IMAGE@.
listRules_resourceType :: Lens.Lens' ListRules ResourceType
listRules_resourceType = Lens.lens (\ListRules' {resourceType} -> resourceType) (\s@ListRules' {} a -> s {resourceType = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRules_nextToken
          Lens..~ rs
          Lens.^? listRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt
      `Prelude.hashWithSalt` lockState
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf lockState
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRules where
  toJSON ListRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LockState" Data..=) Prelude.<$> lockState,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceTags" Data..=) Prelude.<$> resourceTags,
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath ListRules where
  toPath = Prelude.const "/list-rules"

instance Data.ToQuery ListRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the retention rules.
    rules :: Prelude.Maybe [RuleSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'rules', 'listRulesResponse_rules' - Information about the retention rules.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
newListRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextToken = Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | Information about the retention rules.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Prelude.Maybe [RuleSummary])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
