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
-- Module      : Network.AWS.FMS.ListPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicySummary@ objects.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListPolicies
  ( -- * Creating a Request
    ListPolicies (..),
    newListPolicies,

    -- * Request Lenses
    listPolicies_nextToken,
    listPolicies_maxResults,

    -- * Destructuring the Response
    ListPoliciesResponse (..),
    newListPoliciesResponse,

    -- * Response Lenses
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policyList,
    listPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | If you specify a value for @MaxResults@ and you have more
    -- @PolicySummary@ objects than the number that you specify for
    -- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
    -- response that allows you to list another group of @PolicySummary@
    -- objects. For the second and subsequent @ListPolicies@ requests, specify
    -- the value of @NextToken@ from the previous response to get information
    -- about another batch of @PolicySummary@ objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @PolicySummary@ objects that you want AWS
    -- Firewall Manager to return for this request. If you have more
    -- @PolicySummary@ objects than the number that you specify for
    -- @MaxResults@, the response includes a @NextToken@ value that you can use
    -- to get another batch of @PolicySummary@ objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicies_nextToken' - If you specify a value for @MaxResults@ and you have more
-- @PolicySummary@ objects than the number that you specify for
-- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
-- response that allows you to list another group of @PolicySummary@
-- objects. For the second and subsequent @ListPolicies@ requests, specify
-- the value of @NextToken@ from the previous response to get information
-- about another batch of @PolicySummary@ objects.
--
-- 'maxResults', 'listPolicies_maxResults' - Specifies the number of @PolicySummary@ objects that you want AWS
-- Firewall Manager to return for this request. If you have more
-- @PolicySummary@ objects than the number that you specify for
-- @MaxResults@, the response includes a @NextToken@ value that you can use
-- to get another batch of @PolicySummary@ objects.
newListPolicies ::
  ListPolicies
newListPolicies =
  ListPolicies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If you specify a value for @MaxResults@ and you have more
-- @PolicySummary@ objects than the number that you specify for
-- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
-- response that allows you to list another group of @PolicySummary@
-- objects. For the second and subsequent @ListPolicies@ requests, specify
-- the value of @NextToken@ from the previous response to get information
-- about another batch of @PolicySummary@ objects.
listPolicies_nextToken :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Text)
listPolicies_nextToken = Lens.lens (\ListPolicies' {nextToken} -> nextToken) (\s@ListPolicies' {} a -> s {nextToken = a} :: ListPolicies)

-- | Specifies the number of @PolicySummary@ objects that you want AWS
-- Firewall Manager to return for this request. If you have more
-- @PolicySummary@ objects than the number that you specify for
-- @MaxResults@, the response includes a @NextToken@ value that you can use
-- to get another batch of @PolicySummary@ objects.
listPolicies_maxResults :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Natural)
listPolicies_maxResults = Lens.lens (\ListPolicies' {maxResults} -> maxResults) (\s@ListPolicies' {} a -> s {maxResults = a} :: ListPolicies)

instance Core.AWSPager ListPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_policyList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicies_nextToken
          Lens..~ rs
          Lens.^? listPoliciesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPolicies where
  type AWSResponse ListPolicies = ListPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "PolicyList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicies

instance Prelude.NFData ListPolicies

instance Core.ToHeaders ListPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.ListPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPolicies where
  toJSON ListPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | If you have more @PolicySummary@ objects than the number that you
    -- specified for @MaxResults@ in the request, the response includes a
    -- @NextToken@ value. To list more @PolicySummary@ objects, submit another
    -- @ListPolicies@ request, and specify the @NextToken@ value from the
    -- response in the @NextToken@ value in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @PolicySummary@ objects.
    policyList :: Prelude.Maybe [PolicySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPoliciesResponse_nextToken' - If you have more @PolicySummary@ objects than the number that you
-- specified for @MaxResults@ in the request, the response includes a
-- @NextToken@ value. To list more @PolicySummary@ objects, submit another
-- @ListPolicies@ request, and specify the @NextToken@ value from the
-- response in the @NextToken@ value in the next request.
--
-- 'policyList', 'listPoliciesResponse_policyList' - An array of @PolicySummary@ objects.
--
-- 'httpStatus', 'listPoliciesResponse_httpStatus' - The response's http status code.
newListPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPoliciesResponse
newListPoliciesResponse pHttpStatus_ =
  ListPoliciesResponse'
    { nextToken = Prelude.Nothing,
      policyList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @PolicySummary@ objects than the number that you
-- specified for @MaxResults@ in the request, the response includes a
-- @NextToken@ value. To list more @PolicySummary@ objects, submit another
-- @ListPolicies@ request, and specify the @NextToken@ value from the
-- response in the @NextToken@ value in the next request.
listPoliciesResponse_nextToken :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe Prelude.Text)
listPoliciesResponse_nextToken = Lens.lens (\ListPoliciesResponse' {nextToken} -> nextToken) (\s@ListPoliciesResponse' {} a -> s {nextToken = a} :: ListPoliciesResponse)

-- | An array of @PolicySummary@ objects.
listPoliciesResponse_policyList :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe [PolicySummary])
listPoliciesResponse_policyList = Lens.lens (\ListPoliciesResponse' {policyList} -> policyList) (\s@ListPoliciesResponse' {} a -> s {policyList = a} :: ListPoliciesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Prelude.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Prelude.NFData ListPoliciesResponse
