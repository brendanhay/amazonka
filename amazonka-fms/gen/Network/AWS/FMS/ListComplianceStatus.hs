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
-- Module      : Network.AWS.FMS.ListComplianceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicyComplianceStatus@ objects. Use
-- @PolicyComplianceStatus@ to get a summary of which member accounts are
-- protected by the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListComplianceStatus
  ( -- * Creating a Request
    ListComplianceStatus (..),
    newListComplianceStatus,

    -- * Request Lenses
    listComplianceStatus_nextToken,
    listComplianceStatus_maxResults,
    listComplianceStatus_policyId,

    -- * Destructuring the Response
    ListComplianceStatusResponse (..),
    newListComplianceStatusResponse,

    -- * Response Lenses
    listComplianceStatusResponse_nextToken,
    listComplianceStatusResponse_policyComplianceStatusList,
    listComplianceStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListComplianceStatus' smart constructor.
data ListComplianceStatus = ListComplianceStatus'
  { -- | If you specify a value for @MaxResults@ and you have more
    -- @PolicyComplianceStatus@ objects than the number that you specify for
    -- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
    -- response that allows you to list another group of
    -- @PolicyComplianceStatus@ objects. For the second and subsequent
    -- @ListComplianceStatus@ requests, specify the value of @NextToken@ from
    -- the previous response to get information about another batch of
    -- @PolicyComplianceStatus@ objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @PolicyComplianceStatus@ objects that you want
    -- AWS Firewall Manager to return for this request. If you have more
    -- @PolicyComplianceStatus@ objects than the number that you specify for
    -- @MaxResults@, the response includes a @NextToken@ value that you can use
    -- to get another batch of @PolicyComplianceStatus@ objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the AWS Firewall Manager policy that you want the details for.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceStatus_nextToken' - If you specify a value for @MaxResults@ and you have more
-- @PolicyComplianceStatus@ objects than the number that you specify for
-- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
-- response that allows you to list another group of
-- @PolicyComplianceStatus@ objects. For the second and subsequent
-- @ListComplianceStatus@ requests, specify the value of @NextToken@ from
-- the previous response to get information about another batch of
-- @PolicyComplianceStatus@ objects.
--
-- 'maxResults', 'listComplianceStatus_maxResults' - Specifies the number of @PolicyComplianceStatus@ objects that you want
-- AWS Firewall Manager to return for this request. If you have more
-- @PolicyComplianceStatus@ objects than the number that you specify for
-- @MaxResults@, the response includes a @NextToken@ value that you can use
-- to get another batch of @PolicyComplianceStatus@ objects.
--
-- 'policyId', 'listComplianceStatus_policyId' - The ID of the AWS Firewall Manager policy that you want the details for.
newListComplianceStatus ::
  -- | 'policyId'
  Prelude.Text ->
  ListComplianceStatus
newListComplianceStatus pPolicyId_ =
  ListComplianceStatus'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | If you specify a value for @MaxResults@ and you have more
-- @PolicyComplianceStatus@ objects than the number that you specify for
-- @MaxResults@, AWS Firewall Manager returns a @NextToken@ value in the
-- response that allows you to list another group of
-- @PolicyComplianceStatus@ objects. For the second and subsequent
-- @ListComplianceStatus@ requests, specify the value of @NextToken@ from
-- the previous response to get information about another batch of
-- @PolicyComplianceStatus@ objects.
listComplianceStatus_nextToken :: Lens.Lens' ListComplianceStatus (Prelude.Maybe Prelude.Text)
listComplianceStatus_nextToken = Lens.lens (\ListComplianceStatus' {nextToken} -> nextToken) (\s@ListComplianceStatus' {} a -> s {nextToken = a} :: ListComplianceStatus)

-- | Specifies the number of @PolicyComplianceStatus@ objects that you want
-- AWS Firewall Manager to return for this request. If you have more
-- @PolicyComplianceStatus@ objects than the number that you specify for
-- @MaxResults@, the response includes a @NextToken@ value that you can use
-- to get another batch of @PolicyComplianceStatus@ objects.
listComplianceStatus_maxResults :: Lens.Lens' ListComplianceStatus (Prelude.Maybe Prelude.Natural)
listComplianceStatus_maxResults = Lens.lens (\ListComplianceStatus' {maxResults} -> maxResults) (\s@ListComplianceStatus' {} a -> s {maxResults = a} :: ListComplianceStatus)

-- | The ID of the AWS Firewall Manager policy that you want the details for.
listComplianceStatus_policyId :: Lens.Lens' ListComplianceStatus Prelude.Text
listComplianceStatus_policyId = Lens.lens (\ListComplianceStatus' {policyId} -> policyId) (\s@ListComplianceStatus' {} a -> s {policyId = a} :: ListComplianceStatus)

instance Core.AWSPager ListComplianceStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComplianceStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listComplianceStatusResponse_policyComplianceStatusList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listComplianceStatus_nextToken
          Lens..~ rs
          Lens.^? listComplianceStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListComplianceStatus where
  type
    AWSResponse ListComplianceStatus =
      ListComplianceStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComplianceStatusResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PolicyComplianceStatusList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComplianceStatus

instance Prelude.NFData ListComplianceStatus

instance Core.ToHeaders ListComplianceStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.ListComplianceStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListComplianceStatus where
  toJSON ListComplianceStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("PolicyId" Core..= policyId)
          ]
      )

instance Core.ToPath ListComplianceStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery ListComplianceStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComplianceStatusResponse' smart constructor.
data ListComplianceStatusResponse = ListComplianceStatusResponse'
  { -- | If you have more @PolicyComplianceStatus@ objects than the number that
    -- you specified for @MaxResults@ in the request, the response includes a
    -- @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit
    -- another @ListComplianceStatus@ request, and specify the @NextToken@
    -- value from the response in the @NextToken@ value in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @PolicyComplianceStatus@ objects.
    policyComplianceStatusList :: Prelude.Maybe [PolicyComplianceStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceStatusResponse_nextToken' - If you have more @PolicyComplianceStatus@ objects than the number that
-- you specified for @MaxResults@ in the request, the response includes a
-- @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit
-- another @ListComplianceStatus@ request, and specify the @NextToken@
-- value from the response in the @NextToken@ value in the next request.
--
-- 'policyComplianceStatusList', 'listComplianceStatusResponse_policyComplianceStatusList' - An array of @PolicyComplianceStatus@ objects.
--
-- 'httpStatus', 'listComplianceStatusResponse_httpStatus' - The response's http status code.
newListComplianceStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComplianceStatusResponse
newListComplianceStatusResponse pHttpStatus_ =
  ListComplianceStatusResponse'
    { nextToken =
        Prelude.Nothing,
      policyComplianceStatusList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @PolicyComplianceStatus@ objects than the number that
-- you specified for @MaxResults@ in the request, the response includes a
-- @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit
-- another @ListComplianceStatus@ request, and specify the @NextToken@
-- value from the response in the @NextToken@ value in the next request.
listComplianceStatusResponse_nextToken :: Lens.Lens' ListComplianceStatusResponse (Prelude.Maybe Prelude.Text)
listComplianceStatusResponse_nextToken = Lens.lens (\ListComplianceStatusResponse' {nextToken} -> nextToken) (\s@ListComplianceStatusResponse' {} a -> s {nextToken = a} :: ListComplianceStatusResponse)

-- | An array of @PolicyComplianceStatus@ objects.
listComplianceStatusResponse_policyComplianceStatusList :: Lens.Lens' ListComplianceStatusResponse (Prelude.Maybe [PolicyComplianceStatus])
listComplianceStatusResponse_policyComplianceStatusList = Lens.lens (\ListComplianceStatusResponse' {policyComplianceStatusList} -> policyComplianceStatusList) (\s@ListComplianceStatusResponse' {} a -> s {policyComplianceStatusList = a} :: ListComplianceStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listComplianceStatusResponse_httpStatus :: Lens.Lens' ListComplianceStatusResponse Prelude.Int
listComplianceStatusResponse_httpStatus = Lens.lens (\ListComplianceStatusResponse' {httpStatus} -> httpStatus) (\s@ListComplianceStatusResponse' {} a -> s {httpStatus = a} :: ListComplianceStatusResponse)

instance Prelude.NFData ListComplianceStatusResponse
