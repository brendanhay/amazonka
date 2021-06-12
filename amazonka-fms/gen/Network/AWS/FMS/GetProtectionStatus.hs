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
-- Module      : Network.AWS.FMS.GetProtectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you created a Shield Advanced policy, returns policy-level attack
-- summary information in the event of a potential DDoS attack. Other
-- policy types are currently unsupported.
module Network.AWS.FMS.GetProtectionStatus
  ( -- * Creating a Request
    GetProtectionStatus (..),
    newGetProtectionStatus,

    -- * Request Lenses
    getProtectionStatus_nextToken,
    getProtectionStatus_maxResults,
    getProtectionStatus_startTime,
    getProtectionStatus_endTime,
    getProtectionStatus_memberAccountId,
    getProtectionStatus_policyId,

    -- * Destructuring the Response
    GetProtectionStatusResponse (..),
    newGetProtectionStatusResponse,

    -- * Response Lenses
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProtectionStatus' smart constructor.
data GetProtectionStatus = GetProtectionStatus'
  { -- | If you specify a value for @MaxResults@ and you have more objects than
    -- the number that you specify for @MaxResults@, AWS Firewall Manager
    -- returns a @NextToken@ value in the response, which you can use to
    -- retrieve another group of objects. For the second and subsequent
    -- @GetProtectionStatus@ requests, specify the value of @NextToken@ from
    -- the previous response to get information about another batch of objects.
    nextToken :: Core.Maybe Core.Text,
    -- | Specifies the number of objects that you want AWS Firewall Manager to
    -- return for this request. If you have more objects than the number that
    -- you specify for @MaxResults@, the response includes a @NextToken@ value
    -- that you can use to get another batch of objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | The start of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by AWS Firewall Manager is Unix time in
    -- seconds. However, any valid @timestamp@ format is allowed.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by AWS Firewall Manager is Unix time in
    -- seconds. However, any valid @timestamp@ format is allowed.
    endTime :: Core.Maybe Core.POSIX,
    -- | The AWS account that is in scope of the policy that you want to get the
    -- details for.
    memberAccountId :: Core.Maybe Core.Text,
    -- | The ID of the policy for which you want to get the attack information.
    policyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProtectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getProtectionStatus_nextToken' - If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, AWS Firewall Manager
-- returns a @NextToken@ value in the response, which you can use to
-- retrieve another group of objects. For the second and subsequent
-- @GetProtectionStatus@ requests, specify the value of @NextToken@ from
-- the previous response to get information about another batch of objects.
--
-- 'maxResults', 'getProtectionStatus_maxResults' - Specifies the number of objects that you want AWS Firewall Manager to
-- return for this request. If you have more objects than the number that
-- you specify for @MaxResults@, the response includes a @NextToken@ value
-- that you can use to get another batch of objects.
--
-- 'startTime', 'getProtectionStatus_startTime' - The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by AWS Firewall Manager is Unix time in
-- seconds. However, any valid @timestamp@ format is allowed.
--
-- 'endTime', 'getProtectionStatus_endTime' - The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by AWS Firewall Manager is Unix time in
-- seconds. However, any valid @timestamp@ format is allowed.
--
-- 'memberAccountId', 'getProtectionStatus_memberAccountId' - The AWS account that is in scope of the policy that you want to get the
-- details for.
--
-- 'policyId', 'getProtectionStatus_policyId' - The ID of the policy for which you want to get the attack information.
newGetProtectionStatus ::
  -- | 'policyId'
  Core.Text ->
  GetProtectionStatus
newGetProtectionStatus pPolicyId_ =
  GetProtectionStatus'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      memberAccountId = Core.Nothing,
      policyId = pPolicyId_
    }

-- | If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, AWS Firewall Manager
-- returns a @NextToken@ value in the response, which you can use to
-- retrieve another group of objects. For the second and subsequent
-- @GetProtectionStatus@ requests, specify the value of @NextToken@ from
-- the previous response to get information about another batch of objects.
getProtectionStatus_nextToken :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.Text)
getProtectionStatus_nextToken = Lens.lens (\GetProtectionStatus' {nextToken} -> nextToken) (\s@GetProtectionStatus' {} a -> s {nextToken = a} :: GetProtectionStatus)

-- | Specifies the number of objects that you want AWS Firewall Manager to
-- return for this request. If you have more objects than the number that
-- you specify for @MaxResults@, the response includes a @NextToken@ value
-- that you can use to get another batch of objects.
getProtectionStatus_maxResults :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.Natural)
getProtectionStatus_maxResults = Lens.lens (\GetProtectionStatus' {maxResults} -> maxResults) (\s@GetProtectionStatus' {} a -> s {maxResults = a} :: GetProtectionStatus)

-- | The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by AWS Firewall Manager is Unix time in
-- seconds. However, any valid @timestamp@ format is allowed.
getProtectionStatus_startTime :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.UTCTime)
getProtectionStatus_startTime = Lens.lens (\GetProtectionStatus' {startTime} -> startTime) (\s@GetProtectionStatus' {} a -> s {startTime = a} :: GetProtectionStatus) Core.. Lens.mapping Core._Time

-- | The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by AWS Firewall Manager is Unix time in
-- seconds. However, any valid @timestamp@ format is allowed.
getProtectionStatus_endTime :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.UTCTime)
getProtectionStatus_endTime = Lens.lens (\GetProtectionStatus' {endTime} -> endTime) (\s@GetProtectionStatus' {} a -> s {endTime = a} :: GetProtectionStatus) Core.. Lens.mapping Core._Time

-- | The AWS account that is in scope of the policy that you want to get the
-- details for.
getProtectionStatus_memberAccountId :: Lens.Lens' GetProtectionStatus (Core.Maybe Core.Text)
getProtectionStatus_memberAccountId = Lens.lens (\GetProtectionStatus' {memberAccountId} -> memberAccountId) (\s@GetProtectionStatus' {} a -> s {memberAccountId = a} :: GetProtectionStatus)

-- | The ID of the policy for which you want to get the attack information.
getProtectionStatus_policyId :: Lens.Lens' GetProtectionStatus Core.Text
getProtectionStatus_policyId = Lens.lens (\GetProtectionStatus' {policyId} -> policyId) (\s@GetProtectionStatus' {} a -> s {policyId = a} :: GetProtectionStatus)

instance Core.AWSRequest GetProtectionStatus where
  type
    AWSResponse GetProtectionStatus =
      GetProtectionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProtectionStatusResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AdminAccountId")
            Core.<*> (x Core..?> "Data")
            Core.<*> (x Core..?> "ServiceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetProtectionStatus

instance Core.NFData GetProtectionStatus

instance Core.ToHeaders GetProtectionStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetProtectionStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetProtectionStatus where
  toJSON GetProtectionStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("MemberAccountId" Core..=) Core.<$> memberAccountId,
            Core.Just ("PolicyId" Core..= policyId)
          ]
      )

instance Core.ToPath GetProtectionStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetProtectionStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetProtectionStatusResponse' smart constructor.
data GetProtectionStatusResponse = GetProtectionStatusResponse'
  { -- | If you have more objects than the number that you specified for
    -- @MaxResults@ in the request, the response includes a @NextToken@ value.
    -- To list more objects, submit another @GetProtectionStatus@ request, and
    -- specify the @NextToken@ value from the response in the @NextToken@ value
    -- in the next request.
    --
    -- AWS SDKs provide auto-pagination that identify @NextToken@ in a response
    -- and make subsequent request calls automatically on your behalf. However,
    -- this feature is not supported by @GetProtectionStatus@. You must submit
    -- subsequent requests with @NextToken@ using your own processes.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the AWS Firewall administrator account for this policy.
    adminAccountId :: Core.Maybe Core.Text,
    -- | Details about the attack, including the following:
    --
    -- -   Attack type
    --
    -- -   Account ID
    --
    -- -   ARN of the resource attacked
    --
    -- -   Start time of the attack
    --
    -- -   End time of the attack (ongoing attacks will not have an end time)
    --
    -- The details are in JSON format.
    data' :: Core.Maybe Core.Text,
    -- | The service type that is protected by the policy. Currently, this is
    -- always @SHIELD_ADVANCED@.
    serviceType :: Core.Maybe SecurityServiceType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProtectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getProtectionStatusResponse_nextToken' - If you have more objects than the number that you specified for
-- @MaxResults@ in the request, the response includes a @NextToken@ value.
-- To list more objects, submit another @GetProtectionStatus@ request, and
-- specify the @NextToken@ value from the response in the @NextToken@ value
-- in the next request.
--
-- AWS SDKs provide auto-pagination that identify @NextToken@ in a response
-- and make subsequent request calls automatically on your behalf. However,
-- this feature is not supported by @GetProtectionStatus@. You must submit
-- subsequent requests with @NextToken@ using your own processes.
--
-- 'adminAccountId', 'getProtectionStatusResponse_adminAccountId' - The ID of the AWS Firewall administrator account for this policy.
--
-- 'data'', 'getProtectionStatusResponse_data' - Details about the attack, including the following:
--
-- -   Attack type
--
-- -   Account ID
--
-- -   ARN of the resource attacked
--
-- -   Start time of the attack
--
-- -   End time of the attack (ongoing attacks will not have an end time)
--
-- The details are in JSON format.
--
-- 'serviceType', 'getProtectionStatusResponse_serviceType' - The service type that is protected by the policy. Currently, this is
-- always @SHIELD_ADVANCED@.
--
-- 'httpStatus', 'getProtectionStatusResponse_httpStatus' - The response's http status code.
newGetProtectionStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetProtectionStatusResponse
newGetProtectionStatusResponse pHttpStatus_ =
  GetProtectionStatusResponse'
    { nextToken =
        Core.Nothing,
      adminAccountId = Core.Nothing,
      data' = Core.Nothing,
      serviceType = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more objects than the number that you specified for
-- @MaxResults@ in the request, the response includes a @NextToken@ value.
-- To list more objects, submit another @GetProtectionStatus@ request, and
-- specify the @NextToken@ value from the response in the @NextToken@ value
-- in the next request.
--
-- AWS SDKs provide auto-pagination that identify @NextToken@ in a response
-- and make subsequent request calls automatically on your behalf. However,
-- this feature is not supported by @GetProtectionStatus@. You must submit
-- subsequent requests with @NextToken@ using your own processes.
getProtectionStatusResponse_nextToken :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Core.Text)
getProtectionStatusResponse_nextToken = Lens.lens (\GetProtectionStatusResponse' {nextToken} -> nextToken) (\s@GetProtectionStatusResponse' {} a -> s {nextToken = a} :: GetProtectionStatusResponse)

-- | The ID of the AWS Firewall administrator account for this policy.
getProtectionStatusResponse_adminAccountId :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Core.Text)
getProtectionStatusResponse_adminAccountId = Lens.lens (\GetProtectionStatusResponse' {adminAccountId} -> adminAccountId) (\s@GetProtectionStatusResponse' {} a -> s {adminAccountId = a} :: GetProtectionStatusResponse)

-- | Details about the attack, including the following:
--
-- -   Attack type
--
-- -   Account ID
--
-- -   ARN of the resource attacked
--
-- -   Start time of the attack
--
-- -   End time of the attack (ongoing attacks will not have an end time)
--
-- The details are in JSON format.
getProtectionStatusResponse_data :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe Core.Text)
getProtectionStatusResponse_data = Lens.lens (\GetProtectionStatusResponse' {data'} -> data') (\s@GetProtectionStatusResponse' {} a -> s {data' = a} :: GetProtectionStatusResponse)

-- | The service type that is protected by the policy. Currently, this is
-- always @SHIELD_ADVANCED@.
getProtectionStatusResponse_serviceType :: Lens.Lens' GetProtectionStatusResponse (Core.Maybe SecurityServiceType)
getProtectionStatusResponse_serviceType = Lens.lens (\GetProtectionStatusResponse' {serviceType} -> serviceType) (\s@GetProtectionStatusResponse' {} a -> s {serviceType = a} :: GetProtectionStatusResponse)

-- | The response's http status code.
getProtectionStatusResponse_httpStatus :: Lens.Lens' GetProtectionStatusResponse Core.Int
getProtectionStatusResponse_httpStatus = Lens.lens (\GetProtectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetProtectionStatusResponse' {} a -> s {httpStatus = a} :: GetProtectionStatusResponse)

instance Core.NFData GetProtectionStatusResponse
