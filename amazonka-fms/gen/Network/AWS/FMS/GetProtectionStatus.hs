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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProtectionStatus' smart constructor.
data GetProtectionStatus = GetProtectionStatus'
  { -- | If you specify a value for @MaxResults@ and you have more objects than
    -- the number that you specify for @MaxResults@, Firewall Manager returns a
    -- @NextToken@ value in the response, which you can use to retrieve another
    -- group of objects. For the second and subsequent @GetProtectionStatus@
    -- requests, specify the value of @NextToken@ from the previous response to
    -- get information about another batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of objects that you want Firewall Manager to return
    -- for this request. If you have more objects than the number that you
    -- specify for @MaxResults@, the response includes a @NextToken@ value that
    -- you can use to get another batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The start of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by Firewall Manager is Unix time in seconds.
    -- However, any valid @timestamp@ format is allowed.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The end of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by Firewall Manager is Unix time in seconds.
    -- However, any valid @timestamp@ format is allowed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account that is in scope of the policy that you
    -- want to get the details for.
    memberAccountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the policy for which you want to get the attack information.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProtectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getProtectionStatus_nextToken' - If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, Firewall Manager returns a
-- @NextToken@ value in the response, which you can use to retrieve another
-- group of objects. For the second and subsequent @GetProtectionStatus@
-- requests, specify the value of @NextToken@ from the previous response to
-- get information about another batch of objects.
--
-- 'maxResults', 'getProtectionStatus_maxResults' - Specifies the number of objects that you want Firewall Manager to return
-- for this request. If you have more objects than the number that you
-- specify for @MaxResults@, the response includes a @NextToken@ value that
-- you can use to get another batch of objects.
--
-- 'startTime', 'getProtectionStatus_startTime' - The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
--
-- 'endTime', 'getProtectionStatus_endTime' - The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
--
-- 'memberAccountId', 'getProtectionStatus_memberAccountId' - The Amazon Web Services account that is in scope of the policy that you
-- want to get the details for.
--
-- 'policyId', 'getProtectionStatus_policyId' - The ID of the policy for which you want to get the attack information.
newGetProtectionStatus ::
  -- | 'policyId'
  Prelude.Text ->
  GetProtectionStatus
newGetProtectionStatus pPolicyId_ =
  GetProtectionStatus'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      memberAccountId = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, Firewall Manager returns a
-- @NextToken@ value in the response, which you can use to retrieve another
-- group of objects. For the second and subsequent @GetProtectionStatus@
-- requests, specify the value of @NextToken@ from the previous response to
-- get information about another batch of objects.
getProtectionStatus_nextToken :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Text)
getProtectionStatus_nextToken = Lens.lens (\GetProtectionStatus' {nextToken} -> nextToken) (\s@GetProtectionStatus' {} a -> s {nextToken = a} :: GetProtectionStatus)

-- | Specifies the number of objects that you want Firewall Manager to return
-- for this request. If you have more objects than the number that you
-- specify for @MaxResults@, the response includes a @NextToken@ value that
-- you can use to get another batch of objects.
getProtectionStatus_maxResults :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Natural)
getProtectionStatus_maxResults = Lens.lens (\GetProtectionStatus' {maxResults} -> maxResults) (\s@GetProtectionStatus' {} a -> s {maxResults = a} :: GetProtectionStatus)

-- | The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
getProtectionStatus_startTime :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.UTCTime)
getProtectionStatus_startTime = Lens.lens (\GetProtectionStatus' {startTime} -> startTime) (\s@GetProtectionStatus' {} a -> s {startTime = a} :: GetProtectionStatus) Prelude.. Lens.mapping Core._Time

-- | The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
getProtectionStatus_endTime :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.UTCTime)
getProtectionStatus_endTime = Lens.lens (\GetProtectionStatus' {endTime} -> endTime) (\s@GetProtectionStatus' {} a -> s {endTime = a} :: GetProtectionStatus) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account that is in scope of the policy that you
-- want to get the details for.
getProtectionStatus_memberAccountId :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Text)
getProtectionStatus_memberAccountId = Lens.lens (\GetProtectionStatus' {memberAccountId} -> memberAccountId) (\s@GetProtectionStatus' {} a -> s {memberAccountId = a} :: GetProtectionStatus)

-- | The ID of the policy for which you want to get the attack information.
getProtectionStatus_policyId :: Lens.Lens' GetProtectionStatus Prelude.Text
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AdminAccountId")
            Prelude.<*> (x Core..?> "Data")
            Prelude.<*> (x Core..?> "ServiceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProtectionStatus

instance Prelude.NFData GetProtectionStatus

instance Core.ToHeaders GetProtectionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetProtectionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetProtectionStatus where
  toJSON GetProtectionStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("MemberAccountId" Core..=)
              Prelude.<$> memberAccountId,
            Prelude.Just ("PolicyId" Core..= policyId)
          ]
      )

instance Core.ToPath GetProtectionStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetProtectionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProtectionStatusResponse' smart constructor.
data GetProtectionStatusResponse = GetProtectionStatusResponse'
  { -- | If you have more objects than the number that you specified for
    -- @MaxResults@ in the request, the response includes a @NextToken@ value.
    -- To list more objects, submit another @GetProtectionStatus@ request, and
    -- specify the @NextToken@ value from the response in the @NextToken@ value
    -- in the next request.
    --
    -- Amazon Web Services SDKs provide auto-pagination that identify
    -- @NextToken@ in a response and make subsequent request calls
    -- automatically on your behalf. However, this feature is not supported by
    -- @GetProtectionStatus@. You must submit subsequent requests with
    -- @NextToken@ using your own processes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Firewall Manager administrator account for this policy.
    adminAccountId :: Prelude.Maybe Prelude.Text,
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
    data' :: Prelude.Maybe Prelude.Text,
    -- | The service type that is protected by the policy. Currently, this is
    -- always @SHIELD_ADVANCED@.
    serviceType :: Prelude.Maybe SecurityServiceType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Amazon Web Services SDKs provide auto-pagination that identify
-- @NextToken@ in a response and make subsequent request calls
-- automatically on your behalf. However, this feature is not supported by
-- @GetProtectionStatus@. You must submit subsequent requests with
-- @NextToken@ using your own processes.
--
-- 'adminAccountId', 'getProtectionStatusResponse_adminAccountId' - The ID of the Firewall Manager administrator account for this policy.
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
  Prelude.Int ->
  GetProtectionStatusResponse
newGetProtectionStatusResponse pHttpStatus_ =
  GetProtectionStatusResponse'
    { nextToken =
        Prelude.Nothing,
      adminAccountId = Prelude.Nothing,
      data' = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more objects than the number that you specified for
-- @MaxResults@ in the request, the response includes a @NextToken@ value.
-- To list more objects, submit another @GetProtectionStatus@ request, and
-- specify the @NextToken@ value from the response in the @NextToken@ value
-- in the next request.
--
-- Amazon Web Services SDKs provide auto-pagination that identify
-- @NextToken@ in a response and make subsequent request calls
-- automatically on your behalf. However, this feature is not supported by
-- @GetProtectionStatus@. You must submit subsequent requests with
-- @NextToken@ using your own processes.
getProtectionStatusResponse_nextToken :: Lens.Lens' GetProtectionStatusResponse (Prelude.Maybe Prelude.Text)
getProtectionStatusResponse_nextToken = Lens.lens (\GetProtectionStatusResponse' {nextToken} -> nextToken) (\s@GetProtectionStatusResponse' {} a -> s {nextToken = a} :: GetProtectionStatusResponse)

-- | The ID of the Firewall Manager administrator account for this policy.
getProtectionStatusResponse_adminAccountId :: Lens.Lens' GetProtectionStatusResponse (Prelude.Maybe Prelude.Text)
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
getProtectionStatusResponse_data :: Lens.Lens' GetProtectionStatusResponse (Prelude.Maybe Prelude.Text)
getProtectionStatusResponse_data = Lens.lens (\GetProtectionStatusResponse' {data'} -> data') (\s@GetProtectionStatusResponse' {} a -> s {data' = a} :: GetProtectionStatusResponse)

-- | The service type that is protected by the policy. Currently, this is
-- always @SHIELD_ADVANCED@.
getProtectionStatusResponse_serviceType :: Lens.Lens' GetProtectionStatusResponse (Prelude.Maybe SecurityServiceType)
getProtectionStatusResponse_serviceType = Lens.lens (\GetProtectionStatusResponse' {serviceType} -> serviceType) (\s@GetProtectionStatusResponse' {} a -> s {serviceType = a} :: GetProtectionStatusResponse)

-- | The response's http status code.
getProtectionStatusResponse_httpStatus :: Lens.Lens' GetProtectionStatusResponse Prelude.Int
getProtectionStatusResponse_httpStatus = Lens.lens (\GetProtectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetProtectionStatusResponse' {} a -> s {httpStatus = a} :: GetProtectionStatusResponse)

instance Prelude.NFData GetProtectionStatusResponse
