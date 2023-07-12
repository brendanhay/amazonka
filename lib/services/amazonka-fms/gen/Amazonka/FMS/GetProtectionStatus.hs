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
-- Module      : Amazonka.FMS.GetProtectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you created a Shield Advanced policy, returns policy-level attack
-- summary information in the event of a potential DDoS attack. Other
-- policy types are currently unsupported.
module Amazonka.FMS.GetProtectionStatus
  ( -- * Creating a Request
    GetProtectionStatus (..),
    newGetProtectionStatus,

    -- * Request Lenses
    getProtectionStatus_endTime,
    getProtectionStatus_maxResults,
    getProtectionStatus_memberAccountId,
    getProtectionStatus_nextToken,
    getProtectionStatus_startTime,
    getProtectionStatus_policyId,

    -- * Destructuring the Response
    GetProtectionStatusResponse (..),
    newGetProtectionStatusResponse,

    -- * Response Lenses
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProtectionStatus' smart constructor.
data GetProtectionStatus = GetProtectionStatus'
  { -- | The end of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by Firewall Manager is Unix time in seconds.
    -- However, any valid @timestamp@ format is allowed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the number of objects that you want Firewall Manager to return
    -- for this request. If you have more objects than the number that you
    -- specify for @MaxResults@, the response includes a @NextToken@ value that
    -- you can use to get another batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services account that is in scope of the policy that you
    -- want to get the details for.
    memberAccountId :: Prelude.Maybe Prelude.Text,
    -- | If you specify a value for @MaxResults@ and you have more objects than
    -- the number that you specify for @MaxResults@, Firewall Manager returns a
    -- @NextToken@ value in the response, which you can use to retrieve another
    -- group of objects. For the second and subsequent @GetProtectionStatus@
    -- requests, specify the value of @NextToken@ from the previous response to
    -- get information about another batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time period to query for the attacks. This is a
    -- @timestamp@ type. The request syntax listing indicates a @number@ type
    -- because the default used by Firewall Manager is Unix time in seconds.
    -- However, any valid @timestamp@ format is allowed.
    startTime :: Prelude.Maybe Data.POSIX,
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
-- 'endTime', 'getProtectionStatus_endTime' - The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
--
-- 'maxResults', 'getProtectionStatus_maxResults' - Specifies the number of objects that you want Firewall Manager to return
-- for this request. If you have more objects than the number that you
-- specify for @MaxResults@, the response includes a @NextToken@ value that
-- you can use to get another batch of objects.
--
-- 'memberAccountId', 'getProtectionStatus_memberAccountId' - The Amazon Web Services account that is in scope of the policy that you
-- want to get the details for.
--
-- 'nextToken', 'getProtectionStatus_nextToken' - If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, Firewall Manager returns a
-- @NextToken@ value in the response, which you can use to retrieve another
-- group of objects. For the second and subsequent @GetProtectionStatus@
-- requests, specify the value of @NextToken@ from the previous response to
-- get information about another batch of objects.
--
-- 'startTime', 'getProtectionStatus_startTime' - The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
--
-- 'policyId', 'getProtectionStatus_policyId' - The ID of the policy for which you want to get the attack information.
newGetProtectionStatus ::
  -- | 'policyId'
  Prelude.Text ->
  GetProtectionStatus
newGetProtectionStatus pPolicyId_ =
  GetProtectionStatus'
    { endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      memberAccountId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | The end of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
getProtectionStatus_endTime :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.UTCTime)
getProtectionStatus_endTime = Lens.lens (\GetProtectionStatus' {endTime} -> endTime) (\s@GetProtectionStatus' {} a -> s {endTime = a} :: GetProtectionStatus) Prelude.. Lens.mapping Data._Time

-- | Specifies the number of objects that you want Firewall Manager to return
-- for this request. If you have more objects than the number that you
-- specify for @MaxResults@, the response includes a @NextToken@ value that
-- you can use to get another batch of objects.
getProtectionStatus_maxResults :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Natural)
getProtectionStatus_maxResults = Lens.lens (\GetProtectionStatus' {maxResults} -> maxResults) (\s@GetProtectionStatus' {} a -> s {maxResults = a} :: GetProtectionStatus)

-- | The Amazon Web Services account that is in scope of the policy that you
-- want to get the details for.
getProtectionStatus_memberAccountId :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Text)
getProtectionStatus_memberAccountId = Lens.lens (\GetProtectionStatus' {memberAccountId} -> memberAccountId) (\s@GetProtectionStatus' {} a -> s {memberAccountId = a} :: GetProtectionStatus)

-- | If you specify a value for @MaxResults@ and you have more objects than
-- the number that you specify for @MaxResults@, Firewall Manager returns a
-- @NextToken@ value in the response, which you can use to retrieve another
-- group of objects. For the second and subsequent @GetProtectionStatus@
-- requests, specify the value of @NextToken@ from the previous response to
-- get information about another batch of objects.
getProtectionStatus_nextToken :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.Text)
getProtectionStatus_nextToken = Lens.lens (\GetProtectionStatus' {nextToken} -> nextToken) (\s@GetProtectionStatus' {} a -> s {nextToken = a} :: GetProtectionStatus)

-- | The start of the time period to query for the attacks. This is a
-- @timestamp@ type. The request syntax listing indicates a @number@ type
-- because the default used by Firewall Manager is Unix time in seconds.
-- However, any valid @timestamp@ format is allowed.
getProtectionStatus_startTime :: Lens.Lens' GetProtectionStatus (Prelude.Maybe Prelude.UTCTime)
getProtectionStatus_startTime = Lens.lens (\GetProtectionStatus' {startTime} -> startTime) (\s@GetProtectionStatus' {} a -> s {startTime = a} :: GetProtectionStatus) Prelude.. Lens.mapping Data._Time

-- | The ID of the policy for which you want to get the attack information.
getProtectionStatus_policyId :: Lens.Lens' GetProtectionStatus Prelude.Text
getProtectionStatus_policyId = Lens.lens (\GetProtectionStatus' {policyId} -> policyId) (\s@GetProtectionStatus' {} a -> s {policyId = a} :: GetProtectionStatus)

instance Core.AWSRequest GetProtectionStatus where
  type
    AWSResponse GetProtectionStatus =
      GetProtectionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProtectionStatusResponse'
            Prelude.<$> (x Data..?> "AdminAccountId")
            Prelude.<*> (x Data..?> "Data")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ServiceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProtectionStatus where
  hashWithSalt _salt GetProtectionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` memberAccountId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData GetProtectionStatus where
  rnf GetProtectionStatus' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf memberAccountId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders GetProtectionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetProtectionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetProtectionStatus where
  toJSON GetProtectionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MemberAccountId" Data..=)
              Prelude.<$> memberAccountId,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTime" Data..=) Prelude.<$> startTime,
            Prelude.Just ("PolicyId" Data..= policyId)
          ]
      )

instance Data.ToPath GetProtectionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetProtectionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProtectionStatusResponse' smart constructor.
data GetProtectionStatusResponse = GetProtectionStatusResponse'
  { -- | The ID of the Firewall Manager administrator account for this policy.
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
    nextToken :: Prelude.Maybe Prelude.Text,
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
    { adminAccountId =
        Prelude.Nothing,
      data' = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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

-- | The service type that is protected by the policy. Currently, this is
-- always @SHIELD_ADVANCED@.
getProtectionStatusResponse_serviceType :: Lens.Lens' GetProtectionStatusResponse (Prelude.Maybe SecurityServiceType)
getProtectionStatusResponse_serviceType = Lens.lens (\GetProtectionStatusResponse' {serviceType} -> serviceType) (\s@GetProtectionStatusResponse' {} a -> s {serviceType = a} :: GetProtectionStatusResponse)

-- | The response's http status code.
getProtectionStatusResponse_httpStatus :: Lens.Lens' GetProtectionStatusResponse Prelude.Int
getProtectionStatusResponse_httpStatus = Lens.lens (\GetProtectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetProtectionStatusResponse' {} a -> s {httpStatus = a} :: GetProtectionStatusResponse)

instance Prelude.NFData GetProtectionStatusResponse where
  rnf GetProtectionStatusResponse' {..} =
    Prelude.rnf adminAccountId
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf httpStatus
