{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetProtectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you created a Shield Advanced policy, returns policy-level attack summary information in the event of a potential DDoS attack. Other policy types are currently unsupported.
module Network.AWS.FMS.GetProtectionStatus
  ( -- * Creating a request
    GetProtectionStatus (..),
    mkGetProtectionStatus,

    -- ** Request lenses
    gpsMemberAccountId,
    gpsStartTime,
    gpsNextToken,
    gpsEndTime,
    gpsMaxResults,
    gpsPolicyId,

    -- * Destructuring the response
    GetProtectionStatusResponse (..),
    mkGetProtectionStatusResponse,

    -- ** Response lenses
    gpsrsData,
    gpsrsAdminAccountId,
    gpsrsNextToken,
    gpsrsServiceType,
    gpsrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetProtectionStatus' smart constructor.
data GetProtectionStatus = GetProtectionStatus'
  { memberAccountId ::
      Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    maxResults :: Lude.Maybe Lude.Natural,
    policyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProtectionStatus' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
-- * 'maxResults' - Specifies the number of objects that you want AWS Firewall Manager to return for this request. If you have more objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of objects.
-- * 'memberAccountId' - The AWS account that is in scope of the policy that you want to get the details for.
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response, which you can use to retrieve another group of objects. For the second and subsequent @GetProtectionStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of objects.
-- * 'policyId' - The ID of the policy for which you want to get the attack information.
-- * 'startTime' - The start of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
mkGetProtectionStatus ::
  -- | 'policyId'
  Lude.Text ->
  GetProtectionStatus
mkGetProtectionStatus pPolicyId_ =
  GetProtectionStatus'
    { memberAccountId = Lude.Nothing,
      startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      maxResults = Lude.Nothing,
      policyId = pPolicyId_
    }

-- | The AWS account that is in scope of the policy that you want to get the details for.
--
-- /Note:/ Consider using 'memberAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMemberAccountId :: Lens.Lens' GetProtectionStatus (Lude.Maybe Lude.Text)
gpsMemberAccountId = Lens.lens (memberAccountId :: GetProtectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {memberAccountId = a} :: GetProtectionStatus)
{-# DEPRECATED gpsMemberAccountId "Use generic-lens or generic-optics with 'memberAccountId' instead." #-}

-- | The start of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsStartTime :: Lens.Lens' GetProtectionStatus (Lude.Maybe Lude.Timestamp)
gpsStartTime = Lens.lens (startTime :: GetProtectionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetProtectionStatus)
{-# DEPRECATED gpsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response, which you can use to retrieve another group of objects. For the second and subsequent @GetProtectionStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsNextToken :: Lens.Lens' GetProtectionStatus (Lude.Maybe Lude.Text)
gpsNextToken = Lens.lens (nextToken :: GetProtectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetProtectionStatus)
{-# DEPRECATED gpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The end of the time period to query for the attacks. This is a @timestamp@ type. The request syntax listing indicates a @number@ type because the default used by AWS Firewall Manager is Unix time in seconds. However, any valid @timestamp@ format is allowed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsEndTime :: Lens.Lens' GetProtectionStatus (Lude.Maybe Lude.Timestamp)
gpsEndTime = Lens.lens (endTime :: GetProtectionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetProtectionStatus)
{-# DEPRECATED gpsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Specifies the number of objects that you want AWS Firewall Manager to return for this request. If you have more objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMaxResults :: Lens.Lens' GetProtectionStatus (Lude.Maybe Lude.Natural)
gpsMaxResults = Lens.lens (maxResults :: GetProtectionStatus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetProtectionStatus)
{-# DEPRECATED gpsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the policy for which you want to get the attack information.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsPolicyId :: Lens.Lens' GetProtectionStatus Lude.Text
gpsPolicyId = Lens.lens (policyId :: GetProtectionStatus -> Lude.Text) (\s a -> s {policyId = a} :: GetProtectionStatus)
{-# DEPRECATED gpsPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Lude.AWSRequest GetProtectionStatus where
  type Rs GetProtectionStatus = GetProtectionStatusResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProtectionStatusResponse'
            Lude.<$> (x Lude..?> "Data")
            Lude.<*> (x Lude..?> "AdminAccountId")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ServiceType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProtectionStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetProtectionStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProtectionStatus where
  toJSON GetProtectionStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MemberAccountId" Lude..=) Lude.<$> memberAccountId,
            ("StartTime" Lude..=) Lude.<$> startTime,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("PolicyId" Lude..= policyId)
          ]
      )

instance Lude.ToPath GetProtectionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProtectionStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetProtectionStatusResponse' smart constructor.
data GetProtectionStatusResponse = GetProtectionStatusResponse'
  { data' ::
      Lude.Maybe Lude.Text,
    adminAccountId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    serviceType ::
      Lude.Maybe SecurityServiceType,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProtectionStatusResponse' with the minimum fields required to make a request.
--
-- * 'adminAccountId' - The ID of the AWS Firewall administrator account for this policy.
-- * 'data'' - Details about the attack, including the following:
--
--
--     * Attack type
--
--
--     * Account ID
--
--
--     * ARN of the resource attacked
--
--
--     * Start time of the attack
--
--
--     * End time of the attack (ongoing attacks will not have an end time)
--
--
-- The details are in JSON format.
-- * 'nextToken' - If you have more objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more objects, submit another @GetProtectionStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- AWS SDKs provide auto-pagination that identify @NextToken@ in a response and make subsequent request calls automatically on your behalf. However, this feature is not supported by @GetProtectionStatus@ . You must submit subsequent requests with @NextToken@ using your own processes.
-- * 'responseStatus' - The response status code.
-- * 'serviceType' - The service type that is protected by the policy. Currently, this is always @SHIELD_ADVANCED@ .
mkGetProtectionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProtectionStatusResponse
mkGetProtectionStatusResponse pResponseStatus_ =
  GetProtectionStatusResponse'
    { data' = Lude.Nothing,
      adminAccountId = Lude.Nothing,
      nextToken = Lude.Nothing,
      serviceType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Details about the attack, including the following:
--
--
--     * Attack type
--
--
--     * Account ID
--
--
--     * ARN of the resource attacked
--
--
--     * Start time of the attack
--
--
--     * End time of the attack (ongoing attacks will not have an end time)
--
--
-- The details are in JSON format.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsData :: Lens.Lens' GetProtectionStatusResponse (Lude.Maybe Lude.Text)
gpsrsData = Lens.lens (data' :: GetProtectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: GetProtectionStatusResponse)
{-# DEPRECATED gpsrsData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The ID of the AWS Firewall administrator account for this policy.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsAdminAccountId :: Lens.Lens' GetProtectionStatusResponse (Lude.Maybe Lude.Text)
gpsrsAdminAccountId = Lens.lens (adminAccountId :: GetProtectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {adminAccountId = a} :: GetProtectionStatusResponse)
{-# DEPRECATED gpsrsAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

-- | If you have more objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more objects, submit another @GetProtectionStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- AWS SDKs provide auto-pagination that identify @NextToken@ in a response and make subsequent request calls automatically on your behalf. However, this feature is not supported by @GetProtectionStatus@ . You must submit subsequent requests with @NextToken@ using your own processes.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsNextToken :: Lens.Lens' GetProtectionStatusResponse (Lude.Maybe Lude.Text)
gpsrsNextToken = Lens.lens (nextToken :: GetProtectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetProtectionStatusResponse)
{-# DEPRECATED gpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The service type that is protected by the policy. Currently, this is always @SHIELD_ADVANCED@ .
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsServiceType :: Lens.Lens' GetProtectionStatusResponse (Lude.Maybe SecurityServiceType)
gpsrsServiceType = Lens.lens (serviceType :: GetProtectionStatusResponse -> Lude.Maybe SecurityServiceType) (\s a -> s {serviceType = a} :: GetProtectionStatusResponse)
{-# DEPRECATED gpsrsServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsResponseStatus :: Lens.Lens' GetProtectionStatusResponse Lude.Int
gpsrsResponseStatus = Lens.lens (responseStatus :: GetProtectionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProtectionStatusResponse)
{-# DEPRECATED gpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
