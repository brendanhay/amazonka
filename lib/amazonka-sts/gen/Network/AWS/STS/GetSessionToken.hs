{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetSessionToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary credentials for an AWS account or IAM user. The credentials consist of an access key ID, a secret access key, and a security token. Typically, you use @GetSessionToken@ if you want to use MFA to protect programmatic calls to specific AWS API operations like Amazon EC2 @StopInstances@ . MFA-enabled IAM users would need to call @GetSessionToken@ and submit an MFA code that is associated with their MFA device. Using the temporary security credentials that are returned from the call, IAM users can then make programmatic calls to API operations that require MFA authentication. If you do not supply a correct MFA code, then the API returns an access denied error. For a comparison of @GetSessionToken@ with the other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
--
-- __Session Duration__
-- The @GetSessionToken@ operation must be called by using the long-term AWS security credentials of the AWS account root user or an IAM user. Credentials that are created by IAM users are valid for the duration that you specify. This duration can range from 900 seconds (15 minutes) up to a maximum of 129,600 seconds (36 hours), with a default of 43,200 seconds (12 hours). Credentials based on account credentials can range from 900 seconds (15 minutes) up to 3,600 seconds (1 hour), with a default of 1 hour.
-- __Permissions__
-- The temporary security credentials created by @GetSessionToken@ can be used to make API calls to any AWS service with the following exceptions:
--
--     * You cannot call any IAM API operations unless MFA authentication information is included in the request.
--
--
--     * You cannot call any STS API /except/ @AssumeRole@ or @GetCallerIdentity@ .
--
--
-- The credentials that are returned by @GetSessionToken@ are based on permissions associated with the user whose credentials were used to call the operation. If @GetSessionToken@ is called using AWS account root user credentials, the temporary credentials have root user permissions. Similarly, if @GetSessionToken@ is called using the credentials of an IAM user, the temporary credentials have the same permissions as the IAM user.
-- For more information about using @GetSessionToken@ to create temporary credentials, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getsessiontoken Temporary Credentials for Users in Untrusted Environments> in the /IAM User Guide/ .
module Network.AWS.STS.GetSessionToken
  ( -- * Creating a request
    GetSessionToken (..),
    mkGetSessionToken,

    -- ** Request lenses
    gstDurationSeconds,
    gstSerialNumber,
    gstTokenCode,

    -- * Destructuring the response
    GetSessionTokenResponse (..),
    mkGetSessionTokenResponse,

    -- ** Response lenses
    gstrrsCredentials,
    gstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkGetSessionToken' smart constructor.
data GetSessionToken = GetSessionToken'
  { -- | The duration, in seconds, that the credentials should remain valid. Acceptable durations for IAM user sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions for AWS account owners are restricted to a maximum of 3,600 seconds (one hour). If the duration is longer than one hour, the session for AWS account owners defaults to one hour.
    durationSeconds :: Core.Maybe Core.Natural,
    -- | The identification number of the MFA device that is associated with the IAM user who is making the @GetSessionToken@ call. Specify this value if the IAM user has a policy that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). You can find the device for an IAM user by going to the AWS Management Console and viewing the user's security credentials.
    --
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
    serialNumber :: Core.Maybe Types.SerialNumberType,
    -- | The value provided by the MFA device, if MFA is required. If any policy requires the IAM user to submit an MFA code, specify this value. If MFA authentication is required, the user must provide a code when requesting a set of temporary security credentials. A user who fails to provide the code receives an "access denied" response when requesting resources that require MFA authentication.
    --
    -- The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
    tokenCode :: Core.Maybe Types.TokenCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSessionToken' value with any optional fields omitted.
mkGetSessionToken ::
  GetSessionToken
mkGetSessionToken =
  GetSessionToken'
    { durationSeconds = Core.Nothing,
      serialNumber = Core.Nothing,
      tokenCode = Core.Nothing
    }

-- | The duration, in seconds, that the credentials should remain valid. Acceptable durations for IAM user sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions for AWS account owners are restricted to a maximum of 3,600 seconds (one hour). If the duration is longer than one hour, the session for AWS account owners defaults to one hour.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstDurationSeconds :: Lens.Lens' GetSessionToken (Core.Maybe Core.Natural)
gstDurationSeconds = Lens.field @"durationSeconds"
{-# DEPRECATED gstDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | The identification number of the MFA device that is associated with the IAM user who is making the @GetSessionToken@ call. Specify this value if the IAM user has a policy that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). You can find the device for an IAM user by going to the AWS Management Console and viewing the user's security credentials.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstSerialNumber :: Lens.Lens' GetSessionToken (Core.Maybe Types.SerialNumberType)
gstSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED gstSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The value provided by the MFA device, if MFA is required. If any policy requires the IAM user to submit an MFA code, specify this value. If MFA authentication is required, the user must provide a code when requesting a set of temporary security credentials. A user who fails to provide the code receives an "access denied" response when requesting resources that require MFA authentication.
--
-- The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
--
-- /Note:/ Consider using 'tokenCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstTokenCode :: Lens.Lens' GetSessionToken (Core.Maybe Types.TokenCode)
gstTokenCode = Lens.field @"tokenCode"
{-# DEPRECATED gstTokenCode "Use generic-lens or generic-optics with 'tokenCode' instead." #-}

instance Core.AWSRequest GetSessionToken where
  type Rs GetSessionToken = GetSessionTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetSessionToken")
                Core.<> (Core.pure ("Version", "2011-06-15"))
                Core.<> (Core.toQueryValue "DurationSeconds" Core.<$> durationSeconds)
                Core.<> (Core.toQueryValue "SerialNumber" Core.<$> serialNumber)
                Core.<> (Core.toQueryValue "TokenCode" Core.<$> tokenCode)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetSessionTokenResult"
      ( \s h x ->
          GetSessionTokenResponse'
            Core.<$> (x Core..@? "Credentials") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetSessionToken' request, including temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'mkGetSessionTokenResponse' smart constructor.
data GetSessionTokenResponse = GetSessionTokenResponse'
  { -- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
    credentials :: Core.Maybe Types.AuthEnv,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSessionTokenResponse' value with any optional fields omitted.
mkGetSessionTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSessionTokenResponse
mkGetSessionTokenResponse responseStatus =
  GetSessionTokenResponse'
    { credentials = Core.Nothing,
      responseStatus
    }

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsCredentials :: Lens.Lens' GetSessionTokenResponse (Core.Maybe Types.AuthEnv)
gstrrsCredentials = Lens.field @"credentials"
{-# DEPRECATED gstrrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsResponseStatus :: Lens.Lens' GetSessionTokenResponse Core.Int
gstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
