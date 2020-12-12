{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Account
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Account
  ( Account (..),

    -- * Smart constructor
    mkAccount,

    -- * Lenses
    aApiKeyVersion,
    aCloudwatchRoleARN,
    aFeatures,
    aThrottleSettings,
  )
where

import Network.AWS.APIGateway.Types.ThrottleSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an AWS account that is associated with API Gateway.
--
-- To view the account info, call @GET@ on this resource.
-- __Error Codes__
-- The following exception may be thrown when the request fails.
--
--     * UnauthorizedException
--
--     * NotFoundException
--
--     * TooManyRequestsException
--
-- For detailed error code information, including the corresponding HTTP Status Codes, see <https://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes API Gateway Error Codes>
-- __Example: Get the information about an account.__
-- __Request__
-- @@GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } } @ @ In addition to making the REST API call directly, you can use the AWS CLI and an AWS SDK to access this resource.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html API Gateway Limits> <https://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html Developer Guide> , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html AWS CLI>
--
-- /See:/ 'mkAccount' smart constructor.
data Account = Account'
  { apiKeyVersion :: Lude.Maybe Lude.Text,
    cloudwatchRoleARN :: Lude.Maybe Lude.Text,
    features :: Lude.Maybe [Lude.Text],
    throttleSettings :: Lude.Maybe ThrottleSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- * 'apiKeyVersion' - The version of the API keys used for the account.
-- * 'cloudwatchRoleARN' - The ARN of an Amazon CloudWatch role for the current 'Account' .
-- * 'features' - A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
-- * 'throttleSettings' - Specifies the API request limits configured for the current 'Account' .
mkAccount ::
  Account
mkAccount =
  Account'
    { apiKeyVersion = Lude.Nothing,
      cloudwatchRoleARN = Lude.Nothing,
      features = Lude.Nothing,
      throttleSettings = Lude.Nothing
    }

-- | The version of the API keys used for the account.
--
-- /Note:/ Consider using 'apiKeyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApiKeyVersion :: Lens.Lens' Account (Lude.Maybe Lude.Text)
aApiKeyVersion = Lens.lens (apiKeyVersion :: Account -> Lude.Maybe Lude.Text) (\s a -> s {apiKeyVersion = a} :: Account)
{-# DEPRECATED aApiKeyVersion "Use generic-lens or generic-optics with 'apiKeyVersion' instead." #-}

-- | The ARN of an Amazon CloudWatch role for the current 'Account' .
--
-- /Note:/ Consider using 'cloudwatchRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchRoleARN :: Lens.Lens' Account (Lude.Maybe Lude.Text)
aCloudwatchRoleARN = Lens.lens (cloudwatchRoleARN :: Account -> Lude.Maybe Lude.Text) (\s a -> s {cloudwatchRoleARN = a} :: Account)
{-# DEPRECATED aCloudwatchRoleARN "Use generic-lens or generic-optics with 'cloudwatchRoleARN' instead." #-}

-- | A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
--
-- /Note:/ Consider using 'features' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFeatures :: Lens.Lens' Account (Lude.Maybe [Lude.Text])
aFeatures = Lens.lens (features :: Account -> Lude.Maybe [Lude.Text]) (\s a -> s {features = a} :: Account)
{-# DEPRECATED aFeatures "Use generic-lens or generic-optics with 'features' instead." #-}

-- | Specifies the API request limits configured for the current 'Account' .
--
-- /Note:/ Consider using 'throttleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aThrottleSettings :: Lens.Lens' Account (Lude.Maybe ThrottleSettings)
aThrottleSettings = Lens.lens (throttleSettings :: Account -> Lude.Maybe ThrottleSettings) (\s a -> s {throttleSettings = a} :: Account)
{-# DEPRECATED aThrottleSettings "Use generic-lens or generic-optics with 'throttleSettings' instead." #-}

instance Lude.FromJSON Account where
  parseJSON =
    Lude.withObject
      "Account"
      ( \x ->
          Account'
            Lude.<$> (x Lude..:? "apiKeyVersion")
            Lude.<*> (x Lude..:? "cloudwatchRoleArn")
            Lude.<*> (x Lude..:? "features" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "throttleSettings")
      )
