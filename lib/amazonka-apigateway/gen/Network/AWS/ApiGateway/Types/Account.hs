{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Account
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Account
  ( Account (..)
  -- * Smart constructor
  , mkAccount
  -- * Lenses
  , aApiKeyVersion
  , aCloudwatchRoleArn
  , aFeatures
  , aThrottleSettings
  ) where

import qualified Network.AWS.ApiGateway.Types.ThrottleSettings as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { apiKeyVersion :: Core.Maybe Core.Text
    -- ^ The version of the API keys used for the account.
  , cloudwatchRoleArn :: Core.Maybe Core.Text
    -- ^ The ARN of an Amazon CloudWatch role for the current 'Account' . 
  , features :: Core.Maybe [Core.Text]
    -- ^ A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
  , throttleSettings :: Core.Maybe Types.ThrottleSettings
    -- ^ Specifies the API request limits configured for the current 'Account' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Account' value with any optional fields omitted.
mkAccount
    :: Account
mkAccount
  = Account'{apiKeyVersion = Core.Nothing,
             cloudwatchRoleArn = Core.Nothing, features = Core.Nothing,
             throttleSettings = Core.Nothing}

-- | The version of the API keys used for the account.
--
-- /Note:/ Consider using 'apiKeyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApiKeyVersion :: Lens.Lens' Account (Core.Maybe Core.Text)
aApiKeyVersion = Lens.field @"apiKeyVersion"
{-# INLINEABLE aApiKeyVersion #-}
{-# DEPRECATED apiKeyVersion "Use generic-lens or generic-optics with 'apiKeyVersion' instead"  #-}

-- | The ARN of an Amazon CloudWatch role for the current 'Account' . 
--
-- /Note:/ Consider using 'cloudwatchRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchRoleArn :: Lens.Lens' Account (Core.Maybe Core.Text)
aCloudwatchRoleArn = Lens.field @"cloudwatchRoleArn"
{-# INLINEABLE aCloudwatchRoleArn #-}
{-# DEPRECATED cloudwatchRoleArn "Use generic-lens or generic-optics with 'cloudwatchRoleArn' instead"  #-}

-- | A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
--
-- /Note:/ Consider using 'features' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFeatures :: Lens.Lens' Account (Core.Maybe [Core.Text])
aFeatures = Lens.field @"features"
{-# INLINEABLE aFeatures #-}
{-# DEPRECATED features "Use generic-lens or generic-optics with 'features' instead"  #-}

-- | Specifies the API request limits configured for the current 'Account' .
--
-- /Note:/ Consider using 'throttleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aThrottleSettings :: Lens.Lens' Account (Core.Maybe Types.ThrottleSettings)
aThrottleSettings = Lens.field @"throttleSettings"
{-# INLINEABLE aThrottleSettings #-}
{-# DEPRECATED throttleSettings "Use generic-lens or generic-optics with 'throttleSettings' instead"  #-}

instance Core.FromJSON Account where
        parseJSON
          = Core.withObject "Account" Core.$
              \ x ->
                Account' Core.<$>
                  (x Core..:? "apiKeyVersion") Core.<*>
                    x Core..:? "cloudwatchRoleArn"
                    Core.<*> x Core..:? "features"
                    Core.<*> x Core..:? "throttleSettings"
