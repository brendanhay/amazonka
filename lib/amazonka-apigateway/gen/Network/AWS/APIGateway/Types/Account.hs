{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Account
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Account where

import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an AWS account that is associated with API Gateway.
--
--
-- To view the account info, call @GET@ on this resource.
--
-- __Error Codes__
-- The following exception may be thrown when the request fails.
--
--     * UnauthorizedException    * NotFoundException    * TooManyRequestsException
--
-- For detailed error code information, including the corresponding HTTP Status Codes, see <https://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes API Gateway Error Codes>
--
-- __Example: Get the information about an account.__
-- __Request__
-- @@GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
--
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } } @ @ In addition to making the REST API call directly, you can use the AWS CLI and an AWS SDK to access this resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html API Gateway Limits> <https://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html Developer Guide> , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html AWS CLI>
--
-- /See:/ 'account' smart constructor.
data Account = Account'
  { _aApiKeyVersion :: !(Maybe Text),
    _aCloudwatchRoleARN :: !(Maybe Text),
    _aFeatures :: !(Maybe [Text]),
    _aThrottleSettings :: !(Maybe ThrottleSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aApiKeyVersion' - The version of the API keys used for the account.
--
-- * 'aCloudwatchRoleARN' - The ARN of an Amazon CloudWatch role for the current 'Account' .
--
-- * 'aFeatures' - A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
--
-- * 'aThrottleSettings' - Specifies the API request limits configured for the current 'Account' .
account ::
  Account
account =
  Account'
    { _aApiKeyVersion = Nothing,
      _aCloudwatchRoleARN = Nothing,
      _aFeatures = Nothing,
      _aThrottleSettings = Nothing
    }

-- | The version of the API keys used for the account.
aApiKeyVersion :: Lens' Account (Maybe Text)
aApiKeyVersion = lens _aApiKeyVersion (\s a -> s {_aApiKeyVersion = a})

-- | The ARN of an Amazon CloudWatch role for the current 'Account' .
aCloudwatchRoleARN :: Lens' Account (Maybe Text)
aCloudwatchRoleARN = lens _aCloudwatchRoleARN (\s a -> s {_aCloudwatchRoleARN = a})

-- | A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
aFeatures :: Lens' Account [Text]
aFeatures = lens _aFeatures (\s a -> s {_aFeatures = a}) . _Default . _Coerce

-- | Specifies the API request limits configured for the current 'Account' .
aThrottleSettings :: Lens' Account (Maybe ThrottleSettings)
aThrottleSettings = lens _aThrottleSettings (\s a -> s {_aThrottleSettings = a})

instance FromJSON Account where
  parseJSON =
    withObject
      "Account"
      ( \x ->
          Account'
            <$> (x .:? "apiKeyVersion")
            <*> (x .:? "cloudwatchRoleArn")
            <*> (x .:? "features" .!= mempty)
            <*> (x .:? "throttleSettings")
      )

instance Hashable Account

instance NFData Account
