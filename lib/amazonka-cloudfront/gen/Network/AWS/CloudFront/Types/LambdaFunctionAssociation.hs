{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociation where

import Network.AWS.CloudFront.Types.EventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains a Lambda function association.
--
--
--
-- /See:/ 'lambdaFunctionAssociation' smart constructor.
data LambdaFunctionAssociation = LambdaFunctionAssociation'
  { _lfaIncludeBody ::
      !(Maybe Bool),
    _lfaLambdaFunctionARN :: !Text,
    _lfaEventType :: !EventType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfaIncludeBody' - A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
--
-- * 'lfaLambdaFunctionARN' - The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
--
-- * 'lfaEventType' - Specifies the event type that triggers a Lambda function invocation. You can specify the following values:     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.      * @origin-request@ : The function executes only when CloudFront sends a request to your origin. When the requested object is in the edge cache, the function doesn't execute.     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute.     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
lambdaFunctionAssociation ::
  -- | 'lfaLambdaFunctionARN'
  Text ->
  -- | 'lfaEventType'
  EventType ->
  LambdaFunctionAssociation
lambdaFunctionAssociation pLambdaFunctionARN_ pEventType_ =
  LambdaFunctionAssociation'
    { _lfaIncludeBody = Nothing,
      _lfaLambdaFunctionARN = pLambdaFunctionARN_,
      _lfaEventType = pEventType_
    }

-- | A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
lfaIncludeBody :: Lens' LambdaFunctionAssociation (Maybe Bool)
lfaIncludeBody = lens _lfaIncludeBody (\s a -> s {_lfaIncludeBody = a})

-- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
lfaLambdaFunctionARN :: Lens' LambdaFunctionAssociation Text
lfaLambdaFunctionARN = lens _lfaLambdaFunctionARN (\s a -> s {_lfaLambdaFunctionARN = a})

-- | Specifies the event type that triggers a Lambda function invocation. You can specify the following values:     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.      * @origin-request@ : The function executes only when CloudFront sends a request to your origin. When the requested object is in the edge cache, the function doesn't execute.     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute.     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
lfaEventType :: Lens' LambdaFunctionAssociation EventType
lfaEventType = lens _lfaEventType (\s a -> s {_lfaEventType = a})

instance FromXML LambdaFunctionAssociation where
  parseXML x =
    LambdaFunctionAssociation'
      <$> (x .@? "IncludeBody")
      <*> (x .@ "LambdaFunctionARN")
      <*> (x .@ "EventType")

instance Hashable LambdaFunctionAssociation

instance NFData LambdaFunctionAssociation

instance ToXML LambdaFunctionAssociation where
  toXML LambdaFunctionAssociation' {..} =
    mconcat
      [ "IncludeBody" @= _lfaIncludeBody,
        "LambdaFunctionARN" @= _lfaLambdaFunctionARN,
        "EventType" @= _lfaEventType
      ]
