{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the function or function version, with a link to download the deployment package that's valid for 10 minutes. If you specify a function version, only details that are specific to that version are returned.
--
--
module Network.AWS.Lambda.GetFunction
    (
    -- * Creating a Request
      getFunction
    , GetFunction
    -- * Request Lenses
    , gfQualifier
    , gfFunctionName

    -- * Destructuring the Response
    , getFunctionResponse
    , GetFunctionResponse
    -- * Response Lenses
    , gfrsConcurrency
    , gfrsCode
    , gfrsConfiguration
    , gfrsTags
    , gfrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunction' smart constructor.
data GetFunction = GetFunction'
  { _gfQualifier    :: !(Maybe Text)
  , _gfFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfQualifier' - Specify a version or alias to get details about a published version of the function.
--
-- * 'gfFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunction
    :: Text -- ^ 'gfFunctionName'
    -> GetFunction
getFunction pFunctionName_ =
  GetFunction' {_gfQualifier = Nothing, _gfFunctionName = pFunctionName_}


-- | Specify a version or alias to get details about a published version of the function.
gfQualifier :: Lens' GetFunction (Maybe Text)
gfQualifier = lens _gfQualifier (\ s a -> s{_gfQualifier = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gfFunctionName :: Lens' GetFunction Text
gfFunctionName = lens _gfFunctionName (\ s a -> s{_gfFunctionName = a})

instance AWSRequest GetFunction where
        type Rs GetFunction = GetFunctionResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 GetFunctionResponse' <$>
                   (x .?> "Concurrency") <*> (x .?> "Code") <*>
                     (x .?> "Configuration")
                     <*> (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetFunction where

instance NFData GetFunction where

instance ToHeaders GetFunction where
        toHeaders = const mempty

instance ToPath GetFunction where
        toPath GetFunction'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gfFunctionName]

instance ToQuery GetFunction where
        toQuery GetFunction'{..}
          = mconcat ["Qualifier" =: _gfQualifier]

-- | /See:/ 'getFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { _gfrsConcurrency    :: !(Maybe Concurrency)
  , _gfrsCode           :: !(Maybe FunctionCodeLocation)
  , _gfrsConfiguration  :: !(Maybe FunctionConfiguration)
  , _gfrsTags           :: !(Maybe (Map Text Text))
  , _gfrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsConcurrency' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
--
-- * 'gfrsCode' - The deployment package of the function or version.
--
-- * 'gfrsConfiguration' - The configuration of the function or version.
--
-- * 'gfrsTags' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
--
-- * 'gfrsResponseStatus' - -- | The response status code.
getFunctionResponse
    :: Int -- ^ 'gfrsResponseStatus'
    -> GetFunctionResponse
getFunctionResponse pResponseStatus_ =
  GetFunctionResponse'
    { _gfrsConcurrency = Nothing
    , _gfrsCode = Nothing
    , _gfrsConfiguration = Nothing
    , _gfrsTags = Nothing
    , _gfrsResponseStatus = pResponseStatus_
    }


-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
gfrsConcurrency :: Lens' GetFunctionResponse (Maybe Concurrency)
gfrsConcurrency = lens _gfrsConcurrency (\ s a -> s{_gfrsConcurrency = a})

-- | The deployment package of the function or version.
gfrsCode :: Lens' GetFunctionResponse (Maybe FunctionCodeLocation)
gfrsCode = lens _gfrsCode (\ s a -> s{_gfrsCode = a})

-- | The configuration of the function or version.
gfrsConfiguration :: Lens' GetFunctionResponse (Maybe FunctionConfiguration)
gfrsConfiguration = lens _gfrsConfiguration (\ s a -> s{_gfrsConfiguration = a})

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
gfrsTags :: Lens' GetFunctionResponse (HashMap Text Text)
gfrsTags = lens _gfrsTags (\ s a -> s{_gfrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFunctionResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\ s a -> s{_gfrsResponseStatus = a})

instance NFData GetFunctionResponse where
