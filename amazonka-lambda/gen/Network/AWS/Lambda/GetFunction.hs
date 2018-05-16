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
-- Returns the configuration information of the Lambda function and a presigned URL link to the .zip file you uploaded with 'CreateFunction' so you can download the .zip file. Note that the URL is valid for up to 10 minutes. The configuration information is the same information you provided as parameters when uploading the function.
--
--
-- Using the optional @Qualifier@ parameter, you can specify a specific function version for which you want this information. If you don't specify this parameter, the API uses unqualified function ARN which return information about the @> LATEST@ version of the Lambda function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:GetFunction@ action.
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

-- |
--
--
--
-- /See:/ 'getFunction' smart constructor.
data GetFunction = GetFunction'
  { _gfQualifier    :: !(Maybe Text)
  , _gfFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfQualifier' - Use this optional parameter to specify a function version or an alias name. If you specify function version, the API uses qualified function ARN for the request and returns information about the specific Lambda function version. If you specify an alias name, the API uses the alias ARN and returns information about the function version to which the alias points. If you don't provide this parameter, the API uses unqualified function ARN and returns information about the @> LATEST@ version of the Lambda function.
--
-- * 'gfFunctionName' - The Lambda function name. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunction
    :: Text -- ^ 'gfFunctionName'
    -> GetFunction
getFunction pFunctionName_ =
  GetFunction' {_gfQualifier = Nothing, _gfFunctionName = pFunctionName_}


-- | Use this optional parameter to specify a function version or an alias name. If you specify function version, the API uses qualified function ARN for the request and returns information about the specific Lambda function version. If you specify an alias name, the API uses the alias ARN and returns information about the function version to which the alias points. If you don't provide this parameter, the API uses unqualified function ARN and returns information about the @> LATEST@ version of the Lambda function.
gfQualifier :: Lens' GetFunction (Maybe Text)
gfQualifier = lens _gfQualifier (\ s a -> s{_gfQualifier = a})

-- | The Lambda function name. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
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

-- | This response contains the object for the Lambda function location (see 'FunctionCodeLocation' .
--
--
--
-- /See:/ 'getFunctionResponse' smart constructor.
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
-- * 'gfrsConcurrency' - The concurrent execution limit set for this function. For more information, see 'concurrent-executions' .
--
-- * 'gfrsCode' - Undocumented member.
--
-- * 'gfrsConfiguration' - Undocumented member.
--
-- * 'gfrsTags' - Returns the list of tags associated with the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
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


-- | The concurrent execution limit set for this function. For more information, see 'concurrent-executions' .
gfrsConcurrency :: Lens' GetFunctionResponse (Maybe Concurrency)
gfrsConcurrency = lens _gfrsConcurrency (\ s a -> s{_gfrsConcurrency = a})

-- | Undocumented member.
gfrsCode :: Lens' GetFunctionResponse (Maybe FunctionCodeLocation)
gfrsCode = lens _gfrsCode (\ s a -> s{_gfrsCode = a})

-- | Undocumented member.
gfrsConfiguration :: Lens' GetFunctionResponse (Maybe FunctionConfiguration)
gfrsConfiguration = lens _gfrsConfiguration (\ s a -> s{_gfrsConfiguration = a})

-- | Returns the list of tags associated with the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
gfrsTags :: Lens' GetFunctionResponse (HashMap Text Text)
gfrsTags = lens _gfrsTags (\ s a -> s{_gfrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFunctionResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\ s a -> s{_gfrsResponseStatus = a})

instance NFData GetFunctionResponse where
