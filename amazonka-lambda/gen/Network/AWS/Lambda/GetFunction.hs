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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information of the Lambda function and a
-- presigned URL link to the .zip file you uploaded with CreateFunction so
-- you can download the .zip file. Note that the URL is valid for up to 10
-- minutes. The configuration information is the same information you
-- provided as parameters when uploading the function.
--
-- This operation requires permission for the 'lambda:GetFunction' action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunction.html AWS API Reference> for GetFunction.
module Network.AWS.Lambda.GetFunction
    (
    -- * Creating a Request
      getFunction
    , GetFunction
    -- * Request Lenses
    , gfFunctionName

    -- * Destructuring the Response
    , getFunctionResponse
    , GetFunctionResponse
    -- * Response Lenses
    , gfrsCode
    , gfrsConfiguration
    , gfrsStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getFunction' smart constructor.
newtype GetFunction = GetFunction'
    { _gfFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfFunctionName'
getFunction
    :: Text -- ^ 'gfFunctionName'
    -> GetFunction
getFunction pFunctionName_ =
    GetFunction'
    { _gfFunctionName = pFunctionName_
    }

-- | The Lambda function name.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
gfFunctionName :: Lens' GetFunction Text
gfFunctionName = lens _gfFunctionName (\ s a -> s{_gfFunctionName = a});

instance AWSRequest GetFunction where
        type Rs GetFunction = GetFunctionResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 GetFunctionResponse' <$>
                   (x .?> "Code") <*> (x .?> "Configuration") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetFunction where
        toHeaders = const mempty

instance ToPath GetFunction where
        toPath GetFunction'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gfFunctionName,
               "/versions/HEAD"]

instance ToQuery GetFunction where
        toQuery = const mempty

-- | This response contains the object for the Lambda function location (see
-- API_FunctionCodeLocation
--
-- /See:/ 'getFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
    { _gfrsCode          :: !(Maybe FunctionCodeLocation)
    , _gfrsConfiguration :: !(Maybe FunctionConfiguration)
    , _gfrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsCode'
--
-- * 'gfrsConfiguration'
--
-- * 'gfrsStatus'
getFunctionResponse
    :: Int -- ^ 'gfrsStatus'
    -> GetFunctionResponse
getFunctionResponse pStatus_ =
    GetFunctionResponse'
    { _gfrsCode = Nothing
    , _gfrsConfiguration = Nothing
    , _gfrsStatus = pStatus_
    }

-- | Undocumented member.
gfrsCode :: Lens' GetFunctionResponse (Maybe FunctionCodeLocation)
gfrsCode = lens _gfrsCode (\ s a -> s{_gfrsCode = a});

-- | Undocumented member.
gfrsConfiguration :: Lens' GetFunctionResponse (Maybe FunctionConfiguration)
gfrsConfiguration = lens _gfrsConfiguration (\ s a -> s{_gfrsConfiguration = a});

-- | The response status code.
gfrsStatus :: Lens' GetFunctionResponse Int
gfrsStatus = lens _gfrsStatus (\ s a -> s{_gfrsStatus = a});
