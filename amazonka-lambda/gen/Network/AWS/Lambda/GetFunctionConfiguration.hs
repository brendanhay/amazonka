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
-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information of the Lambda function. This the
-- same information you provided as parameters when uploading the function
-- by using CreateFunction.
--
-- This operation requires permission for the
-- @lambda:GetFunctionConfiguration@ operation.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunctionConfiguration.html AWS API Reference> for GetFunctionConfiguration.
module Network.AWS.Lambda.GetFunctionConfiguration
    (
    -- * Creating a Request
      GetFunctionConfiguration
    , getFunctionConfiguration
    -- * Request Lenses
    , gfcFunctionName

    -- * Destructuring the Response
    , FunctionConfiguration
    , functionConfiguration
    -- * Response Lenses
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunctionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfcFunctionName'
newtype GetFunctionConfiguration = GetFunctionConfiguration'
    { _gfcFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetFunctionConfiguration' smart constructor.
getFunctionConfiguration :: Text -> GetFunctionConfiguration
getFunctionConfiguration pFunctionName_ = 
    GetFunctionConfiguration'
    { _gfcFunctionName = pFunctionName_
    }

-- | The name of the Lambda function for which you want to retrieve the
-- configuration information.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
gfcFunctionName :: Lens' GetFunctionConfiguration Text
gfcFunctionName = lens _gfcFunctionName (\ s a -> s{_gfcFunctionName = a});

instance AWSRequest GetFunctionConfiguration where
        type Sv GetFunctionConfiguration = Lambda
        type Rs GetFunctionConfiguration =
             FunctionConfiguration
        request = get
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders GetFunctionConfiguration where
        toHeaders = const mempty

instance ToPath GetFunctionConfiguration where
        toPath GetFunctionConfiguration'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gfcFunctionName,
               "/versions/HEAD/configuration"]

instance ToQuery GetFunctionConfiguration where
        toQuery = const mempty
