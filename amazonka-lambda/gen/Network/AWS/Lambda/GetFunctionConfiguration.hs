{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the configuration information of the Lambda function. This the
-- same information you provided as parameters when uploading the function
-- by using CreateFunction.
--
-- This operation requires permission for the
-- @lambda:GetFunctionConfiguration@ operation.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunctionConfiguration.html>
module Network.AWS.Lambda.GetFunctionConfiguration
    (
    -- * Request
      GetFunctionConfiguration
    -- ** Request constructor
    , getFunctionConfiguration
    -- ** Request lenses
    , gfcFunctionName

    -- * Response
    , FunctionConfiguration
    -- ** Response constructor
    , functionConfiguration
    -- ** Response lenses
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

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
getFunctionConfiguration pFunctionName =
    GetFunctionConfiguration'
    { _gfcFunctionName = pFunctionName
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
              ["/2015-03-31/functions/", toText _gfcFunctionName,
               "/versions/HEAD/configuration"]

instance ToQuery GetFunctionConfiguration where
        toQuery = const mempty
