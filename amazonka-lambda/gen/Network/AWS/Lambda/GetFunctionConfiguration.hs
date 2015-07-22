{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information of the Lambda function. This the
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
    , gfcrqFunctionName

    -- * Response
    , FunctionConfiguration
    -- ** Response constructor
    , functionConfiguration
    -- ** Response lenses
    , gfcrsRuntime
    , gfcrsMemorySize
    , gfcrsFunctionARN
    , gfcrsRole
    , gfcrsFunctionName
    , gfcrsCodeSize
    , gfcrsHandler
    , gfcrsTimeout
    , gfcrsLastModified
    , gfcrsDescription
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getFunctionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfcrqFunctionName'
newtype GetFunctionConfiguration = GetFunctionConfiguration'
    { _gfcrqFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetFunctionConfiguration' smart constructor.
getFunctionConfiguration :: Text -> GetFunctionConfiguration
getFunctionConfiguration pFunctionName =
    GetFunctionConfiguration'
    { _gfcrqFunctionName = pFunctionName
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
gfcrqFunctionName :: Lens' GetFunctionConfiguration Text
gfcrqFunctionName = lens _gfcrqFunctionName (\ s a -> s{_gfcrqFunctionName = a});

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
              ["/2015-03-31/functions/", toText _gfcrqFunctionName,
               "/versions/HEAD/configuration"]

instance ToQuery GetFunctionConfiguration where
        toQuery = const mempty
