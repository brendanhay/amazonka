{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration parameters for the specified Lambda function
-- by using the values provided in the request. You provide only the
-- parameters you want to change. This operation must only be used on an
-- existing Lambda function and cannot be used to update the function\'s
-- code.
--
-- This operation requires permission for the
-- @lambda:UpdateFunctionConfiguration@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionConfiguration.html>
module Network.AWS.Lambda.UpdateFunctionConfiguration
    (
    -- * Request
      UpdateFunctionConfiguration
    -- ** Request constructor
    , updateFunctionConfiguration
    -- ** Request lenses
    , ufcrqMemorySize
    , ufcrqRole
    , ufcrqHandler
    , ufcrqTimeout
    , ufcrqDescription
    , ufcrqFunctionName

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

-- | /See:/ 'updateFunctionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufcrqMemorySize'
--
-- * 'ufcrqRole'
--
-- * 'ufcrqHandler'
--
-- * 'ufcrqTimeout'
--
-- * 'ufcrqDescription'
--
-- * 'ufcrqFunctionName'
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
    { _ufcrqMemorySize   :: !(Maybe Nat)
    , _ufcrqRole         :: !(Maybe Text)
    , _ufcrqHandler      :: !(Maybe Text)
    , _ufcrqTimeout      :: !(Maybe Nat)
    , _ufcrqDescription  :: !(Maybe Text)
    , _ufcrqFunctionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateFunctionConfiguration' smart constructor.
updateFunctionConfiguration :: Text -> UpdateFunctionConfiguration
updateFunctionConfiguration pFunctionName_ =
    UpdateFunctionConfiguration'
    { _ufcrqMemorySize = Nothing
    , _ufcrqRole = Nothing
    , _ufcrqHandler = Nothing
    , _ufcrqTimeout = Nothing
    , _ufcrqDescription = Nothing
    , _ufcrqFunctionName = pFunctionName_
    }

-- | The amount of memory, in MB, your Lambda function is given. AWS Lambda
-- uses this memory size to infer the amount of CPU allocated to your
-- function. Your function use-case determines your CPU and memory
-- requirements. For example, a database operation might need less memory
-- compared to an image processing function. The default value is 128 MB.
-- The value must be a multiple of 64 MB.
ufcrqMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcrqMemorySize = lens _ufcrqMemorySize (\ s a -> s{_ufcrqMemorySize = a}) . mapping _Nat;

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda will assume
-- when it executes your function.
ufcrqRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcrqRole = lens _ufcrqRole (\ s a -> s{_ufcrqRole = a});

-- | The function that Lambda calls to begin executing your function. For
-- Node.js, it is the /module-name.export/ value in your function.
ufcrqHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcrqHandler = lens _ufcrqHandler (\ s a -> s{_ufcrqHandler = a});

-- | The function execution time at which AWS Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufcrqTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcrqTimeout = lens _ufcrqTimeout (\ s a -> s{_ufcrqTimeout = a}) . mapping _Nat;

-- | A short user-defined function description. AWS Lambda does not use this
-- value. Assign a meaningful description as you see fit.
ufcrqDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcrqDescription = lens _ufcrqDescription (\ s a -> s{_ufcrqDescription = a});

-- | The name of the Lambda function.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
ufcrqFunctionName :: Lens' UpdateFunctionConfiguration Text
ufcrqFunctionName = lens _ufcrqFunctionName (\ s a -> s{_ufcrqFunctionName = a});

instance AWSRequest UpdateFunctionConfiguration where
        type Sv UpdateFunctionConfiguration = Lambda
        type Rs UpdateFunctionConfiguration =
             FunctionConfiguration
        request = putJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateFunctionConfiguration where
        toHeaders = const mempty

instance ToJSON UpdateFunctionConfiguration where
        toJSON UpdateFunctionConfiguration'{..}
          = object
              ["MemorySize" .= _ufcrqMemorySize,
               "Role" .= _ufcrqRole, "Handler" .= _ufcrqHandler,
               "Timeout" .= _ufcrqTimeout,
               "Description" .= _ufcrqDescription]

instance ToPath UpdateFunctionConfiguration where
        toPath UpdateFunctionConfiguration'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _ufcrqFunctionName,
               "/versions/HEAD/configuration"]

instance ToQuery UpdateFunctionConfiguration where
        toQuery = const mempty
