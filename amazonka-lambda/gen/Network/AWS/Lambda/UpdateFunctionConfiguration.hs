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
-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration parameters for the specified Lambda function
-- by using the values provided in the request. You provide only the
-- parameters you want to change. This operation must only be used on an
-- existing Lambda function and cannot be used to update the function\'s
-- code.
--
-- This operation requires permission for the
-- 'lambda:UpdateFunctionConfiguration' action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionConfiguration.html AWS API Reference> for UpdateFunctionConfiguration.
module Network.AWS.Lambda.UpdateFunctionConfiguration
    (
    -- * Creating a Request
      updateFunctionConfiguration
    , UpdateFunctionConfiguration
    -- * Request Lenses
    , ufcMemorySize
    , ufcRole
    , ufcHandler
    , ufcTimeout
    , ufcDescription
    , ufcFunctionName

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcMemorySize
    , fcRuntime
    , fcFunctionARN
    , fcRole
    , fcVersion
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcDescription
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
    { _ufcMemorySize   :: !(Maybe Nat)
    , _ufcRole         :: !(Maybe Text)
    , _ufcHandler      :: !(Maybe Text)
    , _ufcTimeout      :: !(Maybe Nat)
    , _ufcDescription  :: !(Maybe Text)
    , _ufcFunctionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufcMemorySize'
--
-- * 'ufcRole'
--
-- * 'ufcHandler'
--
-- * 'ufcTimeout'
--
-- * 'ufcDescription'
--
-- * 'ufcFunctionName'
updateFunctionConfiguration
    :: Text -- ^ 'ufcFunctionName'
    -> UpdateFunctionConfiguration
updateFunctionConfiguration pFunctionName_ =
    UpdateFunctionConfiguration'
    { _ufcMemorySize = Nothing
    , _ufcRole = Nothing
    , _ufcHandler = Nothing
    , _ufcTimeout = Nothing
    , _ufcDescription = Nothing
    , _ufcFunctionName = pFunctionName_
    }

-- | The amount of memory, in MB, your Lambda function is given. AWS Lambda
-- uses this memory size to infer the amount of CPU allocated to your
-- function. Your function use-case determines your CPU and memory
-- requirements. For example, a database operation might need less memory
-- compared to an image processing function. The default value is 128 MB.
-- The value must be a multiple of 64 MB.
ufcMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcMemorySize = lens _ufcMemorySize (\ s a -> s{_ufcMemorySize = a}) . mapping _Nat;

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda will assume
-- when it executes your function.
ufcRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRole = lens _ufcRole (\ s a -> s{_ufcRole = a});

-- | The function that Lambda calls to begin executing your function. For
-- Node.js, it is the /module-name.export/ value in your function.
ufcHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcHandler = lens _ufcHandler (\ s a -> s{_ufcHandler = a});

-- | The function execution time at which AWS Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufcTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcTimeout = lens _ufcTimeout (\ s a -> s{_ufcTimeout = a}) . mapping _Nat;

-- | A short user-defined function description. AWS Lambda does not use this
-- value. Assign a meaningful description as you see fit.
ufcDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcDescription = lens _ufcDescription (\ s a -> s{_ufcDescription = a});

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
ufcFunctionName :: Lens' UpdateFunctionConfiguration Text
ufcFunctionName = lens _ufcFunctionName (\ s a -> s{_ufcFunctionName = a});

instance AWSRequest UpdateFunctionConfiguration where
        type Rs UpdateFunctionConfiguration =
             FunctionConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateFunctionConfiguration where
        toHeaders = const mempty

instance ToJSON UpdateFunctionConfiguration where
        toJSON UpdateFunctionConfiguration'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _ufcMemorySize,
                  ("Role" .=) <$> _ufcRole,
                  ("Handler" .=) <$> _ufcHandler,
                  ("Timeout" .=) <$> _ufcTimeout,
                  ("Description" .=) <$> _ufcDescription])

instance ToPath UpdateFunctionConfiguration where
        toPath UpdateFunctionConfiguration'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _ufcFunctionName,
               "/configuration"]

instance ToQuery UpdateFunctionConfiguration where
        toQuery = const mempty
