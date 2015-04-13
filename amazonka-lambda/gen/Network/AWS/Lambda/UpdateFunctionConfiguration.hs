{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the configuration parameters for the specified Lambda function by
-- using the values provided in the request. You provide only the parameters you
-- want to change. This operation must only be used on an existing Lambda
-- function and cannot be used to update the function's code.
--
-- This operation requires permission for the 'lambda:UpdateFunctionConfiguration'
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionConfiguration.html>
module Network.AWS.Lambda.UpdateFunctionConfiguration
    (
    -- * Request
      UpdateFunctionConfiguration
    -- ** Request constructor
    , updateFunctionConfiguration
    -- ** Request lenses
    , ufcDescription
    , ufcFunctionName
    , ufcHandler
    , ufcMemorySize
    , ufcRole
    , ufcTimeout

    -- * Response
    , UpdateFunctionConfigurationResponse
    -- ** Response constructor
    , updateFunctionConfigurationResponse
    -- ** Response lenses
    , ufcr1CodeSize
    , ufcr1Description
    , ufcr1FunctionArn
    , ufcr1FunctionName
    , ufcr1Handler
    , ufcr1LastModified
    , ufcr1MemorySize
    , ufcr1Role
    , ufcr1Runtime
    , ufcr1Timeout
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data UpdateFunctionConfiguration = UpdateFunctionConfiguration
    { _ufcDescription  :: Maybe Text
    , _ufcFunctionName :: Text
    , _ufcHandler      :: Maybe Text
    , _ufcMemorySize   :: Maybe Nat
    , _ufcRole         :: Maybe Text
    , _ufcTimeout      :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateFunctionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufcDescription' @::@ 'Maybe' 'Text'
--
-- * 'ufcFunctionName' @::@ 'Text'
--
-- * 'ufcHandler' @::@ 'Maybe' 'Text'
--
-- * 'ufcMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufcRole' @::@ 'Maybe' 'Text'
--
-- * 'ufcTimeout' @::@ 'Maybe' 'Natural'
--
updateFunctionConfiguration :: Text -- ^ 'ufcFunctionName'
                            -> UpdateFunctionConfiguration
updateFunctionConfiguration p1 = UpdateFunctionConfiguration
    { _ufcFunctionName = p1
    , _ufcRole         = Nothing
    , _ufcHandler      = Nothing
    , _ufcDescription  = Nothing
    , _ufcTimeout      = Nothing
    , _ufcMemorySize   = Nothing
    }

-- | A short user-defined function description. AWS Lambda does not use this
-- value. Assign a meaningful description as you see fit.
ufcDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcDescription = lens _ufcDescription (\s a -> s { _ufcDescription = a })

-- | The name of the Lambda function.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
ufcFunctionName :: Lens' UpdateFunctionConfiguration Text
ufcFunctionName = lens _ufcFunctionName (\s a -> s { _ufcFunctionName = a })

-- | The function that Lambda calls to begin executing your function. For Node.js,
-- it is the /module-name.export/ value in your function.
ufcHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcHandler = lens _ufcHandler (\s a -> s { _ufcHandler = a })

-- | The amount of memory, in MB, your Lambda function is given. AWS Lambda uses
-- this memory size to infer the amount of CPU allocated to your function. Your
-- function use-case determines your CPU and memory requirements. For example, a
-- database operation might need less memory compared to an image processing
-- function. The default value is 128 MB. The value must be a multiple of 64 MB.
ufcMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcMemorySize = lens _ufcMemorySize (\s a -> s { _ufcMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda will assume when
-- it executes your function.
ufcRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRole = lens _ufcRole (\s a -> s { _ufcRole = a })

-- | The function execution time at which AWS Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend you
-- set this value based on your expected execution time. The default is 3
-- seconds.
ufcTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcTimeout = lens _ufcTimeout (\s a -> s { _ufcTimeout = a }) . mapping _Nat

data UpdateFunctionConfigurationResponse = UpdateFunctionConfigurationResponse
    { _ufcr1CodeSize     :: Maybe Integer
    , _ufcr1Description  :: Maybe Text
    , _ufcr1FunctionArn  :: Maybe Text
    , _ufcr1FunctionName :: Maybe Text
    , _ufcr1Handler      :: Maybe Text
    , _ufcr1LastModified :: Maybe Text
    , _ufcr1MemorySize   :: Maybe Nat
    , _ufcr1Role         :: Maybe Text
    , _ufcr1Runtime      :: Maybe Runtime
    , _ufcr1Timeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'UpdateFunctionConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufcr1CodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'ufcr1Description' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1FunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1FunctionName' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1Handler' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1LastModified' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1MemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufcr1Role' @::@ 'Maybe' 'Text'
--
-- * 'ufcr1Runtime' @::@ 'Maybe' 'Runtime'
--
-- * 'ufcr1Timeout' @::@ 'Maybe' 'Natural'
--
updateFunctionConfigurationResponse :: UpdateFunctionConfigurationResponse
updateFunctionConfigurationResponse = UpdateFunctionConfigurationResponse
    { _ufcr1FunctionName = Nothing
    , _ufcr1FunctionArn  = Nothing
    , _ufcr1Runtime      = Nothing
    , _ufcr1Role         = Nothing
    , _ufcr1Handler      = Nothing
    , _ufcr1CodeSize     = Nothing
    , _ufcr1Description  = Nothing
    , _ufcr1Timeout      = Nothing
    , _ufcr1MemorySize   = Nothing
    , _ufcr1LastModified = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
ufcr1CodeSize :: Lens' UpdateFunctionConfigurationResponse (Maybe Integer)
ufcr1CodeSize = lens _ufcr1CodeSize (\s a -> s { _ufcr1CodeSize = a })

-- | The user-provided description.
ufcr1Description :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1Description = lens _ufcr1Description (\s a -> s { _ufcr1Description = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
ufcr1FunctionArn :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1FunctionArn = lens _ufcr1FunctionArn (\s a -> s { _ufcr1FunctionArn = a })

-- | The name of the function.
ufcr1FunctionName :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1FunctionName =
    lens _ufcr1FunctionName (\s a -> s { _ufcr1FunctionName = a })

-- | The function Lambda calls to begin executing your function.
ufcr1Handler :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1Handler = lens _ufcr1Handler (\s a -> s { _ufcr1Handler = a })

-- | The timestamp of the last time you updated the function.
ufcr1LastModified :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1LastModified =
    lens _ufcr1LastModified (\s a -> s { _ufcr1LastModified = a })

-- | The memory size, in MB, you configured for the function. Must be a multiple
-- of 64 MB.
ufcr1MemorySize :: Lens' UpdateFunctionConfigurationResponse (Maybe Natural)
ufcr1MemorySize = lens _ufcr1MemorySize (\s a -> s { _ufcr1MemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources.
ufcr1Role :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcr1Role = lens _ufcr1Role (\s a -> s { _ufcr1Role = a })

-- | The runtime environment for the Lambda function.
ufcr1Runtime :: Lens' UpdateFunctionConfigurationResponse (Maybe Runtime)
ufcr1Runtime = lens _ufcr1Runtime (\s a -> s { _ufcr1Runtime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
ufcr1Timeout :: Lens' UpdateFunctionConfigurationResponse (Maybe Natural)
ufcr1Timeout = lens _ufcr1Timeout (\s a -> s { _ufcr1Timeout = a }) . mapping _Nat

instance ToPath UpdateFunctionConfiguration where
    toPath UpdateFunctionConfiguration{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _ufcFunctionName
        , "/versions/HEAD/configuration"
        ]

instance ToQuery UpdateFunctionConfiguration where
    toQuery = const mempty

instance ToHeaders UpdateFunctionConfiguration

instance ToJSON UpdateFunctionConfiguration where
    toJSON UpdateFunctionConfiguration{..} = object
        [ "Role"        .= _ufcRole
        , "Handler"     .= _ufcHandler
        , "Description" .= _ufcDescription
        , "Timeout"     .= _ufcTimeout
        , "MemorySize"  .= _ufcMemorySize
        ]

instance AWSRequest UpdateFunctionConfiguration where
    type Sv UpdateFunctionConfiguration = Lambda
    type Rs UpdateFunctionConfiguration = UpdateFunctionConfigurationResponse

    request  = put
    response = jsonResponse

instance FromJSON UpdateFunctionConfigurationResponse where
    parseJSON = withObject "UpdateFunctionConfigurationResponse" $ \o -> UpdateFunctionConfigurationResponse
        <$> o .:? "CodeSize"
        <*> o .:? "Description"
        <*> o .:? "FunctionArn"
        <*> o .:? "FunctionName"
        <*> o .:? "Handler"
        <*> o .:? "LastModified"
        <*> o .:? "MemorySize"
        <*> o .:? "Role"
        <*> o .:? "Runtime"
        <*> o .:? "Timeout"
