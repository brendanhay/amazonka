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

-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
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

-- | Returns the configuration information of the Lambda function. This the same
-- information you provided as parameters when uploading the function by using 'CreateFunction'.
--
-- This operation requires permission for the 'lambda:GetFunctionConfiguration'
-- operation.
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
    , GetFunctionConfigurationResponse
    -- ** Response constructor
    , getFunctionConfigurationResponse
    -- ** Response lenses
    , gfcrCodeSize
    , gfcrDescription
    , gfcrFunctionArn
    , gfcrFunctionName
    , gfcrHandler
    , gfcrLastModified
    , gfcrMemorySize
    , gfcrRole
    , gfcrRuntime
    , gfcrTimeout
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype GetFunctionConfiguration = GetFunctionConfiguration
    { _gfcFunctionName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetFunctionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfcFunctionName' @::@ 'Text'
--
getFunctionConfiguration :: Text -- ^ 'gfcFunctionName'
                         -> GetFunctionConfiguration
getFunctionConfiguration p1 = GetFunctionConfiguration
    { _gfcFunctionName = p1
    }

-- | The name of the Lambda function for which you want to retrieve the
-- configuration information.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
gfcFunctionName :: Lens' GetFunctionConfiguration Text
gfcFunctionName = lens _gfcFunctionName (\s a -> s { _gfcFunctionName = a })

data GetFunctionConfigurationResponse = GetFunctionConfigurationResponse
    { _gfcrCodeSize     :: Maybe Integer
    , _gfcrDescription  :: Maybe Text
    , _gfcrFunctionArn  :: Maybe Text
    , _gfcrFunctionName :: Maybe Text
    , _gfcrHandler      :: Maybe Text
    , _gfcrLastModified :: Maybe Text
    , _gfcrMemorySize   :: Maybe Nat
    , _gfcrRole         :: Maybe Text
    , _gfcrRuntime      :: Maybe Runtime
    , _gfcrTimeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'GetFunctionConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfcrCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'gfcrDescription' @::@ 'Maybe' 'Text'
--
-- * 'gfcrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'gfcrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'gfcrHandler' @::@ 'Maybe' 'Text'
--
-- * 'gfcrLastModified' @::@ 'Maybe' 'Text'
--
-- * 'gfcrMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'gfcrRole' @::@ 'Maybe' 'Text'
--
-- * 'gfcrRuntime' @::@ 'Maybe' 'Runtime'
--
-- * 'gfcrTimeout' @::@ 'Maybe' 'Natural'
--
getFunctionConfigurationResponse :: GetFunctionConfigurationResponse
getFunctionConfigurationResponse = GetFunctionConfigurationResponse
    { _gfcrFunctionName = Nothing
    , _gfcrFunctionArn  = Nothing
    , _gfcrRuntime      = Nothing
    , _gfcrRole         = Nothing
    , _gfcrHandler      = Nothing
    , _gfcrCodeSize     = Nothing
    , _gfcrDescription  = Nothing
    , _gfcrTimeout      = Nothing
    , _gfcrMemorySize   = Nothing
    , _gfcrLastModified = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
gfcrCodeSize :: Lens' GetFunctionConfigurationResponse (Maybe Integer)
gfcrCodeSize = lens _gfcrCodeSize (\s a -> s { _gfcrCodeSize = a })

-- | The user-provided description.
gfcrDescription :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrDescription = lens _gfcrDescription (\s a -> s { _gfcrDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
gfcrFunctionArn :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrFunctionArn = lens _gfcrFunctionArn (\s a -> s { _gfcrFunctionArn = a })

-- | The name of the function.
gfcrFunctionName :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrFunctionName = lens _gfcrFunctionName (\s a -> s { _gfcrFunctionName = a })

-- | The function Lambda calls to begin executing your function.
gfcrHandler :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrHandler = lens _gfcrHandler (\s a -> s { _gfcrHandler = a })

-- | The timestamp of the last time you updated the function.
gfcrLastModified :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrLastModified = lens _gfcrLastModified (\s a -> s { _gfcrLastModified = a })

-- | The memory size, in MB, you configured for the function. Must be a multiple
-- of 64 MB.
gfcrMemorySize :: Lens' GetFunctionConfigurationResponse (Maybe Natural)
gfcrMemorySize = lens _gfcrMemorySize (\s a -> s { _gfcrMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources.
gfcrRole :: Lens' GetFunctionConfigurationResponse (Maybe Text)
gfcrRole = lens _gfcrRole (\s a -> s { _gfcrRole = a })

-- | The runtime environment for the Lambda function.
gfcrRuntime :: Lens' GetFunctionConfigurationResponse (Maybe Runtime)
gfcrRuntime = lens _gfcrRuntime (\s a -> s { _gfcrRuntime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
gfcrTimeout :: Lens' GetFunctionConfigurationResponse (Maybe Natural)
gfcrTimeout = lens _gfcrTimeout (\s a -> s { _gfcrTimeout = a }) . mapping _Nat

instance ToPath GetFunctionConfiguration where
    toPath GetFunctionConfiguration{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _gfcFunctionName
        , "/versions/HEAD/configuration"
        ]

instance ToQuery GetFunctionConfiguration where
    toQuery = const mempty

instance ToHeaders GetFunctionConfiguration

instance ToJSON GetFunctionConfiguration where
    toJSON = const (toJSON Empty)

instance AWSRequest GetFunctionConfiguration where
    type Sv GetFunctionConfiguration = Lambda
    type Rs GetFunctionConfiguration = GetFunctionConfigurationResponse

    request  = get
    response = jsonResponse

instance FromJSON GetFunctionConfigurationResponse where
    parseJSON = withObject "GetFunctionConfigurationResponse" $ \o -> GetFunctionConfigurationResponse
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
