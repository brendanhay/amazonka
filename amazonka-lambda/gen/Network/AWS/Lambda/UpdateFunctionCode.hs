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

-- Module      : Network.AWS.Lambda.UpdateFunctionCode
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

-- | Updates the code for the specified Lambda function. This operation must only
-- be used on an existing Lambda function and cannot be used to update the
-- function configuration.
--
-- This operation requires permision for the 'lambda:UpdateFunctionCode' action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionCode.html>
module Network.AWS.Lambda.UpdateFunctionCode
    (
    -- * Request
      UpdateFunctionCode
    -- ** Request constructor
    , updateFunctionCode
    -- ** Request lenses
    , ufc1FunctionName
    , ufc1ZipFile

    -- * Response
    , UpdateFunctionCodeResponse
    -- ** Response constructor
    , updateFunctionCodeResponse
    -- ** Response lenses
    , ufcrCodeSize
    , ufcrDescription
    , ufcrFunctionArn
    , ufcrFunctionName
    , ufcrHandler
    , ufcrLastModified
    , ufcrMemorySize
    , ufcrRole
    , ufcrRuntime
    , ufcrTimeout
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data UpdateFunctionCode = UpdateFunctionCode
    { _ufc1FunctionName :: Text
    , _ufc1ZipFile      :: Base64
    } deriving (Eq, Read, Show)

-- | 'UpdateFunctionCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufc1FunctionName' @::@ 'Text'
--
-- * 'ufc1ZipFile' @::@ 'Base64'
--
updateFunctionCode :: Text -- ^ 'ufc1FunctionName'
                   -> Base64 -- ^ 'ufc1ZipFile'
                   -> UpdateFunctionCode
updateFunctionCode p1 p2 = UpdateFunctionCode
    { _ufc1FunctionName = p1
    , _ufc1ZipFile      = p2
    }

-- | The existing Lambda function name whose code you want to replace.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
ufc1FunctionName :: Lens' UpdateFunctionCode Text
ufc1FunctionName = lens _ufc1FunctionName (\s a -> s { _ufc1FunctionName = a })

-- | Based64-encoded .zip file containing your packaged source code.
ufc1ZipFile :: Lens' UpdateFunctionCode Base64
ufc1ZipFile = lens _ufc1ZipFile (\s a -> s { _ufc1ZipFile = a })

data UpdateFunctionCodeResponse = UpdateFunctionCodeResponse
    { _ufcrCodeSize     :: Maybe Integer
    , _ufcrDescription  :: Maybe Text
    , _ufcrFunctionArn  :: Maybe Text
    , _ufcrFunctionName :: Maybe Text
    , _ufcrHandler      :: Maybe Text
    , _ufcrLastModified :: Maybe Text
    , _ufcrMemorySize   :: Maybe Nat
    , _ufcrRole         :: Maybe Text
    , _ufcrRuntime      :: Maybe Runtime
    , _ufcrTimeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'UpdateFunctionCodeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufcrCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'ufcrDescription' @::@ 'Maybe' 'Text'
--
-- * 'ufcrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'ufcrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'ufcrHandler' @::@ 'Maybe' 'Text'
--
-- * 'ufcrLastModified' @::@ 'Maybe' 'Text'
--
-- * 'ufcrMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufcrRole' @::@ 'Maybe' 'Text'
--
-- * 'ufcrRuntime' @::@ 'Maybe' 'Runtime'
--
-- * 'ufcrTimeout' @::@ 'Maybe' 'Natural'
--
updateFunctionCodeResponse :: UpdateFunctionCodeResponse
updateFunctionCodeResponse = UpdateFunctionCodeResponse
    { _ufcrFunctionName = Nothing
    , _ufcrFunctionArn  = Nothing
    , _ufcrRuntime      = Nothing
    , _ufcrRole         = Nothing
    , _ufcrHandler      = Nothing
    , _ufcrCodeSize     = Nothing
    , _ufcrDescription  = Nothing
    , _ufcrTimeout      = Nothing
    , _ufcrMemorySize   = Nothing
    , _ufcrLastModified = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
ufcrCodeSize :: Lens' UpdateFunctionCodeResponse (Maybe Integer)
ufcrCodeSize = lens _ufcrCodeSize (\s a -> s { _ufcrCodeSize = a })

-- | The user-provided description.
ufcrDescription :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrDescription = lens _ufcrDescription (\s a -> s { _ufcrDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
ufcrFunctionArn :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrFunctionArn = lens _ufcrFunctionArn (\s a -> s { _ufcrFunctionArn = a })

-- | The name of the function.
ufcrFunctionName :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrFunctionName = lens _ufcrFunctionName (\s a -> s { _ufcrFunctionName = a })

-- | The function Lambda calls to begin executing your function.
ufcrHandler :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrHandler = lens _ufcrHandler (\s a -> s { _ufcrHandler = a })

-- | The timestamp of the last time you updated the function.
ufcrLastModified :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrLastModified = lens _ufcrLastModified (\s a -> s { _ufcrLastModified = a })

-- | The memory size, in MB, you configured for the function. Must be a multiple
-- of 64 MB.
ufcrMemorySize :: Lens' UpdateFunctionCodeResponse (Maybe Natural)
ufcrMemorySize = lens _ufcrMemorySize (\s a -> s { _ufcrMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources.
ufcrRole :: Lens' UpdateFunctionCodeResponse (Maybe Text)
ufcrRole = lens _ufcrRole (\s a -> s { _ufcrRole = a })

-- | The runtime environment for the Lambda function.
ufcrRuntime :: Lens' UpdateFunctionCodeResponse (Maybe Runtime)
ufcrRuntime = lens _ufcrRuntime (\s a -> s { _ufcrRuntime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
ufcrTimeout :: Lens' UpdateFunctionCodeResponse (Maybe Natural)
ufcrTimeout = lens _ufcrTimeout (\s a -> s { _ufcrTimeout = a }) . mapping _Nat

instance ToPath UpdateFunctionCode where
    toPath UpdateFunctionCode{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _ufc1FunctionName
        , "/versions/HEAD/code"
        ]

instance ToQuery UpdateFunctionCode where
    toQuery = const mempty

instance ToHeaders UpdateFunctionCode

instance ToJSON UpdateFunctionCode where
    toJSON UpdateFunctionCode{..} = object
        [ "ZipFile" .= _ufc1ZipFile
        ]

instance AWSRequest UpdateFunctionCode where
    type Sv UpdateFunctionCode = Lambda
    type Rs UpdateFunctionCode = UpdateFunctionCodeResponse

    request  = put
    response = jsonResponse

instance FromJSON UpdateFunctionCodeResponse where
    parseJSON = withObject "UpdateFunctionCodeResponse" $ \o -> UpdateFunctionCodeResponse
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
