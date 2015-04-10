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

-- Module      : Network.AWS.Lambda.CreateFunction
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

-- | Creates a new Lambda function. The function metadata is created from the
-- request parameters, and the code for the function is provided by a .zip file
-- in the request body. If the function name already exists, the operation will
-- fail. Note that the function name is case-sensitive.
--
-- This operation requires permission for the 'lambda:CreateFunction' action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_CreateFunction.html>
module Network.AWS.Lambda.CreateFunction
    (
    -- * Request
      CreateFunction
    -- ** Request constructor
    , createFunction
    -- ** Request lenses
    , cfCode
    , cfDescription
    , cfFunctionName
    , cfHandler
    , cfMemorySize
    , cfRole
    , cfRuntime
    , cfTimeout

    -- * Response
    , CreateFunctionResponse
    -- ** Response constructor
    , createFunctionResponse
    -- ** Response lenses
    , cfrCodeSize
    , cfrDescription
    , cfrFunctionArn
    , cfrFunctionName
    , cfrHandler
    , cfrLastModified
    , cfrMemorySize
    , cfrRole
    , cfrRuntime
    , cfrTimeout
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data CreateFunction = CreateFunction
    { _cfCode         :: FunctionCode
    , _cfDescription  :: Maybe Text
    , _cfFunctionName :: Text
    , _cfHandler      :: Text
    , _cfMemorySize   :: Maybe Nat
    , _cfRole         :: Text
    , _cfRuntime      :: Runtime
    , _cfTimeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'CreateFunction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfCode' @::@ 'FunctionCode'
--
-- * 'cfDescription' @::@ 'Maybe' 'Text'
--
-- * 'cfFunctionName' @::@ 'Text'
--
-- * 'cfHandler' @::@ 'Text'
--
-- * 'cfMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'cfRole' @::@ 'Text'
--
-- * 'cfRuntime' @::@ 'Runtime'
--
-- * 'cfTimeout' @::@ 'Maybe' 'Natural'
--
createFunction :: Text -- ^ 'cfFunctionName'
               -> Runtime -- ^ 'cfRuntime'
               -> Text -- ^ 'cfRole'
               -> Text -- ^ 'cfHandler'
               -> FunctionCode -- ^ 'cfCode'
               -> CreateFunction
createFunction p1 p2 p3 p4 p5 = CreateFunction
    { _cfFunctionName = p1
    , _cfRuntime      = p2
    , _cfRole         = p3
    , _cfHandler      = p4
    , _cfCode         = p5
    , _cfDescription  = Nothing
    , _cfTimeout      = Nothing
    , _cfMemorySize   = Nothing
    }

-- | A structure that includes ZipFile.
cfCode :: Lens' CreateFunction FunctionCode
cfCode = lens _cfCode (\s a -> s { _cfCode = a })

-- | A short, user-defined function description. Lambda does not use this value.
-- Assign a meaningful description as you see fit.
cfDescription :: Lens' CreateFunction (Maybe Text)
cfDescription = lens _cfDescription (\s a -> s { _cfDescription = a })

-- | The name you want to assign to the function you are uploading. You can
-- specify an unqualified function name (for example, "Thumbnail") or you can
-- specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length. The function names appear in the console and are returned in the 'ListFunctions' API. Function names are used to specify functions to other AWS Lambda APIs,
-- such as 'Invoke'.
cfFunctionName :: Lens' CreateFunction Text
cfFunctionName = lens _cfFunctionName (\s a -> s { _cfFunctionName = a })

-- | The function within your code that Lambda calls to begin execution. For
-- Node.js, it is the /module-name/./export/ value in your function.
cfHandler :: Lens' CreateFunction Text
cfHandler = lens _cfHandler (\s a -> s { _cfHandler = a })

-- | The amount of memory, in MB, your Lambda function is given. Lambda uses this
-- memory size to infer the amount of CPU and memory allocated to your function.
-- Your function use-case determines your CPU and memory requirements. For
-- example, a database operation might need less memory compared to an image
-- processing function. The default value is 128 MB. The value must be a
-- multiple of 64 MB.
cfMemorySize :: Lens' CreateFunction (Maybe Natural)
cfMemorySize = lens _cfMemorySize (\s a -> s { _cfMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>
cfRole :: Lens' CreateFunction Text
cfRole = lens _cfRole (\s a -> s { _cfRole = a })

-- | The runtime environment for the Lambda function you are uploading. Currently,
-- Lambda supports only "nodejs" as the runtime.
cfRuntime :: Lens' CreateFunction Runtime
cfRuntime = lens _cfRuntime (\s a -> s { _cfRuntime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
cfTimeout :: Lens' CreateFunction (Maybe Natural)
cfTimeout = lens _cfTimeout (\s a -> s { _cfTimeout = a }) . mapping _Nat

data CreateFunctionResponse = CreateFunctionResponse
    { _cfrCodeSize     :: Maybe Integer
    , _cfrDescription  :: Maybe Text
    , _cfrFunctionArn  :: Maybe Text
    , _cfrFunctionName :: Maybe Text
    , _cfrHandler      :: Maybe Text
    , _cfrLastModified :: Maybe Text
    , _cfrMemorySize   :: Maybe Nat
    , _cfrRole         :: Maybe Text
    , _cfrRuntime      :: Maybe Runtime
    , _cfrTimeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'CreateFunctionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfrCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'cfrDescription' @::@ 'Maybe' 'Text'
--
-- * 'cfrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'cfrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'cfrHandler' @::@ 'Maybe' 'Text'
--
-- * 'cfrLastModified' @::@ 'Maybe' 'Text'
--
-- * 'cfrMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'cfrRole' @::@ 'Maybe' 'Text'
--
-- * 'cfrRuntime' @::@ 'Maybe' 'Runtime'
--
-- * 'cfrTimeout' @::@ 'Maybe' 'Natural'
--
createFunctionResponse :: CreateFunctionResponse
createFunctionResponse = CreateFunctionResponse
    { _cfrFunctionName = Nothing
    , _cfrFunctionArn  = Nothing
    , _cfrRuntime      = Nothing
    , _cfrRole         = Nothing
    , _cfrHandler      = Nothing
    , _cfrCodeSize     = Nothing
    , _cfrDescription  = Nothing
    , _cfrTimeout      = Nothing
    , _cfrMemorySize   = Nothing
    , _cfrLastModified = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
cfrCodeSize :: Lens' CreateFunctionResponse (Maybe Integer)
cfrCodeSize = lens _cfrCodeSize (\s a -> s { _cfrCodeSize = a })

-- | The user-provided description.
cfrDescription :: Lens' CreateFunctionResponse (Maybe Text)
cfrDescription = lens _cfrDescription (\s a -> s { _cfrDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
cfrFunctionArn :: Lens' CreateFunctionResponse (Maybe Text)
cfrFunctionArn = lens _cfrFunctionArn (\s a -> s { _cfrFunctionArn = a })

-- | The name of the function.
cfrFunctionName :: Lens' CreateFunctionResponse (Maybe Text)
cfrFunctionName = lens _cfrFunctionName (\s a -> s { _cfrFunctionName = a })

-- | The function Lambda calls to begin executing your function.
cfrHandler :: Lens' CreateFunctionResponse (Maybe Text)
cfrHandler = lens _cfrHandler (\s a -> s { _cfrHandler = a })

-- | The timestamp of the last time you updated the function.
cfrLastModified :: Lens' CreateFunctionResponse (Maybe Text)
cfrLastModified = lens _cfrLastModified (\s a -> s { _cfrLastModified = a })

-- | The memory size, in MB, you configured for the function. Must be a multiple
-- of 64 MB.
cfrMemorySize :: Lens' CreateFunctionResponse (Maybe Natural)
cfrMemorySize = lens _cfrMemorySize (\s a -> s { _cfrMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources.
cfrRole :: Lens' CreateFunctionResponse (Maybe Text)
cfrRole = lens _cfrRole (\s a -> s { _cfrRole = a })

-- | The runtime environment for the Lambda function.
cfrRuntime :: Lens' CreateFunctionResponse (Maybe Runtime)
cfrRuntime = lens _cfrRuntime (\s a -> s { _cfrRuntime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
cfrTimeout :: Lens' CreateFunctionResponse (Maybe Natural)
cfrTimeout = lens _cfrTimeout (\s a -> s { _cfrTimeout = a }) . mapping _Nat

instance ToPath CreateFunction where
    toPath = const "/2015-03-31/functions"

instance ToQuery CreateFunction where
    toQuery = const mempty

instance ToHeaders CreateFunction

instance ToJSON CreateFunction where
    toJSON CreateFunction{..} = object
        [ "FunctionName" .= _cfFunctionName
        , "Runtime"      .= _cfRuntime
        , "Role"         .= _cfRole
        , "Handler"      .= _cfHandler
        , "Description"  .= _cfDescription
        , "Timeout"      .= _cfTimeout
        , "MemorySize"   .= _cfMemorySize
        , "Code"         .= _cfCode
        ]

instance AWSRequest CreateFunction where
    type Sv CreateFunction = Lambda
    type Rs CreateFunction = CreateFunctionResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateFunctionResponse where
    parseJSON = withObject "CreateFunctionResponse" $ \o -> CreateFunctionResponse
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
