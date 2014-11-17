{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.UploadFunction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Lambda function or updates an existing function. The function
-- metadata is created from the request parameters, and the code for the
-- function is provided by a .zip file in the request body. If the function
-- name already exists, the existing Lambda function is updated with the new
-- code and metadata. This operation requires permission for the
-- lambda:UploadFunction action.
--
-- <UploadFunction.html>
module Network.AWS.Lambda.UploadFunction
    (
    -- * Request
      UploadFunction
    -- ** Request constructor
    , uploadFunction
    -- ** Request lenses
    , ufDescription
    , ufFunctionName
    , ufFunctionZip
    , ufHandler
    , ufMemorySize
    , ufMode
    , ufRole
    , ufRuntime
    , ufTimeout

    -- * Response
    , UploadFunctionResponse
    -- ** Response constructor
    , uploadFunctionResponse
    -- ** Response lenses
    , ufrCodeSize
    , ufrConfigurationId
    , ufrDescription
    , ufrFunctionARN
    , ufrFunctionName
    , ufrHandler
    , ufrLastModified
    , ufrMemorySize
    , ufrMode
    , ufrRole
    , ufrRuntime
    , ufrTimeout
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data UploadFunction = UploadFunction
    { _ufDescription  :: Maybe Text
    , _ufFunctionName :: Text
    , _ufFunctionZip  :: Base64
    , _ufHandler      :: Text
    , _ufMemorySize   :: Maybe Nat
    , _ufMode         :: Text
    , _ufRole         :: Text
    , _ufRuntime      :: Text
    , _ufTimeout      :: Maybe Nat
    } deriving (Eq, Show, Generic)

-- | 'UploadFunction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufDescription' @::@ 'Maybe' 'Text'
--
-- * 'ufFunctionName' @::@ 'Text'
--
-- * 'ufFunctionZip' @::@ 'Base64'
--
-- * 'ufHandler' @::@ 'Text'
--
-- * 'ufMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufMode' @::@ 'Text'
--
-- * 'ufRole' @::@ 'Text'
--
-- * 'ufRuntime' @::@ 'Text'
--
-- * 'ufTimeout' @::@ 'Maybe' 'Natural'
--
uploadFunction :: Text -- ^ 'ufFunctionName'
               -> Base64 -- ^ 'ufFunctionZip'
               -> Text -- ^ 'ufRuntime'
               -> Text -- ^ 'ufRole'
               -> Text -- ^ 'ufHandler'
               -> Text -- ^ 'ufMode'
               -> UploadFunction
uploadFunction p1 p2 p3 p4 p5 p6 = UploadFunction
    { _ufFunctionName = p1
    , _ufFunctionZip  = p2
    , _ufRuntime      = p3
    , _ufRole         = p4
    , _ufHandler      = p5
    , _ufMode         = p6
    , _ufDescription  = Nothing
    , _ufTimeout      = Nothing
    , _ufMemorySize   = Nothing
    }

-- | A short, user-defined function description. Lambda does not use this
-- value. Assign a meaningful description as you see fit.
ufDescription :: Lens' UploadFunction (Maybe Text)
ufDescription = lens _ufDescription (\s a -> s { _ufDescription = a })

-- | The name you want to assign to the function you are uploading. The
-- function names appear in the console and are returned in the
-- ListFunctions API. Function names are used to specify functions to other
-- AWS Lambda APIs, such as InvokeAsync.
ufFunctionName :: Lens' UploadFunction Text
ufFunctionName = lens _ufFunctionName (\s a -> s { _ufFunctionName = a })

-- | A .zip file containing your packaged source code. For more information
-- about creating a .zip file, go to AWS LambdaL How it Works in the AWS
-- Lambda Developer Guide.
ufFunctionZip :: Lens' UploadFunction Base64
ufFunctionZip = lens _ufFunctionZip (\s a -> s { _ufFunctionZip = a })

-- | The function that Lambda calls to begin execution. For Node.js, it is the
-- module-name.export value in your function.
ufHandler :: Lens' UploadFunction Text
ufHandler = lens _ufHandler (\s a -> s { _ufHandler = a })

-- | The amount of memory, in MB, your Lambda function is given. Lambda uses
-- this memory size to infer the amount of CPU allocated to your function.
-- Your function use-case determines your CPU and memory requirements. For
-- example, database operation might need less memory compared to image
-- processing function. The default value is 128 MB. The value must be a
-- multiple of 64 MB.
ufMemorySize :: Lens' UploadFunction (Maybe Natural)
ufMemorySize = lens _ufMemorySize (\s a -> s { _ufMemorySize = a })
    . mapping _Nat

-- | How the Lambda function will be invoked. Lambda supports only the "event"
-- mode.
ufMode :: Lens' UploadFunction Text
ufMode = lens _ufMode (\s a -> s { _ufMode = a })

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
ufRole :: Lens' UploadFunction Text
ufRole = lens _ufRole (\s a -> s { _ufRole = a })

-- | The runtime environment for the Lambda function you are uploading.
-- Currently, Lambda supports only "nodejs" as the runtime.
ufRuntime :: Lens' UploadFunction Text
ufRuntime = lens _ufRuntime (\s a -> s { _ufRuntime = a })

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufTimeout :: Lens' UploadFunction (Maybe Natural)
ufTimeout = lens _ufTimeout (\s a -> s { _ufTimeout = a })
    . mapping _Nat

data UploadFunctionResponse = UploadFunctionResponse
    { _ufrCodeSize        :: Maybe Integer
    , _ufrConfigurationId :: Maybe Text
    , _ufrDescription     :: Maybe Text
    , _ufrFunctionARN     :: Maybe Text
    , _ufrFunctionName    :: Maybe Text
    , _ufrHandler         :: Maybe Text
    , _ufrLastModified    :: Maybe RFC822
    , _ufrMemorySize      :: Maybe Nat
    , _ufrMode            :: Maybe Text
    , _ufrRole            :: Maybe Text
    , _ufrRuntime         :: Maybe Text
    , _ufrTimeout         :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadFunctionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufrCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'ufrConfigurationId' @::@ 'Maybe' 'Text'
--
-- * 'ufrDescription' @::@ 'Maybe' 'Text'
--
-- * 'ufrFunctionARN' @::@ 'Maybe' 'Text'
--
-- * 'ufrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'ufrHandler' @::@ 'Maybe' 'Text'
--
-- * 'ufrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'ufrMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufrMode' @::@ 'Maybe' 'Text'
--
-- * 'ufrRole' @::@ 'Maybe' 'Text'
--
-- * 'ufrRuntime' @::@ 'Maybe' 'Text'
--
-- * 'ufrTimeout' @::@ 'Maybe' 'Natural'
--
uploadFunctionResponse :: UploadFunctionResponse
uploadFunctionResponse = UploadFunctionResponse
    { _ufrFunctionName    = Nothing
    , _ufrFunctionARN     = Nothing
    , _ufrConfigurationId = Nothing
    , _ufrRuntime         = Nothing
    , _ufrRole            = Nothing
    , _ufrHandler         = Nothing
    , _ufrMode            = Nothing
    , _ufrCodeSize        = Nothing
    , _ufrDescription     = Nothing
    , _ufrTimeout         = Nothing
    , _ufrMemorySize      = Nothing
    , _ufrLastModified    = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
ufrCodeSize :: Lens' UploadFunctionResponse (Maybe Integer)
ufrCodeSize = lens _ufrCodeSize (\s a -> s { _ufrCodeSize = a })

-- | A Lambda-assigned unique identifier for the current function code and
-- related configuration.
ufrConfigurationId :: Lens' UploadFunctionResponse (Maybe Text)
ufrConfigurationId =
    lens _ufrConfigurationId (\s a -> s { _ufrConfigurationId = a })

-- | The user-provided description.
ufrDescription :: Lens' UploadFunctionResponse (Maybe Text)
ufrDescription = lens _ufrDescription (\s a -> s { _ufrDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
ufrFunctionARN :: Lens' UploadFunctionResponse (Maybe Text)
ufrFunctionARN = lens _ufrFunctionARN (\s a -> s { _ufrFunctionARN = a })

-- | The name of the function.
ufrFunctionName :: Lens' UploadFunctionResponse (Maybe Text)
ufrFunctionName = lens _ufrFunctionName (\s a -> s { _ufrFunctionName = a })

-- | The function Lambda calls to begin executing your function.
ufrHandler :: Lens' UploadFunctionResponse (Maybe Text)
ufrHandler = lens _ufrHandler (\s a -> s { _ufrHandler = a })

-- | The timestamp of the last time you updated the function.
ufrLastModified :: Lens' UploadFunctionResponse (Maybe UTCTime)
ufrLastModified = lens _ufrLastModified (\s a -> s { _ufrLastModified = a })
    . mapping _Time

-- | The memory size, in MB, you configured for the function. Must be a
-- multiple of 64 MB.
ufrMemorySize :: Lens' UploadFunctionResponse (Maybe Natural)
ufrMemorySize = lens _ufrMemorySize (\s a -> s { _ufrMemorySize = a })
    . mapping _Nat

-- | The type of the Lambda function you uploaded.
ufrMode :: Lens' UploadFunctionResponse (Maybe Text)
ufrMode = lens _ufrMode (\s a -> s { _ufrMode = a })

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
ufrRole :: Lens' UploadFunctionResponse (Maybe Text)
ufrRole = lens _ufrRole (\s a -> s { _ufrRole = a })

-- | The runtime environment for the Lambda function.
ufrRuntime :: Lens' UploadFunctionResponse (Maybe Text)
ufrRuntime = lens _ufrRuntime (\s a -> s { _ufrRuntime = a })

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufrTimeout :: Lens' UploadFunctionResponse (Maybe Natural)
ufrTimeout = lens _ufrTimeout (\s a -> s { _ufrTimeout = a })
    . mapping _Nat

instance AWSRequest UploadFunction where
    type Sv UploadFunction = Lambda
    type Rs UploadFunction = UploadFunctionResponse

    request  = put
    response = jsonResponse

instance FromJSON UploadFunctionResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath UploadFunction where
    toPath UploadFunction{..} = mconcat
        [ "/2014-11-13/functions/"
        , toText _ufFunctionName
        ]

instance ToHeaders UploadFunction

instance ToQuery UploadFunction where
    toQuery UploadFunction{..} = mconcat
        [ "Runtime"     =? _ufRuntime
        , "Role"        =? _ufRole
        , "Handler"     =? _ufHandler
        , "Mode"        =? _ufMode
        , "Description" =? _ufDescription
        , "Timeout"     =? _ufTimeout
        , "MemorySize"  =? _ufMemorySize
        ]

instance ToJSON UploadFunction where
    toJSON = genericToJSON jsonOptions
