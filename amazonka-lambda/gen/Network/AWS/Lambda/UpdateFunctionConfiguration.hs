{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Updates the configuration parameters for the specified Lambda function by
-- using the values provided in the request. You provide only the parameters
-- you want to change. This operation must only be used on an existing Lambda
-- function and cannot be used to update the function's code. This operation
-- requires permission for the lambda:UpdateFunctionConfiguration action.
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
    , ufcrCodeSize
    , ufcrConfigurationId
    , ufcrDescription
    , ufcrFunctionARN
    , ufcrFunctionName
    , ufcrHandler
    , ufcrLastModified
    , ufcrMemorySize
    , ufcrMode
    , ufcrRole
    , ufcrRuntime
    , ufcrTimeout
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data UpdateFunctionConfiguration = UpdateFunctionConfiguration
    { _ufcDescription  :: Maybe Text
    , _ufcFunctionName :: Text
    , _ufcHandler      :: Maybe Text
    , _ufcMemorySize   :: Maybe Nat
    , _ufcRole         :: Maybe Text
    , _ufcTimeout      :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

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

-- | A short user-defined function description. Lambda does not use this
-- value. Assign a meaningful description as you see fit.
ufcDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcDescription = lens _ufcDescription (\s a -> s { _ufcDescription = a })

-- | The name of the Lambda function.
ufcFunctionName :: Lens' UpdateFunctionConfiguration Text
ufcFunctionName = lens _ufcFunctionName (\s a -> s { _ufcFunctionName = a })

-- | The function that Lambda calls to begin executing your function. For
-- Node.js, it is the module-name.export value in your function.
ufcHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcHandler = lens _ufcHandler (\s a -> s { _ufcHandler = a })

-- | The amount of memory, in MB, your Lambda function is given. Lambda uses
-- this memory size to infer the amount of CPU allocated to your function.
-- Your function use-case determines your CPU and memory requirements. For
-- example, a database operation might need less memory compared to an image
-- processing function. The default value is 128 MB. The value must be a
-- multiple of 64 MB.
ufcMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcMemorySize = lens _ufcMemorySize (\s a -> s { _ufcMemorySize = a })
    . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda will assume
-- when it executes your function.
ufcRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRole = lens _ufcRole (\s a -> s { _ufcRole = a })

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufcTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcTimeout = lens _ufcTimeout (\s a -> s { _ufcTimeout = a })
    . mapping _Nat

data UpdateFunctionConfigurationResponse = UpdateFunctionConfigurationResponse
    { _ufcrCodeSize        :: Maybe Integer
    , _ufcrConfigurationId :: Maybe Text
    , _ufcrDescription     :: Maybe Text
    , _ufcrFunctionARN     :: Maybe Text
    , _ufcrFunctionName    :: Maybe Text
    , _ufcrHandler         :: Maybe Text
    , _ufcrLastModified    :: Maybe RFC822
    , _ufcrMemorySize      :: Maybe Nat
    , _ufcrMode            :: Maybe Text
    , _ufcrRole            :: Maybe Text
    , _ufcrRuntime         :: Maybe Text
    , _ufcrTimeout         :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateFunctionConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufcrCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'ufcrConfigurationId' @::@ 'Maybe' 'Text'
--
-- * 'ufcrDescription' @::@ 'Maybe' 'Text'
--
-- * 'ufcrFunctionARN' @::@ 'Maybe' 'Text'
--
-- * 'ufcrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'ufcrHandler' @::@ 'Maybe' 'Text'
--
-- * 'ufcrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'ufcrMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'ufcrMode' @::@ 'Maybe' 'Text'
--
-- * 'ufcrRole' @::@ 'Maybe' 'Text'
--
-- * 'ufcrRuntime' @::@ 'Maybe' 'Text'
--
-- * 'ufcrTimeout' @::@ 'Maybe' 'Natural'
--
updateFunctionConfigurationResponse :: UpdateFunctionConfigurationResponse
updateFunctionConfigurationResponse = UpdateFunctionConfigurationResponse
    { _ufcrFunctionName    = Nothing
    , _ufcrFunctionARN     = Nothing
    , _ufcrConfigurationId = Nothing
    , _ufcrRuntime         = Nothing
    , _ufcrRole            = Nothing
    , _ufcrHandler         = Nothing
    , _ufcrMode            = Nothing
    , _ufcrCodeSize        = Nothing
    , _ufcrDescription     = Nothing
    , _ufcrTimeout         = Nothing
    , _ufcrMemorySize      = Nothing
    , _ufcrLastModified    = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
ufcrCodeSize :: Lens' UpdateFunctionConfigurationResponse (Maybe Integer)
ufcrCodeSize = lens _ufcrCodeSize (\s a -> s { _ufcrCodeSize = a })

-- | A Lambda-assigned unique identifier for the current function code and
-- related configuration.
ufcrConfigurationId :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrConfigurationId =
    lens _ufcrConfigurationId (\s a -> s { _ufcrConfigurationId = a })

-- | The user-provided description.
ufcrDescription :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrDescription = lens _ufcrDescription (\s a -> s { _ufcrDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
ufcrFunctionARN :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrFunctionARN = lens _ufcrFunctionARN (\s a -> s { _ufcrFunctionARN = a })

-- | The name of the function.
ufcrFunctionName :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrFunctionName = lens _ufcrFunctionName (\s a -> s { _ufcrFunctionName = a })

-- | The function Lambda calls to begin executing your function.
ufcrHandler :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrHandler = lens _ufcrHandler (\s a -> s { _ufcrHandler = a })

-- | The timestamp of the last time you updated the function.
ufcrLastModified :: Lens' UpdateFunctionConfigurationResponse (Maybe UTCTime)
ufcrLastModified = lens _ufcrLastModified (\s a -> s { _ufcrLastModified = a })
    . mapping _Time

-- | The memory size, in MB, you configured for the function. Must be a
-- multiple of 64 MB.
ufcrMemorySize :: Lens' UpdateFunctionConfigurationResponse (Maybe Natural)
ufcrMemorySize = lens _ufcrMemorySize (\s a -> s { _ufcrMemorySize = a })
    . mapping _Nat

-- | The type of the Lambda function you uploaded.
ufcrMode :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrMode = lens _ufcrMode (\s a -> s { _ufcrMode = a })

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
ufcrRole :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrRole = lens _ufcrRole (\s a -> s { _ufcrRole = a })

-- | The runtime environment for the Lambda function.
ufcrRuntime :: Lens' UpdateFunctionConfigurationResponse (Maybe Text)
ufcrRuntime = lens _ufcrRuntime (\s a -> s { _ufcrRuntime = a })

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
ufcrTimeout :: Lens' UpdateFunctionConfigurationResponse (Maybe Natural)
ufcrTimeout = lens _ufcrTimeout (\s a -> s { _ufcrTimeout = a })
    . mapping _Nat

instance AWSRequest UpdateFunctionConfiguration where
    type Sv UpdateFunctionConfiguration = Lambda
    type Rs UpdateFunctionConfiguration = UpdateFunctionConfigurationResponse

    request  = put
    response = jsonResponse

instance FromJSON UpdateFunctionConfigurationResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath UpdateFunctionConfiguration where
    toPath UpdateFunctionConfiguration{..} = mconcat
        [ "/2014-11-13/functions/"
        , toText _ufcFunctionName
        , "/configuration"
        ]

instance ToHeaders UpdateFunctionConfiguration

instance ToQuery UpdateFunctionConfiguration where
    toQuery UpdateFunctionConfiguration{..} = mconcat
        [ "Role"        =? _ufcRole
        , "Handler"     =? _ufcHandler
        , "Description" =? _ufcDescription
        , "Timeout"     =? _ufcTimeout
        , "MemorySize"  =? _ufcMemorySize
        ]

instance ToJSON UpdateFunctionConfiguration where
    toJSON = genericToJSON jsonOptions
