{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.GetFunction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the configuration information of the Lambda function and a
-- presigned URL link to the .zip file you uploaded with UploadFunction so you
-- can download the .zip file. Note that the URL is valid for up to 10
-- minutes. The configuration information is the same information you provided
-- as parameters when uploading the function. This operation requires
-- permission for the lambda:GetFunction action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunction.html>
module Network.AWS.Lambda.GetFunction
    (
    -- * Request
      GetFunction
    -- ** Request constructor
    , getFunction
    -- ** Request lenses
    , gfFunctionName

    -- * Response
    , GetFunctionResponse
    -- ** Response constructor
    , getFunctionResponse
    -- ** Response lenses
    , gfrCode
    , gfrConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype GetFunction = GetFunction
    { _gfFunctionName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetFunction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfFunctionName' @::@ 'Text'
--
getFunction :: Text -- ^ 'gfFunctionName'
            -> GetFunction
getFunction p1 = GetFunction
    { _gfFunctionName = p1
    }

-- | The Lambda function name.
gfFunctionName :: Lens' GetFunction Text
gfFunctionName = lens _gfFunctionName (\s a -> s { _gfFunctionName = a })

data GetFunctionResponse = GetFunctionResponse
    { _gfrCode          :: Maybe FunctionCodeLocation
    , _gfrConfiguration :: Maybe FunctionConfiguration
    } deriving (Eq, Show, Generic)

-- | 'GetFunctionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gfrCode' @::@ 'Maybe' 'FunctionCodeLocation'
--
-- * 'gfrConfiguration' @::@ 'Maybe' 'FunctionConfiguration'
--
getFunctionResponse :: GetFunctionResponse
getFunctionResponse = GetFunctionResponse
    { _gfrConfiguration = Nothing
    , _gfrCode          = Nothing
    }

gfrCode :: Lens' GetFunctionResponse (Maybe FunctionCodeLocation)
gfrCode = lens _gfrCode (\s a -> s { _gfrCode = a })

gfrConfiguration :: Lens' GetFunctionResponse (Maybe FunctionConfiguration)
gfrConfiguration = lens _gfrConfiguration (\s a -> s { _gfrConfiguration = a })

instance ToPath GetFunction where
    toPath GetFunction{..} = mconcat
        [ "/2014-11-13/functions/"
        , toText _gfFunctionName
        ]

instance ToQuery GetFunction where
    toQuery = const mempty

instance ToHeaders GetFunction
instance ToJSON GetFunction where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GetFunction where
    type Sv GetFunction = Lambda
    type Rs GetFunction = GetFunctionResponse

    request  = get
    response = jsonResponse

instance FromJSON GetFunctionResponse where
    parseJSON = genericParseJSON jsonOptions
