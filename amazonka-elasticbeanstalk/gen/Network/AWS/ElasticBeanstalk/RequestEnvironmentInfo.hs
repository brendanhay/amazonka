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

-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
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

-- | Initiates a request to compile the specified type of information of the
-- deployed environment.
--
-- Setting the 'InfoType' to 'tail' compiles the last lines from the application
-- server log files of every Amazon EC2 instance in your environment. Use 'RetrieveEnvironmentInfo' to access the compiled information.
--
-- Related Topics
--
-- 'RetrieveEnvironmentInfo'
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RequestEnvironmentInfo.html>
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    (
    -- * Request
      RequestEnvironmentInfo
    -- ** Request constructor
    , requestEnvironmentInfo
    -- ** Request lenses
    , reiEnvironmentId
    , reiEnvironmentName
    , reiInfoType

    -- * Response
    , RequestEnvironmentInfoResponse
    -- ** Response constructor
    , requestEnvironmentInfoResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data RequestEnvironmentInfo = RequestEnvironmentInfo
    { _reiEnvironmentId   :: Maybe Text
    , _reiEnvironmentName :: Maybe Text
    , _reiInfoType        :: EnvironmentInfoType
    } deriving (Eq, Read, Show)

-- | 'RequestEnvironmentInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reiEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'reiEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'reiInfoType' @::@ 'EnvironmentInfoType'
--
requestEnvironmentInfo :: EnvironmentInfoType -- ^ 'reiInfoType'
                       -> RequestEnvironmentInfo
requestEnvironmentInfo p1 = RequestEnvironmentInfo
    { _reiInfoType        = p1
    , _reiEnvironmentId   = Nothing
    , _reiEnvironmentName = Nothing
    }

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, 'RequestEnvironmentInfo' returns an 'InvalidParameterValue' error.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns 'MissingRequiredParameter' error.
reiEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentId = lens _reiEnvironmentId (\s a -> s { _reiEnvironmentId = a })

-- | The name of the environment of the requested data.
--
-- If no such environment is found, 'RequestEnvironmentInfo' returns an 'InvalidParameterValue' error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns 'MissingRequiredParameter' error.
reiEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentName =
    lens _reiEnvironmentName (\s a -> s { _reiEnvironmentName = a })

-- | The type of information to request.
reiInfoType :: Lens' RequestEnvironmentInfo EnvironmentInfoType
reiInfoType = lens _reiInfoType (\s a -> s { _reiInfoType = a })

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RequestEnvironmentInfoResponse' constructor.
requestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse
requestEnvironmentInfoResponse = RequestEnvironmentInfoResponse

instance ToPath RequestEnvironmentInfo where
    toPath = const "/"

instance ToQuery RequestEnvironmentInfo where
    toQuery RequestEnvironmentInfo{..} = mconcat
        [ "EnvironmentId"   =? _reiEnvironmentId
        , "EnvironmentName" =? _reiEnvironmentName
        , "InfoType"        =? _reiInfoType
        ]

instance ToHeaders RequestEnvironmentInfo

instance AWSRequest RequestEnvironmentInfo where
    type Sv RequestEnvironmentInfo = ElasticBeanstalk
    type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse

    request  = post "RequestEnvironmentInfo"
    response = nullResponse RequestEnvironmentInfoResponse
