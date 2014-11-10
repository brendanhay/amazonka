{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates a request to compile the specified type of information of the
-- deployed environment. Setting the InfoType to tail compiles the last lines
-- from the application server log files of every Amazon EC2 instance in your
-- environment. Use RetrieveEnvironmentInfo to access the compiled
-- information. Related Topics RetrieveEnvironmentInfo.
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    (
    -- * Request
      RequestEnvironmentInfoMessage
    -- ** Request constructor
    , requestEnvironmentInfo
    -- ** Request lenses
    , reim1EnvironmentId
    , reim1EnvironmentName
    , reim1InfoType

    -- * Response
    , RequestEnvironmentInfoResponse
    -- ** Response constructor
    , requestEnvironmentInfoResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data RequestEnvironmentInfoMessage = RequestEnvironmentInfoMessage
    { _reim1EnvironmentId   :: Maybe Text
    , _reim1EnvironmentName :: Maybe Text
    , _reim1InfoType        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RequestEnvironmentInfoMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reim1EnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'reim1EnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'reim1InfoType' @::@ 'Text'
--
requestEnvironmentInfo :: Text -- ^ 'reim1InfoType'
                       -> RequestEnvironmentInfoMessage
requestEnvironmentInfo p1 = RequestEnvironmentInfoMessage
    { _reim1InfoType        = p1
    , _reim1EnvironmentId   = Nothing
    , _reim1EnvironmentName = Nothing
    }

-- | The ID of the environment of the requested data. If no such environment
-- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reim1EnvironmentId :: Lens' RequestEnvironmentInfoMessage (Maybe Text)
reim1EnvironmentId =
    lens _reim1EnvironmentId (\s a -> s { _reim1EnvironmentId = a })

-- | The name of the environment of the requested data. If no such environment
-- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reim1EnvironmentName :: Lens' RequestEnvironmentInfoMessage (Maybe Text)
reim1EnvironmentName =
    lens _reim1EnvironmentName (\s a -> s { _reim1EnvironmentName = a })

-- | The type of information to request.
reim1InfoType :: Lens' RequestEnvironmentInfoMessage Text
reim1InfoType = lens _reim1InfoType (\s a -> s { _reim1InfoType = a })

instance ToPath RequestEnvironmentInfoMessage where
    toPath = const "/"

instance ToQuery RequestEnvironmentInfoMessage

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse

-- | 'RequestEnvironmentInfoResponse' constructor.
requestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse
requestEnvironmentInfoResponse = RequestEnvironmentInfoResponse

instance AWSRequest RequestEnvironmentInfoMessage where
    type Sv RequestEnvironmentInfoMessage = ElasticBeanstalk
    type Rs RequestEnvironmentInfoMessage = RequestEnvironmentInfoResponse

    request  = post "RequestEnvironmentInfo"
    response = const (nullaryResponse RequestEnvironmentInfoResponse)
