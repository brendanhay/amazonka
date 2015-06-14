{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- Setting the @InfoType@ to @tail@ compiles the last lines from the
-- application server log files of every Amazon EC2 instance in your
-- environment.
--
-- Setting the @InfoType@ to @bundle@ compresses the application server log
-- files for every Amazon EC2 instance into a @.zip@ file. Legacy and .NET
-- containers do not support bundle logs.
--
-- Use RetrieveEnvironmentInfo to obtain the set of logs.
--
-- Related Topics
--
-- -   RetrieveEnvironmentInfo
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
    , reiInfoType
    , reiEnvironmentName

    -- * Response
    , RequestEnvironmentInfoResponse
    -- ** Response constructor
    , requestEnvironmentInfoResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'requestEnvironmentInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reiEnvironmentId'
--
-- * 'reiInfoType'
--
-- * 'reiEnvironmentName'
data RequestEnvironmentInfo = RequestEnvironmentInfo'{_reiEnvironmentId :: Maybe Text, _reiInfoType :: EnvironmentInfoType, _reiEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'RequestEnvironmentInfo' smart constructor.
requestEnvironmentInfo :: EnvironmentInfoType -> Text -> RequestEnvironmentInfo
requestEnvironmentInfo pInfoType pEnvironmentName = RequestEnvironmentInfo'{_reiEnvironmentId = Nothing, _reiInfoType = pInfoType, _reiEnvironmentName = pEnvironmentName};

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reiEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentId = lens _reiEnvironmentId (\ s a -> s{_reiEnvironmentId = a});

-- | The type of information to request.
reiInfoType :: Lens' RequestEnvironmentInfo EnvironmentInfoType
reiInfoType = lens _reiInfoType (\ s a -> s{_reiInfoType = a});

-- | The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reiEnvironmentName :: Lens' RequestEnvironmentInfo Text
reiEnvironmentName = lens _reiEnvironmentName (\ s a -> s{_reiEnvironmentName = a});

instance AWSRequest RequestEnvironmentInfo where
        type Sv RequestEnvironmentInfo = ElasticBeanstalk
        type Rs RequestEnvironmentInfo =
             RequestEnvironmentInfoResponse
        request = post
        response
          = receiveNull RequestEnvironmentInfoResponse'

instance ToHeaders RequestEnvironmentInfo where
        toHeaders = const mempty

instance ToPath RequestEnvironmentInfo where
        toPath = const "/"

instance ToQuery RequestEnvironmentInfo where
        toQuery RequestEnvironmentInfo'{..}
          = mconcat
              ["Action" =:
                 ("RequestEnvironmentInfo" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentId" =: _reiEnvironmentId,
               "InfoType" =: _reiInfoType,
               "EnvironmentName" =: _reiEnvironmentName]

-- | /See:/ 'requestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse' deriving (Eq, Read, Show)

-- | 'RequestEnvironmentInfoResponse' smart constructor.
requestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse
requestEnvironmentInfoResponse = RequestEnvironmentInfoResponse';
