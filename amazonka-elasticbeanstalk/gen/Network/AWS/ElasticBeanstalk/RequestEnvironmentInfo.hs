{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Initiates a request to compile the specified type of information of the
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
    , reirqEnvironmentName
    , reirqEnvironmentId
    , reirqInfoType

    -- * Response
    , RequestEnvironmentInfoResponse
    -- ** Response constructor
    , requestEnvironmentInfoResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'requestEnvironmentInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirqEnvironmentName'
--
-- * 'reirqEnvironmentId'
--
-- * 'reirqInfoType'
data RequestEnvironmentInfo = RequestEnvironmentInfo'
    { _reirqEnvironmentName :: !(Maybe Text)
    , _reirqEnvironmentId   :: !(Maybe Text)
    , _reirqInfoType        :: !EnvironmentInfoType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestEnvironmentInfo' smart constructor.
requestEnvironmentInfo :: EnvironmentInfoType -> RequestEnvironmentInfo
requestEnvironmentInfo pInfoType =
    RequestEnvironmentInfo'
    { _reirqEnvironmentName = Nothing
    , _reirqEnvironmentId = Nothing
    , _reirqInfoType = pInfoType
    }

-- | The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reirqEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reirqEnvironmentName = lens _reirqEnvironmentName (\ s a -> s{_reirqEnvironmentName = a});

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reirqEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reirqEnvironmentId = lens _reirqEnvironmentId (\ s a -> s{_reirqEnvironmentId = a});

-- | The type of information to request.
reirqInfoType :: Lens' RequestEnvironmentInfo EnvironmentInfoType
reirqInfoType = lens _reirqInfoType (\ s a -> s{_reirqInfoType = a});

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
               "EnvironmentName" =: _reirqEnvironmentName,
               "EnvironmentId" =: _reirqEnvironmentId,
               "InfoType" =: _reirqInfoType]

-- | /See:/ 'requestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse =
    RequestEnvironmentInfoResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestEnvironmentInfoResponse' smart constructor.
requestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse
requestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
