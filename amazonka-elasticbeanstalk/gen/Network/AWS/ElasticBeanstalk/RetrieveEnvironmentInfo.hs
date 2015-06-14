{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
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

-- | Retrieves the compiled information from a RequestEnvironmentInfo
-- request.
--
-- Related Topics
--
-- -   RequestEnvironmentInfo
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RetrieveEnvironmentInfo.html>
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    (
    -- * Request
      RetrieveEnvironmentInfo
    -- ** Request constructor
    , retrieveEnvironmentInfo
    -- ** Request lenses
    , retEnvironmentId
    , retInfoType
    , retEnvironmentName

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response constructor
    , retrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirEnvironmentInfo
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'retrieveEnvironmentInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'retEnvironmentId'
--
-- * 'retInfoType'
--
-- * 'retEnvironmentName'
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo'{_retEnvironmentId :: Maybe Text, _retInfoType :: EnvironmentInfoType, _retEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'RetrieveEnvironmentInfo' smart constructor.
retrieveEnvironmentInfo :: EnvironmentInfoType -> Text -> RetrieveEnvironmentInfo
retrieveEnvironmentInfo pInfoType pEnvironmentName = RetrieveEnvironmentInfo'{_retEnvironmentId = Nothing, _retInfoType = pInfoType, _retEnvironmentName = pEnvironmentName};

-- | The ID of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
retEnvironmentId :: Lens' RetrieveEnvironmentInfo (Maybe Text)
retEnvironmentId = lens _retEnvironmentId (\ s a -> s{_retEnvironmentId = a});

-- | The type of information to retrieve.
retInfoType :: Lens' RetrieveEnvironmentInfo EnvironmentInfoType
retInfoType = lens _retInfoType (\ s a -> s{_retInfoType = a});

-- | The name of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
retEnvironmentName :: Lens' RetrieveEnvironmentInfo Text
retEnvironmentName = lens _retEnvironmentName (\ s a -> s{_retEnvironmentName = a});

instance AWSRequest RetrieveEnvironmentInfo where
        type Sv RetrieveEnvironmentInfo = ElasticBeanstalk
        type Rs RetrieveEnvironmentInfo =
             RetrieveEnvironmentInfoResponse
        request = post
        response
          = receiveXMLWrapper "RetrieveEnvironmentInfoResult"
              (\ s h x ->
                 RetrieveEnvironmentInfoResponse' <$>
                   (x .@? "EnvironmentInfo" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders RetrieveEnvironmentInfo where
        toHeaders = const mempty

instance ToPath RetrieveEnvironmentInfo where
        toPath = const "/"

instance ToQuery RetrieveEnvironmentInfo where
        toQuery RetrieveEnvironmentInfo'{..}
          = mconcat
              ["Action" =:
                 ("RetrieveEnvironmentInfo" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentId" =: _retEnvironmentId,
               "InfoType" =: _retInfoType,
               "EnvironmentName" =: _retEnvironmentName]

-- | /See:/ 'retrieveEnvironmentInfoResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirEnvironmentInfo'
newtype RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'{_reirEnvironmentInfo :: [EnvironmentInfoDescription]} deriving (Eq, Read, Show)

-- | 'RetrieveEnvironmentInfoResponse' smart constructor.
retrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse
retrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'{_reirEnvironmentInfo = mempty};

-- | The EnvironmentInfoDescription of the environment.
reirEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResponse [EnvironmentInfoDescription]
reirEnvironmentInfo = lens _reirEnvironmentInfo (\ s a -> s{_reirEnvironmentInfo = a});
