{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the compiled information from a RequestEnvironmentInfo
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
    , rEnvironmentName
    , rEnvironmentId
    , rInfoType

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response constructor
    , retrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirsEnvironmentInfo
    , reirsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'retrieveEnvironmentInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rEnvironmentName'
--
-- * 'rEnvironmentId'
--
-- * 'rInfoType'
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo'
    { _rEnvironmentName :: !(Maybe Text)
    , _rEnvironmentId   :: !(Maybe Text)
    , _rInfoType        :: !EnvironmentInfoType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveEnvironmentInfo' smart constructor.
retrieveEnvironmentInfo :: EnvironmentInfoType -> RetrieveEnvironmentInfo
retrieveEnvironmentInfo pInfoType_ =
    RetrieveEnvironmentInfo'
    { _rEnvironmentName = Nothing
    , _rEnvironmentId = Nothing
    , _rInfoType = pInfoType_
    }

-- | The name of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rEnvironmentName :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rEnvironmentName = lens _rEnvironmentName (\ s a -> s{_rEnvironmentName = a});

-- | The ID of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rEnvironmentId :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rEnvironmentId = lens _rEnvironmentId (\ s a -> s{_rEnvironmentId = a});

-- | The type of information to retrieve.
rInfoType :: Lens' RetrieveEnvironmentInfo EnvironmentInfoType
rInfoType = lens _rInfoType (\ s a -> s{_rInfoType = a});

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
               "EnvironmentName" =: _rEnvironmentName,
               "EnvironmentId" =: _rEnvironmentId,
               "InfoType" =: _rInfoType]

-- | Result message containing a description of the requested environment
-- info.
--
-- /See:/ 'retrieveEnvironmentInfoResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirsEnvironmentInfo'
--
-- * 'reirsStatus'
data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'
    { _reirsEnvironmentInfo :: !(Maybe [EnvironmentInfoDescription])
    , _reirsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveEnvironmentInfoResponse' smart constructor.
retrieveEnvironmentInfoResponse :: Int -> RetrieveEnvironmentInfoResponse
retrieveEnvironmentInfoResponse pStatus_ =
    RetrieveEnvironmentInfoResponse'
    { _reirsEnvironmentInfo = Nothing
    , _reirsStatus = pStatus_
    }

-- | The EnvironmentInfoDescription of the environment.
reirsEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResponse [EnvironmentInfoDescription]
reirsEnvironmentInfo = lens _reirsEnvironmentInfo (\ s a -> s{_reirsEnvironmentInfo = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
reirsStatus :: Lens' RetrieveEnvironmentInfoResponse Int
reirsStatus = lens _reirsStatus (\ s a -> s{_reirsStatus = a});
