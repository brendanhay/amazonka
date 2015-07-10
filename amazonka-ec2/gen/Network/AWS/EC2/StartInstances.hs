{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon EBS-backed AMI that you\'ve previously stopped.
--
-- Instances that use Amazon EBS volumes as their root devices can be
-- quickly stopped and started. When an instance is stopped, the compute
-- resources are released and you are not billed for hourly instance usage.
-- However, your root partition Amazon EBS volume remains, continues to
-- persist your data, and you are charged for Amazon EBS volume usage. You
-- can restart your instance at any time. Each time you transition an
-- instance from stopped to started, Amazon EC2 charges a full instance
-- hour, even if transitions happen multiple times within a single hour.
--
-- Before stopping an instance, make sure it is in a state from which it
-- can be restarted. Stopping an instance does not preserve data stored in
-- RAM.
--
-- Performing this operation on an instance that uses an instance store as
-- its root device returns an error.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stopping Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html>
module Network.AWS.EC2.StartInstances
    (
    -- * Request
      StartInstances
    -- ** Request constructor
    , startInstances
    -- ** Request lenses
    , staAdditionalInfo
    , staDryRun
    , staInstanceIds

    -- * Response
    , StartInstancesResponse
    -- ** Response constructor
    , startInstancesResponse
    -- ** Response lenses
    , staStartingInstances
    , staStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staAdditionalInfo'
--
-- * 'staDryRun'
--
-- * 'staInstanceIds'
data StartInstances = StartInstances'
    { _staAdditionalInfo :: !(Maybe Text)
    , _staDryRun         :: !(Maybe Bool)
    , _staInstanceIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstances' smart constructor.
startInstances :: StartInstances
startInstances =
    StartInstances'
    { _staAdditionalInfo = Nothing
    , _staDryRun = Nothing
    , _staInstanceIds = mempty
    }

-- | Reserved.
staAdditionalInfo :: Lens' StartInstances (Maybe Text)
staAdditionalInfo = lens _staAdditionalInfo (\ s a -> s{_staAdditionalInfo = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
staDryRun :: Lens' StartInstances (Maybe Bool)
staDryRun = lens _staDryRun (\ s a -> s{_staDryRun = a});

-- | One or more instance IDs.
staInstanceIds :: Lens' StartInstances [Text]
staInstanceIds = lens _staInstanceIds (\ s a -> s{_staInstanceIds = a});

instance AWSRequest StartInstances where
        type Sv StartInstances = EC2
        type Rs StartInstances = StartInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 StartInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders StartInstances where
        toHeaders = const mempty

instance ToPath StartInstances where
        toPath = const "/"

instance ToQuery StartInstances where
        toQuery StartInstances'{..}
          = mconcat
              ["Action" =: ("StartInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AdditionalInfo" =: _staAdditionalInfo,
               "DryRun" =: _staDryRun,
               toQueryList "InstanceId" _staInstanceIds]

-- | /See:/ 'startInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staStartingInstances'
--
-- * 'staStatus'
data StartInstancesResponse = StartInstancesResponse'
    { _staStartingInstances :: !(Maybe [InstanceStateChange])
    , _staStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstancesResponse' smart constructor.
startInstancesResponse :: Int -> StartInstancesResponse
startInstancesResponse pStatus =
    StartInstancesResponse'
    { _staStartingInstances = Nothing
    , _staStatus = pStatus
    }

-- | Information about one or more started instances.
staStartingInstances :: Lens' StartInstancesResponse [InstanceStateChange]
staStartingInstances = lens _staStartingInstances (\ s a -> s{_staStartingInstances = a}) . _Default;

-- | FIXME: Undocumented member.
staStatus :: Lens' StartInstancesResponse Int
staStatus = lens _staStatus (\ s a -> s{_staStatus = a});
