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
    , sAdditionalInfo
    , sDryRun
    , sInstanceIds

    -- * Response
    , StartInstancesResponse
    -- ** Response constructor
    , startInstancesResponse
    -- ** Response lenses
    , srsStartingInstances
    , srsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sAdditionalInfo'
--
-- * 'sDryRun'
--
-- * 'sInstanceIds'
data StartInstances = StartInstances'
    { _sAdditionalInfo :: !(Maybe Text)
    , _sDryRun         :: !(Maybe Bool)
    , _sInstanceIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstances' smart constructor.
startInstances :: StartInstances
startInstances =
    StartInstances'
    { _sAdditionalInfo = Nothing
    , _sDryRun = Nothing
    , _sInstanceIds = mempty
    }

-- | Reserved.
sAdditionalInfo :: Lens' StartInstances (Maybe Text)
sAdditionalInfo = lens _sAdditionalInfo (\ s a -> s{_sAdditionalInfo = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
sDryRun :: Lens' StartInstances (Maybe Bool)
sDryRun = lens _sDryRun (\ s a -> s{_sDryRun = a});

-- | One or more instance IDs.
sInstanceIds :: Lens' StartInstances [Text]
sInstanceIds = lens _sInstanceIds (\ s a -> s{_sInstanceIds = a});

instance AWSRequest StartInstances where
        type Sv StartInstances = EC2
        type Rs StartInstances = StartInstancesResponse
        request = post "StartInstances"
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
               "AdditionalInfo" =: _sAdditionalInfo,
               "DryRun" =: _sDryRun,
               toQueryList "InstanceId" _sInstanceIds]

-- | /See:/ 'startInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsStartingInstances'
--
-- * 'srsStatus'
data StartInstancesResponse = StartInstancesResponse'
    { _srsStartingInstances :: !(Maybe [InstanceStateChange])
    , _srsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartInstancesResponse' smart constructor.
startInstancesResponse :: Int -> StartInstancesResponse
startInstancesResponse pStatus_ =
    StartInstancesResponse'
    { _srsStartingInstances = Nothing
    , _srsStatus = pStatus_
    }

-- | Information about one or more started instances.
srsStartingInstances :: Lens' StartInstancesResponse [InstanceStateChange]
srsStartingInstances = lens _srsStartingInstances (\ s a -> s{_srsStartingInstances = a}) . _Default;

-- | FIXME: Undocumented member.
srsStatus :: Lens' StartInstancesResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
