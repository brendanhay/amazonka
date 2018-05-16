{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon EBS-backed instance that you've previously stopped.
--
--
-- Instances that use Amazon EBS volumes as their root devices can be quickly stopped and started. When an instance is stopped, the compute resources are released and you are not billed for instance usage. However, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. You can restart your instance at any time. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
--
-- Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.
--
-- Performing this operation on an instance that uses an instance store as its root device returns an error.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stopping Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.StartInstances
    (
    -- * Creating a Request
      startInstances
    , StartInstances
    -- * Request Lenses
    , sAdditionalInfo
    , sDryRun
    , sInstanceIds

    -- * Destructuring the Response
    , startInstancesResponse
    , StartInstancesResponse
    -- * Response Lenses
    , srsStartingInstances
    , srsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for StartInstances.
--
--
--
-- /See:/ 'startInstances' smart constructor.
data StartInstances = StartInstances'
  { _sAdditionalInfo :: !(Maybe Text)
  , _sDryRun         :: !(Maybe Bool)
  , _sInstanceIds    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAdditionalInfo' - Reserved.
--
-- * 'sDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'sInstanceIds' - One or more instance IDs.
startInstances
    :: StartInstances
startInstances =
  StartInstances'
    {_sAdditionalInfo = Nothing, _sDryRun = Nothing, _sInstanceIds = mempty}


-- | Reserved.
sAdditionalInfo :: Lens' StartInstances (Maybe Text)
sAdditionalInfo = lens _sAdditionalInfo (\ s a -> s{_sAdditionalInfo = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
sDryRun :: Lens' StartInstances (Maybe Bool)
sDryRun = lens _sDryRun (\ s a -> s{_sDryRun = a})

-- | One or more instance IDs.
sInstanceIds :: Lens' StartInstances [Text]
sInstanceIds = lens _sInstanceIds (\ s a -> s{_sInstanceIds = a}) . _Coerce

instance AWSRequest StartInstances where
        type Rs StartInstances = StartInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 StartInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable StartInstances where

instance NFData StartInstances where

instance ToHeaders StartInstances where
        toHeaders = const mempty

instance ToPath StartInstances where
        toPath = const "/"

instance ToQuery StartInstances where
        toQuery StartInstances'{..}
          = mconcat
              ["Action" =: ("StartInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AdditionalInfo" =: _sAdditionalInfo,
               "DryRun" =: _sDryRun,
               toQueryList "InstanceId" _sInstanceIds]

-- | Contains the output of StartInstances.
--
--
--
-- /See:/ 'startInstancesResponse' smart constructor.
data StartInstancesResponse = StartInstancesResponse'
  { _srsStartingInstances :: !(Maybe [InstanceStateChange])
  , _srsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsStartingInstances' - Information about one or more started instances.
--
-- * 'srsResponseStatus' - -- | The response status code.
startInstancesResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartInstancesResponse
startInstancesResponse pResponseStatus_ =
  StartInstancesResponse'
    {_srsStartingInstances = Nothing, _srsResponseStatus = pResponseStatus_}


-- | Information about one or more started instances.
srsStartingInstances :: Lens' StartInstancesResponse [InstanceStateChange]
srsStartingInstances = lens _srsStartingInstances (\ s a -> s{_srsStartingInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
srsResponseStatus :: Lens' StartInstancesResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartInstancesResponse where
