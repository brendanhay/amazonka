{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetOnPremisesInstances.html>
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
    (
    -- * Request
      BatchGetOnPremisesInstances
    -- ** Request constructor
    , batchGetOnPremisesInstances
    -- ** Request lenses
    , bgopiInstanceNames

    -- * Response
    , BatchGetOnPremisesInstancesResponse
    -- ** Response constructor
    , batchGetOnPremisesInstancesResponse
    -- ** Response lenses
    , bgopirInstanceInfos
    , bgopirStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a batch get on-premises instances operation.
--
-- /See:/ 'batchGetOnPremisesInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopiInstanceNames'
newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'
    { _bgopiInstanceNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetOnPremisesInstances' smart constructor.
batchGetOnPremisesInstances :: BatchGetOnPremisesInstances
batchGetOnPremisesInstances =
    BatchGetOnPremisesInstances'
    { _bgopiInstanceNames = Nothing
    }

-- | The names of the on-premises instances to get information about.
bgopiInstanceNames :: Lens' BatchGetOnPremisesInstances [Text]
bgopiInstanceNames = lens _bgopiInstanceNames (\ s a -> s{_bgopiInstanceNames = a}) . _Default;

instance AWSRequest BatchGetOnPremisesInstances where
        type Sv BatchGetOnPremisesInstances = CodeDeploy
        type Rs BatchGetOnPremisesInstances =
             BatchGetOnPremisesInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetOnPremisesInstancesResponse' <$>
                   (x .?> "instanceInfos" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders BatchGetOnPremisesInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetOnPremisesInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetOnPremisesInstances where
        toJSON BatchGetOnPremisesInstances'{..}
          = object ["instanceNames" .= _bgopiInstanceNames]

instance ToPath BatchGetOnPremisesInstances where
        toPath = const "/"

instance ToQuery BatchGetOnPremisesInstances where
        toQuery = const mempty

-- | Represents the output of a batch get on-premises instances operation.
--
-- /See:/ 'batchGetOnPremisesInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopirInstanceInfos'
--
-- * 'bgopirStatus'
data BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'
    { _bgopirInstanceInfos :: !(Maybe [InstanceInfo])
    , _bgopirStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetOnPremisesInstancesResponse' smart constructor.
batchGetOnPremisesInstancesResponse :: Int -> BatchGetOnPremisesInstancesResponse
batchGetOnPremisesInstancesResponse pStatus =
    BatchGetOnPremisesInstancesResponse'
    { _bgopirInstanceInfos = Nothing
    , _bgopirStatus = pStatus
    }

-- | Information about the on-premises instances.
bgopirInstanceInfos :: Lens' BatchGetOnPremisesInstancesResponse [InstanceInfo]
bgopirInstanceInfos = lens _bgopirInstanceInfos (\ s a -> s{_bgopirInstanceInfos = a}) . _Default;

-- | FIXME: Undocumented member.
bgopirStatus :: Lens' BatchGetOnPremisesInstancesResponse Int
bgopirStatus = lens _bgopirStatus (\ s a -> s{_bgopirStatus = a});
