{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
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

-- | Gets information about one or more on-premises instances.
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'batchGetOnPremisesInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopiInstanceNames'
newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'{_bgopiInstanceNames :: [Text]} deriving (Eq, Read, Show)

-- | 'BatchGetOnPremisesInstances' smart constructor.
batchGetOnPremisesInstances :: BatchGetOnPremisesInstances
batchGetOnPremisesInstances = BatchGetOnPremisesInstances'{_bgopiInstanceNames = mempty};

-- | The names of the on-premises instances to get information about.
bgopiInstanceNames :: Lens' BatchGetOnPremisesInstances [Text]
bgopiInstanceNames = lens _bgopiInstanceNames (\ s a -> s{_bgopiInstanceNames = a});

instance AWSRequest BatchGetOnPremisesInstances where
        type Sv BatchGetOnPremisesInstances = CodeDeploy
        type Rs BatchGetOnPremisesInstances =
             BatchGetOnPremisesInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetOnPremisesInstancesResponse' <$>
                   x .?> "instanceInfos" .!@ mempty)

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

-- | /See:/ 'batchGetOnPremisesInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopirInstanceInfos'
newtype BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'{_bgopirInstanceInfos :: [InstanceInfo]} deriving (Eq, Read, Show)

-- | 'BatchGetOnPremisesInstancesResponse' smart constructor.
batchGetOnPremisesInstancesResponse :: BatchGetOnPremisesInstancesResponse
batchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'{_bgopirInstanceInfos = mempty};

-- | Information about the on-premises instances.
bgopirInstanceInfos :: Lens' BatchGetOnPremisesInstancesResponse [InstanceInfo]
bgopirInstanceInfos = lens _bgopirInstanceInfos (\ s a -> s{_bgopirInstanceInfos = a});
