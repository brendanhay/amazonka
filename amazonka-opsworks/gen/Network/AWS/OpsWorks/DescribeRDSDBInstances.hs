{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeRDSDBInstances
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

-- | Describes Amazon RDS instances.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeRDSDBInstances.html>
module Network.AWS.OpsWorks.DescribeRDSDBInstances
    (
    -- * Request
      DescribeRDSDBInstances
    -- ** Request constructor
    , describeRDSDBInstances
    -- ** Request lenses
    , drdiRDSDBInstanceARNs
    , drdiStackId

    -- * Response
    , DescribeRDSDBInstancesResponse
    -- ** Response constructor
    , describeRDSDBInstancesResponse
    -- ** Response lenses
    , drdirRDSDBInstances
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'describeRDSDBInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdiRDSDBInstanceARNs'
--
-- * 'drdiStackId'
data DescribeRDSDBInstances = DescribeRDSDBInstances'{_drdiRDSDBInstanceARNs :: Maybe [Text], _drdiStackId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeRDSDBInstances' smart constructor.
describeRDSDBInstances :: Text -> DescribeRDSDBInstances
describeRDSDBInstances pStackId = DescribeRDSDBInstances'{_drdiRDSDBInstanceARNs = Nothing, _drdiStackId = pStackId};

-- | An array containing the ARNs of the instances to be described.
drdiRDSDBInstanceARNs :: Lens' DescribeRDSDBInstances (Maybe [Text])
drdiRDSDBInstanceARNs = lens _drdiRDSDBInstanceARNs (\ s a -> s{_drdiRDSDBInstanceARNs = a});

-- | The stack ID that the instances are registered with. The operation
-- returns descriptions of all registered Amazon RDS instances.
drdiStackId :: Lens' DescribeRDSDBInstances Text
drdiStackId = lens _drdiStackId (\ s a -> s{_drdiStackId = a});

instance AWSRequest DescribeRDSDBInstances where
        type Sv DescribeRDSDBInstances = OpsWorks
        type Rs DescribeRDSDBInstances =
             DescribeRDSDBInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRDSDBInstancesResponse' <$>
                   x .?> "RdsDbInstances" .!@ mempty)

instance ToHeaders DescribeRDSDBInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeRDSDBInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRDSDBInstances where
        toJSON DescribeRDSDBInstances'{..}
          = object
              ["RdsDbInstanceArns" .= _drdiRDSDBInstanceARNs,
               "StackId" .= _drdiStackId]

instance ToPath DescribeRDSDBInstances where
        toPath = const "/"

instance ToQuery DescribeRDSDBInstances where
        toQuery = const mempty

-- | /See:/ 'describeRDSDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdirRDSDBInstances'
newtype DescribeRDSDBInstancesResponse = DescribeRDSDBInstancesResponse'{_drdirRDSDBInstances :: Maybe [RDSDBInstance]} deriving (Eq, Read, Show)

-- | 'DescribeRDSDBInstancesResponse' smart constructor.
describeRDSDBInstancesResponse :: DescribeRDSDBInstancesResponse
describeRDSDBInstancesResponse = DescribeRDSDBInstancesResponse'{_drdirRDSDBInstances = Nothing};

-- | An a array of @RdsDbInstance@ objects that describe the instances.
drdirRDSDBInstances :: Lens' DescribeRDSDBInstancesResponse (Maybe [RDSDBInstance])
drdirRDSDBInstances = lens _drdirRDSDBInstances (\ s a -> s{_drdirRDSDBInstances = a});
