{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRDSDBInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon RDS instances.
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
    , drdirsRDSDBInstances
    , drdirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRDSDBInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdiRDSDBInstanceARNs'
--
-- * 'drdiStackId'
data DescribeRDSDBInstances = DescribeRDSDBInstances'
    { _drdiRDSDBInstanceARNs :: !(Maybe [Text])
    , _drdiStackId           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRDSDBInstances' smart constructor.
describeRDSDBInstances :: Text -> DescribeRDSDBInstances
describeRDSDBInstances pStackId_ =
    DescribeRDSDBInstances'
    { _drdiRDSDBInstanceARNs = Nothing
    , _drdiStackId = pStackId_
    }

-- | An array containing the ARNs of the instances to be described.
drdiRDSDBInstanceARNs :: Lens' DescribeRDSDBInstances [Text]
drdiRDSDBInstanceARNs = lens _drdiRDSDBInstanceARNs (\ s a -> s{_drdiRDSDBInstanceARNs = a}) . _Default;

-- | The stack ID that the instances are registered with. The operation
-- returns descriptions of all registered Amazon RDS instances.
drdiStackId :: Lens' DescribeRDSDBInstances Text
drdiStackId = lens _drdiStackId (\ s a -> s{_drdiStackId = a});

instance AWSRequest DescribeRDSDBInstances where
        type Sv DescribeRDSDBInstances = OpsWorks
        type Rs DescribeRDSDBInstances =
             DescribeRDSDBInstancesResponse
        request = postJSON "DescribeRDSDBInstances"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRDSDBInstancesResponse' <$>
                   (x .?> "RdsDbInstances" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | Contains the response to a @DescribeRdsDbInstances@ request.
--
-- /See:/ 'describeRDSDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdirsRDSDBInstances'
--
-- * 'drdirsStatus'
data DescribeRDSDBInstancesResponse = DescribeRDSDBInstancesResponse'
    { _drdirsRDSDBInstances :: !(Maybe [RDSDBInstance])
    , _drdirsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRDSDBInstancesResponse' smart constructor.
describeRDSDBInstancesResponse :: Int -> DescribeRDSDBInstancesResponse
describeRDSDBInstancesResponse pStatus_ =
    DescribeRDSDBInstancesResponse'
    { _drdirsRDSDBInstances = Nothing
    , _drdirsStatus = pStatus_
    }

-- | An a array of @RdsDbInstance@ objects that describe the instances.
drdirsRDSDBInstances :: Lens' DescribeRDSDBInstancesResponse [RDSDBInstance]
drdirsRDSDBInstances = lens _drdirsRDSDBInstances (\ s a -> s{_drdirsRDSDBInstances = a}) . _Default;

-- | FIXME: Undocumented member.
drdirsStatus :: Lens' DescribeRDSDBInstancesResponse Int
drdirsStatus = lens _drdirsStatus (\ s a -> s{_drdirsStatus = a});
