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
-- Module      : Network.AWS.OpsWorks.DescribeRDSDBInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon RDS instances.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
-- This call accepts only one resource-identifying parameter.
--
module Network.AWS.OpsWorks.DescribeRDSDBInstances
    (
    -- * Creating a Request
      describeRDSDBInstances
    , DescribeRDSDBInstances
    -- * Request Lenses
    , drdiRDSDBInstanceARNs
    , drdiStackId

    -- * Destructuring the Response
    , describeRDSDBInstancesResponse
    , DescribeRDSDBInstancesResponse
    -- * Response Lenses
    , drdirsRDSDBInstances
    , drdirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRDSDBInstances' smart constructor.
data DescribeRDSDBInstances = DescribeRDSDBInstances'
  { _drdiRDSDBInstanceARNs :: !(Maybe [Text])
  , _drdiStackId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRDSDBInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdiRDSDBInstanceARNs' - An array containing the ARNs of the instances to be described.
--
-- * 'drdiStackId' - The stack ID that the instances are registered with. The operation returns descriptions of all registered Amazon RDS instances.
describeRDSDBInstances
    :: Text -- ^ 'drdiStackId'
    -> DescribeRDSDBInstances
describeRDSDBInstances pStackId_ =
  DescribeRDSDBInstances'
    {_drdiRDSDBInstanceARNs = Nothing, _drdiStackId = pStackId_}


-- | An array containing the ARNs of the instances to be described.
drdiRDSDBInstanceARNs :: Lens' DescribeRDSDBInstances [Text]
drdiRDSDBInstanceARNs = lens _drdiRDSDBInstanceARNs (\ s a -> s{_drdiRDSDBInstanceARNs = a}) . _Default . _Coerce

-- | The stack ID that the instances are registered with. The operation returns descriptions of all registered Amazon RDS instances.
drdiStackId :: Lens' DescribeRDSDBInstances Text
drdiStackId = lens _drdiStackId (\ s a -> s{_drdiStackId = a})

instance AWSRequest DescribeRDSDBInstances where
        type Rs DescribeRDSDBInstances =
             DescribeRDSDBInstancesResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRDSDBInstancesResponse' <$>
                   (x .?> "RdsDbInstances" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRDSDBInstances where

instance NFData DescribeRDSDBInstances where

instance ToHeaders DescribeRDSDBInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeRdsDbInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRDSDBInstances where
        toJSON DescribeRDSDBInstances'{..}
          = object
              (catMaybes
                 [("RdsDbInstanceArns" .=) <$> _drdiRDSDBInstanceARNs,
                  Just ("StackId" .= _drdiStackId)])

instance ToPath DescribeRDSDBInstances where
        toPath = const "/"

instance ToQuery DescribeRDSDBInstances where
        toQuery = const mempty

-- | Contains the response to a @DescribeRdsDbInstances@ request.
--
--
--
-- /See:/ 'describeRDSDBInstancesResponse' smart constructor.
data DescribeRDSDBInstancesResponse = DescribeRDSDBInstancesResponse'
  { _drdirsRDSDBInstances :: !(Maybe [RDSDBInstance])
  , _drdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRDSDBInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdirsRDSDBInstances' - An a array of @RdsDbInstance@ objects that describe the instances.
--
-- * 'drdirsResponseStatus' - -- | The response status code.
describeRDSDBInstancesResponse
    :: Int -- ^ 'drdirsResponseStatus'
    -> DescribeRDSDBInstancesResponse
describeRDSDBInstancesResponse pResponseStatus_ =
  DescribeRDSDBInstancesResponse'
    {_drdirsRDSDBInstances = Nothing, _drdirsResponseStatus = pResponseStatus_}


-- | An a array of @RdsDbInstance@ objects that describe the instances.
drdirsRDSDBInstances :: Lens' DescribeRDSDBInstancesResponse [RDSDBInstance]
drdirsRDSDBInstances = lens _drdirsRDSDBInstances (\ s a -> s{_drdirsRDSDBInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
drdirsResponseStatus :: Lens' DescribeRDSDBInstancesResponse Int
drdirsResponseStatus = lens _drdirsResponseStatus (\ s a -> s{_drdirsResponseStatus = a})

instance NFData DescribeRDSDBInstancesResponse where
