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
-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resource descriptions for running and deleted stacks. If
-- 'StackName' is specified, all the associated resources that are part of
-- the stack are returned. If 'PhysicalResourceId' is specified, the
-- associated resources of the stack that the resource belongs to are
-- returned.
--
-- Only the first 100 resources will be returned. If your stack has more
-- resources than this, you should use 'ListStackResources' instead.
--
-- For deleted stacks, 'DescribeStackResources' returns resource
-- information for up to 90 days after the stack has been deleted.
--
-- You must specify either 'StackName' or 'PhysicalResourceId', but not
-- both. In addition, you can specify 'LogicalResourceId' to filter the
-- returned result. For more information about resources, the
-- 'LogicalResourceId' and 'PhysicalResourceId', go to the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide AWS CloudFormation User Guide>.
--
-- A 'ValidationError' is returned if you specify both 'StackName' and
-- 'PhysicalResourceId' in the same request.
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResources.html AWS API Reference> for DescribeStackResources.
module Network.AWS.CloudFormation.DescribeStackResources
    (
    -- * Creating a Request
      describeStackResources
    , DescribeStackResources
    -- * Request Lenses
    , dsrLogicalResourceId
    , dsrPhysicalResourceId
    , dsrStackName

    -- * Destructuring the Response
    , describeStackResourcesResponse
    , DescribeStackResourcesResponse
    -- * Response Lenses
    , drsStackResources
    , drsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStackResources action.
--
-- /See:/ 'describeStackResources' smart constructor.
data DescribeStackResources = DescribeStackResources'
    { _dsrLogicalResourceId  :: !(Maybe Text)
    , _dsrPhysicalResourceId :: !(Maybe Text)
    , _dsrStackName          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStackResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrLogicalResourceId'
--
-- * 'dsrPhysicalResourceId'
--
-- * 'dsrStackName'
describeStackResources
    :: DescribeStackResources
describeStackResources =
    DescribeStackResources'
    { _dsrLogicalResourceId = Nothing
    , _dsrPhysicalResourceId = Nothing
    , _dsrStackName = Nothing
    }

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
dsrLogicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrLogicalResourceId = lens _dsrLogicalResourceId (\ s a -> s{_dsrLogicalResourceId = a});

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance,
-- 'PhysicalResourceId' corresponds to the 'InstanceId'. You can pass the
-- EC2 'InstanceId' to 'DescribeStackResources' to find which stack the
-- instance belongs to and what other resources are part of the stack.
--
-- Required: Conditional. If you do not specify 'PhysicalResourceId', you
-- must specify 'StackName'.
--
-- Default: There is no default value.
dsrPhysicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrPhysicalResourceId = lens _dsrPhysicalResourceId (\ s a -> s{_dsrPhysicalResourceId = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- Required: Conditional. If you do not specify 'StackName', you must
-- specify 'PhysicalResourceId'.
dsrStackName :: Lens' DescribeStackResources (Maybe Text)
dsrStackName = lens _dsrStackName (\ s a -> s{_dsrStackName = a});

instance AWSRequest DescribeStackResources where
        type Rs DescribeStackResources =
             DescribeStackResourcesResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackResourcesResult"
              (\ s h x ->
                 DescribeStackResourcesResponse' <$>
                   (x .@? "StackResources" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeStackResources where
        toHeaders = const mempty

instance ToPath DescribeStackResources where
        toPath = const "/"

instance ToQuery DescribeStackResources where
        toQuery DescribeStackResources'{..}
          = mconcat
              ["Action" =:
                 ("DescribeStackResources" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "LogicalResourceId" =: _dsrLogicalResourceId,
               "PhysicalResourceId" =: _dsrPhysicalResourceId,
               "StackName" =: _dsrStackName]

-- | The output for a DescribeStackResources action.
--
-- /See:/ 'describeStackResourcesResponse' smart constructor.
data DescribeStackResourcesResponse = DescribeStackResourcesResponse'
    { _drsStackResources :: !(Maybe [StackResource])
    , _drsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStackResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsStackResources'
--
-- * 'drsStatus'
describeStackResourcesResponse
    :: Int -- ^ 'drsStatus'
    -> DescribeStackResourcesResponse
describeStackResourcesResponse pStatus_ =
    DescribeStackResourcesResponse'
    { _drsStackResources = Nothing
    , _drsStatus = pStatus_
    }

-- | A list of 'StackResource' structures.
drsStackResources :: Lens' DescribeStackResourcesResponse [StackResource]
drsStackResources = lens _drsStackResources (\ s a -> s{_drsStackResources = a}) . _Default . _Coerce;

-- | The response status code.
drsStatus :: Lens' DescribeStackResourcesResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
