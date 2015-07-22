{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resource descriptions for running and deleted stacks. If
-- @StackName@ is specified, all the associated resources that are part of
-- the stack are returned. If @PhysicalResourceId@ is specified, the
-- associated resources of the stack that the resource belongs to are
-- returned.
--
-- Only the first 100 resources will be returned. If your stack has more
-- resources than this, you should use @ListStackResources@ instead.
--
-- For deleted stacks, @DescribeStackResources@ returns resource
-- information for up to 90 days after the stack has been deleted.
--
-- You must specify either @StackName@ or @PhysicalResourceId@, but not
-- both. In addition, you can specify @LogicalResourceId@ to filter the
-- returned result. For more information about resources, the
-- @LogicalResourceId@ and @PhysicalResourceId@, go to the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide AWS CloudFormation User Guide>.
--
-- A @ValidationError@ is returned if you specify both @StackName@ and
-- @PhysicalResourceId@ in the same request.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResources.html>
module Network.AWS.CloudFormation.DescribeStackResources
    (
    -- * Request
      DescribeStackResources
    -- ** Request constructor
    , describeStackResources
    -- ** Request lenses
    , dsrrqLogicalResourceId
    , dsrrqPhysicalResourceId
    , dsrrqStackName

    -- * Response
    , DescribeStackResourcesResponse
    -- ** Response constructor
    , describeStackResourcesResponse
    -- ** Response lenses
    , dsrrsStackResources
    , dsrrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStackResources action.
--
-- /See:/ 'describeStackResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrrqLogicalResourceId'
--
-- * 'dsrrqPhysicalResourceId'
--
-- * 'dsrrqStackName'
data DescribeStackResources = DescribeStackResources'
    { _dsrrqLogicalResourceId  :: !(Maybe Text)
    , _dsrrqPhysicalResourceId :: !(Maybe Text)
    , _dsrrqStackName          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackResources' smart constructor.
describeStackResources :: DescribeStackResources
describeStackResources =
    DescribeStackResources'
    { _dsrrqLogicalResourceId = Nothing
    , _dsrrqPhysicalResourceId = Nothing
    , _dsrrqStackName = Nothing
    }

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
dsrrqLogicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrrqLogicalResourceId = lens _dsrrqLogicalResourceId (\ s a -> s{_dsrrqLogicalResourceId = a});

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance,
-- @PhysicalResourceId@ corresponds to the @InstanceId@. You can pass the
-- EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the
-- instance belongs to and what other resources are part of the stack.
--
-- Required: Conditional. If you do not specify @PhysicalResourceId@, you
-- must specify @StackName@.
--
-- Default: There is no default value.
dsrrqPhysicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrrqPhysicalResourceId = lens _dsrrqPhysicalResourceId (\ s a -> s{_dsrrqPhysicalResourceId = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- Required: Conditional. If you do not specify @StackName@, you must
-- specify @PhysicalResourceId@.
dsrrqStackName :: Lens' DescribeStackResources (Maybe Text)
dsrrqStackName = lens _dsrrqStackName (\ s a -> s{_dsrrqStackName = a});

instance AWSRequest DescribeStackResources where
        type Sv DescribeStackResources = CloudFormation
        type Rs DescribeStackResources =
             DescribeStackResourcesResponse
        request = post
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
               "LogicalResourceId" =: _dsrrqLogicalResourceId,
               "PhysicalResourceId" =: _dsrrqPhysicalResourceId,
               "StackName" =: _dsrrqStackName]

-- | The output for a DescribeStackResources action.
--
-- /See:/ 'describeStackResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrrsStackResources'
--
-- * 'dsrrsStatus'
data DescribeStackResourcesResponse = DescribeStackResourcesResponse'
    { _dsrrsStackResources :: !(Maybe [StackResource])
    , _dsrrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackResourcesResponse' smart constructor.
describeStackResourcesResponse :: Int -> DescribeStackResourcesResponse
describeStackResourcesResponse pStatus =
    DescribeStackResourcesResponse'
    { _dsrrsStackResources = Nothing
    , _dsrrsStatus = pStatus
    }

-- | A list of @StackResource@ structures.
dsrrsStackResources :: Lens' DescribeStackResourcesResponse [StackResource]
dsrrsStackResources = lens _dsrrsStackResources (\ s a -> s{_dsrrsStackResources = a}) . _Default;

-- | FIXME: Undocumented member.
dsrrsStatus :: Lens' DescribeStackResourcesResponse Int
dsrrsStatus = lens _dsrrsStatus (\ s a -> s{_dsrrsStatus = a});
