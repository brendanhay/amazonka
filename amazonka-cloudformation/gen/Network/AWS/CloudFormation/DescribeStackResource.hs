{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified resource in the specified stack.
--
-- For deleted stacks, DescribeStackResource returns resource information
-- for up to 90 days after the stack has been deleted.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResource.html>
module Network.AWS.CloudFormation.DescribeStackResource
    (
    -- * Request
      DescribeStackResource
    -- ** Request constructor
    , describeStackResource
    -- ** Request lenses
    , desStackName
    , desLogicalResourceId

    -- * Response
    , DescribeStackResourceResponse
    -- ** Response constructor
    , describeStackResourceResponse
    -- ** Response lenses
    , dsrrsStackResourceDetail
    , dsrrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStackResource action.
--
-- /See:/ 'describeStackResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desStackName'
--
-- * 'desLogicalResourceId'
data DescribeStackResource = DescribeStackResource'
    { _desStackName         :: !Text
    , _desLogicalResourceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackResource' smart constructor.
describeStackResource :: Text -> Text -> DescribeStackResource
describeStackResource pStackName_ pLogicalResourceId_ =
    DescribeStackResource'
    { _desStackName = pStackName_
    , _desLogicalResourceId = pLogicalResourceId_
    }

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
desStackName :: Lens' DescribeStackResource Text
desStackName = lens _desStackName (\ s a -> s{_desStackName = a});

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
desLogicalResourceId :: Lens' DescribeStackResource Text
desLogicalResourceId = lens _desLogicalResourceId (\ s a -> s{_desLogicalResourceId = a});

instance AWSRequest DescribeStackResource where
        type Sv DescribeStackResource = CloudFormation
        type Rs DescribeStackResource =
             DescribeStackResourceResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeStackResourceResult"
              (\ s h x ->
                 DescribeStackResourceResponse' <$>
                   (x .@? "StackResourceDetail") <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeStackResource where
        toHeaders = const mempty

instance ToPath DescribeStackResource where
        toPath = const "/"

instance ToQuery DescribeStackResource where
        toQuery DescribeStackResource'{..}
          = mconcat
              ["Action" =: ("DescribeStackResource" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _desStackName,
               "LogicalResourceId" =: _desLogicalResourceId]

-- | The output for a DescribeStackResource action.
--
-- /See:/ 'describeStackResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrrsStackResourceDetail'
--
-- * 'dsrrsStatus'
data DescribeStackResourceResponse = DescribeStackResourceResponse'
    { _dsrrsStackResourceDetail :: !(Maybe StackResourceDetail)
    , _dsrrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackResourceResponse' smart constructor.
describeStackResourceResponse :: Int -> DescribeStackResourceResponse
describeStackResourceResponse pStatus_ =
    DescribeStackResourceResponse'
    { _dsrrsStackResourceDetail = Nothing
    , _dsrrsStatus = pStatus_
    }

-- | A @StackResourceDetail@ structure containing the description of the
-- specified resource in the specified stack.
dsrrsStackResourceDetail :: Lens' DescribeStackResourceResponse (Maybe StackResourceDetail)
dsrrsStackResourceDetail = lens _dsrrsStackResourceDetail (\ s a -> s{_dsrrsStackResourceDetail = a});

-- | FIXME: Undocumented member.
dsrrsStatus :: Lens' DescribeStackResourceResponse Int
dsrrsStatus = lens _dsrrsStatus (\ s a -> s{_dsrrsStatus = a});
