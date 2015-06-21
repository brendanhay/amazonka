{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.DescribeStackResource
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

-- | Returns a description of the specified resource in the specified stack.
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
    , dStackName
    , dLogicalResourceId

    -- * Response
    , DescribeStackResourceResponse
    -- ** Response constructor
    , describeStackResourceResponse
    -- ** Response lenses
    , dsrrStackResourceDetail
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dStackName'
--
-- * 'dLogicalResourceId'
data DescribeStackResource = DescribeStackResource'{_dStackName :: Text, _dLogicalResourceId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeStackResource' smart constructor.
describeStackResource :: Text -> Text -> DescribeStackResource
describeStackResource pStackName pLogicalResourceId = DescribeStackResource'{_dStackName = pStackName, _dLogicalResourceId = pLogicalResourceId};

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
dStackName :: Lens' DescribeStackResource Text
dStackName = lens _dStackName (\ s a -> s{_dStackName = a});

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
dLogicalResourceId :: Lens' DescribeStackResource Text
dLogicalResourceId = lens _dLogicalResourceId (\ s a -> s{_dLogicalResourceId = a});

instance AWSRequest DescribeStackResource where
        type Sv DescribeStackResource = CloudFormation
        type Rs DescribeStackResource =
             DescribeStackResourceResponse
        request = post
        response
          = receiveXMLWrapper "DescribeStackResourceResult"
              (\ s h x ->
                 DescribeStackResourceResponse' <$>
                   (x .@? "StackResourceDetail"))

instance ToHeaders DescribeStackResource where
        toHeaders = const mempty

instance ToPath DescribeStackResource where
        toPath = const "/"

instance ToQuery DescribeStackResource where
        toQuery DescribeStackResource'{..}
          = mconcat
              ["Action" =: ("DescribeStackResource" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _dStackName,
               "LogicalResourceId" =: _dLogicalResourceId]

-- | /See:/ 'describeStackResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrrStackResourceDetail'
newtype DescribeStackResourceResponse = DescribeStackResourceResponse'{_dsrrStackResourceDetail :: Maybe StackResourceDetail} deriving (Eq, Read, Show)

-- | 'DescribeStackResourceResponse' smart constructor.
describeStackResourceResponse :: DescribeStackResourceResponse
describeStackResourceResponse = DescribeStackResourceResponse'{_dsrrStackResourceDetail = Nothing};

-- | A @StackResourceDetail@ structure containing the description of the
-- specified resource in the specified stack.
dsrrStackResourceDetail :: Lens' DescribeStackResourceResponse (Maybe StackResourceDetail)
dsrrStackResourceDetail = lens _dsrrStackResourceDetail (\ s a -> s{_dsrrStackResourceDetail = a});
