{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.DescribeStacks
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

-- | Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStacks.html>
module Network.AWS.CloudFormation.DescribeStacks
    (
    -- * Request
      DescribeStacks
    -- ** Request constructor
    , describeStacks
    -- ** Request lenses
    , desStackName
    , desNextToken

    -- * Response
    , DescribeStacksResponse
    -- ** Response constructor
    , describeStacksResponse
    -- ** Response lenses
    , dsrStacks
    , dsrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFormation.Types

-- | /See:/ 'describeStacks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desStackName'
--
-- * 'desNextToken'
data DescribeStacks = DescribeStacks'{_desStackName :: Maybe Text, _desNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeStacks' smart constructor.
describeStacks :: Text -> DescribeStacks
describeStacks pNextToken = DescribeStacks'{_desStackName = Nothing, _desNextToken = pNextToken};

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
desStackName :: Lens' DescribeStacks (Maybe Text)
desStackName = lens _desStackName (\ s a -> s{_desStackName = a});

-- | String that identifies the start of the next list of stacks, if there is
-- one.
desNextToken :: Lens' DescribeStacks Text
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

instance AWSRequest DescribeStacks where
        type Sv DescribeStacks = CloudFormation
        type Rs DescribeStacks = DescribeStacksResponse
        request = post
        response
          = receiveXMLWrapper "DescribeStacksResult"
              (\ s h x ->
                 DescribeStacksResponse' <$>
                   (x .@? "Stacks" .!@ mempty >>= parseXMLList "member")
                     <*> x .@ "NextToken")

instance ToHeaders DescribeStacks where
        toHeaders = const mempty

instance ToPath DescribeStacks where
        toPath = const "/"

instance ToQuery DescribeStacks where
        toQuery DescribeStacks'{..}
          = mconcat
              ["Action" =: ("DescribeStacks" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _desStackName,
               "NextToken" =: _desNextToken]

-- | /See:/ 'describeStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStacks'
--
-- * 'dsrNextToken'
data DescribeStacksResponse = DescribeStacksResponse'{_dsrStacks :: [Stack], _dsrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeStacksResponse' smart constructor.
describeStacksResponse :: Text -> DescribeStacksResponse
describeStacksResponse pNextToken = DescribeStacksResponse'{_dsrStacks = mempty, _dsrNextToken = pNextToken};

-- | A list of stack structures.
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\ s a -> s{_dsrStacks = a});

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsrNextToken :: Lens' DescribeStacksResponse Text
dsrNextToken = lens _dsrNextToken (\ s a -> s{_dsrNextToken = a});
