{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , desNextToken
    , desStackName

    -- * Response
    , DescribeStacksResponse
    -- ** Response constructor
    , describeStacksResponse
    -- ** Response lenses
    , dsrNextToken
    , dsrStacks
    , dsrStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStacks action.
--
-- /See:/ 'describeStacks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desNextToken'
--
-- * 'desStackName'
data DescribeStacks = DescribeStacks'
    { _desNextToken :: !(Maybe Text)
    , _desStackName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStacks' smart constructor.
describeStacks :: DescribeStacks
describeStacks =
    DescribeStacks'
    { _desNextToken = Nothing
    , _desStackName = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
desNextToken :: Lens' DescribeStacks (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

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

instance AWSPager DescribeStacks where
        page rq rs
          | stop (rs ^. dsrNextToken) = Nothing
          | stop (rs ^. dsrStacks) = Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dsrNextToken

instance AWSRequest DescribeStacks where
        type Sv DescribeStacks = CloudFormation
        type Rs DescribeStacks = DescribeStacksResponse
        request = post
        response
          = receiveXMLWrapper "DescribeStacksResult"
              (\ s h x ->
                 DescribeStacksResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Stacks" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeStacks where
        toHeaders = const mempty

instance ToPath DescribeStacks where
        toPath = const "/"

instance ToQuery DescribeStacks where
        toQuery DescribeStacks'{..}
          = mconcat
              ["Action" =: ("DescribeStacks" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _desNextToken,
               "StackName" =: _desStackName]

-- | The output for a DescribeStacks action.
--
-- /See:/ 'describeStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrNextToken'
--
-- * 'dsrStacks'
--
-- * 'dsrStatus'
data DescribeStacksResponse = DescribeStacksResponse'
    { _dsrNextToken :: !(Maybe Text)
    , _dsrStacks    :: !(Maybe [Stack])
    , _dsrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStacksResponse' smart constructor.
describeStacksResponse :: Int -> DescribeStacksResponse
describeStacksResponse pStatus =
    DescribeStacksResponse'
    { _dsrNextToken = Nothing
    , _dsrStacks = Nothing
    , _dsrStatus = pStatus
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsrNextToken :: Lens' DescribeStacksResponse (Maybe Text)
dsrNextToken = lens _dsrNextToken (\ s a -> s{_dsrNextToken = a});

-- | A list of stack structures.
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\ s a -> s{_dsrStacks = a}) . _Default;

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DescribeStacksResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});
