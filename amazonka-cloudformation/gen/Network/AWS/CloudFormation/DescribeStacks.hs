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
-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStacks.html AWS API Reference> for DescribeStacks.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStacks
    (
    -- * Creating a Request
      describeStacks
    , DescribeStacks
    -- * Request Lenses
    , dNextToken
    , dStackName

    -- * Destructuring the Response
    , describeStacksResponse
    , DescribeStacksResponse
    -- * Response Lenses
    , dsrsNextToken
    , dsrsStacks
    , dsrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStacks action.
--
-- /See:/ 'describeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
    { _dNextToken :: !(Maybe Text)
    , _dStackName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken'
--
-- * 'dStackName'
describeStacks
    :: DescribeStacks
describeStacks =
    DescribeStacks'
    { _dNextToken = Nothing
    , _dStackName = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dNextToken :: Lens' DescribeStacks (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
dStackName :: Lens' DescribeStacks (Maybe Text)
dStackName = lens _dStackName (\ s a -> s{_dStackName = a});

instance AWSPager DescribeStacks where
        page rq rs
          | stop (rs ^. dsrsNextToken) = Nothing
          | stop (rs ^. dsrsStacks) = Nothing
          | otherwise =
            Just $ rq & dNextToken .~ rs ^. dsrsNextToken

instance AWSRequest DescribeStacks where
        type Rs DescribeStacks = DescribeStacksResponse
        request = postQuery cloudFormation
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
               "NextToken" =: _dNextToken,
               "StackName" =: _dStackName]

-- | The output for a DescribeStacks action.
--
-- /See:/ 'describeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
    { _dsrsNextToken :: !(Maybe Text)
    , _dsrsStacks    :: !(Maybe [Stack])
    , _dsrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsNextToken'
--
-- * 'dsrsStacks'
--
-- * 'dsrsStatus'
describeStacksResponse
    :: Int -- ^ 'dsrsStatus'
    -> DescribeStacksResponse
describeStacksResponse pStatus_ =
    DescribeStacksResponse'
    { _dsrsNextToken = Nothing
    , _dsrsStacks = Nothing
    , _dsrsStatus = pStatus_
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsrsNextToken :: Lens' DescribeStacksResponse (Maybe Text)
dsrsNextToken = lens _dsrsNextToken (\ s a -> s{_dsrsNextToken = a});

-- | A list of stack structures.
dsrsStacks :: Lens' DescribeStacksResponse [Stack]
dsrsStacks = lens _dsrsStacks (\ s a -> s{_dsrsStacks = a}) . _Default . _Coerce;

-- | The response status code.
dsrsStatus :: Lens' DescribeStacksResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
