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
-- Module      : Network.AWS.OpsWorks.DescribeStacks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more stacks.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeStacks
    (
    -- * Creating a Request
      describeStacks
    , DescribeStacks
    -- * Request Lenses
    , dsStackIds

    -- * Destructuring the Response
    , describeStacksResponse
    , DescribeStacksResponse
    -- * Response Lenses
    , dsrsStacks
    , dsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStacks' smart constructor.
newtype DescribeStacks = DescribeStacks'
  { _dsStackIds :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStackIds' - An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
describeStacks
    :: DescribeStacks
describeStacks = DescribeStacks' {_dsStackIds = Nothing}


-- | An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
dsStackIds :: Lens' DescribeStacks [Text]
dsStackIds = lens _dsStackIds (\ s a -> s{_dsStackIds = a}) . _Default . _Coerce

instance AWSRequest DescribeStacks where
        type Rs DescribeStacks = DescribeStacksResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStacksResponse' <$>
                   (x .?> "Stacks" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeStacks where

instance NFData DescribeStacks where

instance ToHeaders DescribeStacks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeStacks" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStacks where
        toJSON DescribeStacks'{..}
          = object
              (catMaybes [("StackIds" .=) <$> _dsStackIds])

instance ToPath DescribeStacks where
        toPath = const "/"

instance ToQuery DescribeStacks where
        toQuery = const mempty

-- | Contains the response to a @DescribeStacks@ request.
--
--
--
-- /See:/ 'describeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { _dsrsStacks         :: !(Maybe [Stack])
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsStacks' - An array of @Stack@ objects that describe the stacks.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeStacksResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeStacksResponse
describeStacksResponse pResponseStatus_ =
  DescribeStacksResponse'
    {_dsrsStacks = Nothing, _dsrsResponseStatus = pResponseStatus_}


-- | An array of @Stack@ objects that describe the stacks.
dsrsStacks :: Lens' DescribeStacksResponse [Stack]
dsrsStacks = lens _dsrsStacks (\ s a -> s{_dsrsStacks = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeStacksResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeStacksResponse where
