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
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks> in the AWS CloudFormation User Guide.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStackEvents
    (
    -- * Creating a Request
      describeStackEvents
    , DescribeStackEvents
    -- * Request Lenses
    , dseNextToken
    , dseStackName

    -- * Destructuring the Response
    , describeStackEventsResponse
    , DescribeStackEventsResponse
    -- * Response Lenses
    , dsersNextToken
    , dsersStackEvents
    , dsersResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for 'DescribeStackEvents' action.
--
--
--
-- /See:/ 'describeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
  { _dseNextToken :: !(Maybe Text)
  , _dseStackName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseNextToken' - A string that identifies the next page of events that you want to retrieve.
--
-- * 'dseStackName' - The name or the unique stack ID that is associated with the stack, which are not always interchangeable:     * Running stacks: You can specify either the stack's name or its unique stack ID.     * Deleted stacks: You must specify the unique stack ID. Default: There is no default value.
describeStackEvents
    :: DescribeStackEvents
describeStackEvents =
  DescribeStackEvents' {_dseNextToken = Nothing, _dseStackName = Nothing}


-- | A string that identifies the next page of events that you want to retrieve.
dseNextToken :: Lens' DescribeStackEvents (Maybe Text)
dseNextToken = lens _dseNextToken (\ s a -> s{_dseNextToken = a})

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:     * Running stacks: You can specify either the stack's name or its unique stack ID.     * Deleted stacks: You must specify the unique stack ID. Default: There is no default value.
dseStackName :: Lens' DescribeStackEvents (Maybe Text)
dseStackName = lens _dseStackName (\ s a -> s{_dseStackName = a})

instance AWSPager DescribeStackEvents where
        page rq rs
          | stop (rs ^. dsersNextToken) = Nothing
          | stop (rs ^. dsersStackEvents) = Nothing
          | otherwise =
            Just $ rq & dseNextToken .~ rs ^. dsersNextToken

instance AWSRequest DescribeStackEvents where
        type Rs DescribeStackEvents =
             DescribeStackEventsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackEventsResult"
              (\ s h x ->
                 DescribeStackEventsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "StackEvents" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeStackEvents where

instance NFData DescribeStackEvents where

instance ToHeaders DescribeStackEvents where
        toHeaders = const mempty

instance ToPath DescribeStackEvents where
        toPath = const "/"

instance ToQuery DescribeStackEvents where
        toQuery DescribeStackEvents'{..}
          = mconcat
              ["Action" =: ("DescribeStackEvents" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _dseNextToken,
               "StackName" =: _dseStackName]

-- | The output for a 'DescribeStackEvents' action.
--
--
--
-- /See:/ 'describeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
  { _dsersNextToken      :: !(Maybe Text)
  , _dsersStackEvents    :: !(Maybe [StackEvent])
  , _dsersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsersNextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
--
-- * 'dsersStackEvents' - A list of @StackEvents@ structures.
--
-- * 'dsersResponseStatus' - -- | The response status code.
describeStackEventsResponse
    :: Int -- ^ 'dsersResponseStatus'
    -> DescribeStackEventsResponse
describeStackEventsResponse pResponseStatus_ =
  DescribeStackEventsResponse'
    { _dsersNextToken = Nothing
    , _dsersStackEvents = Nothing
    , _dsersResponseStatus = pResponseStatus_
    }


-- | If the output exceeds 1 MB in size, a string that identifies the next page of events. If no additional page exists, this value is null.
dsersNextToken :: Lens' DescribeStackEventsResponse (Maybe Text)
dsersNextToken = lens _dsersNextToken (\ s a -> s{_dsersNextToken = a})

-- | A list of @StackEvents@ structures.
dsersStackEvents :: Lens' DescribeStackEventsResponse [StackEvent]
dsersStackEvents = lens _dsersStackEvents (\ s a -> s{_dsersStackEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
dsersResponseStatus :: Lens' DescribeStackEventsResponse Int
dsersResponseStatus = lens _dsersResponseStatus (\ s a -> s{_dsersResponseStatus = a})

instance NFData DescribeStackEventsResponse where
