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
-- Module      : Network.AWS.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more instances.
--
--
module Network.AWS.SSM.DescribeInstancePatchStates
    (
    -- * Creating a Request
      describeInstancePatchStates
    , DescribeInstancePatchStates
    -- * Request Lenses
    , dipsNextToken
    , dipsMaxResults
    , dipsInstanceIds

    -- * Destructuring the Response
    , describeInstancePatchStatesResponse
    , DescribeInstancePatchStatesResponse
    -- * Response Lenses
    , dipsrsNextToken
    , dipsrsInstancePatchStates
    , dipsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { _dipsNextToken   :: !(Maybe Text)
  , _dipsMaxResults  :: !(Maybe Nat)
  , _dipsInstanceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatchStates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dipsMaxResults' - The maximum number of instances to return (per page).
--
-- * 'dipsInstanceIds' - The ID of the instance whose patch state information should be retrieved.
describeInstancePatchStates
    :: DescribeInstancePatchStates
describeInstancePatchStates =
  DescribeInstancePatchStates'
    { _dipsNextToken = Nothing
    , _dipsMaxResults = Nothing
    , _dipsInstanceIds = mempty
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
dipsNextToken :: Lens' DescribeInstancePatchStates (Maybe Text)
dipsNextToken = lens _dipsNextToken (\ s a -> s{_dipsNextToken = a})

-- | The maximum number of instances to return (per page).
dipsMaxResults :: Lens' DescribeInstancePatchStates (Maybe Natural)
dipsMaxResults = lens _dipsMaxResults (\ s a -> s{_dipsMaxResults = a}) . mapping _Nat

-- | The ID of the instance whose patch state information should be retrieved.
dipsInstanceIds :: Lens' DescribeInstancePatchStates [Text]
dipsInstanceIds = lens _dipsInstanceIds (\ s a -> s{_dipsInstanceIds = a}) . _Coerce

instance AWSRequest DescribeInstancePatchStates where
        type Rs DescribeInstancePatchStates =
             DescribeInstancePatchStatesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancePatchStatesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "InstancePatchStates" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstancePatchStates where

instance NFData DescribeInstancePatchStates where

instance ToHeaders DescribeInstancePatchStates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeInstancePatchStates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInstancePatchStates where
        toJSON DescribeInstancePatchStates'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dipsNextToken,
                  ("MaxResults" .=) <$> _dipsMaxResults,
                  Just ("InstanceIds" .= _dipsInstanceIds)])

instance ToPath DescribeInstancePatchStates where
        toPath = const "/"

instance ToQuery DescribeInstancePatchStates where
        toQuery = const mempty

-- | /See:/ 'describeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { _dipsrsNextToken           :: !(Maybe Text)
  , _dipsrsInstancePatchStates :: !(Maybe [InstancePatchState])
  , _dipsrsResponseStatus      :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatchStatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dipsrsInstancePatchStates' - The high-level patch state for the requested instances.
--
-- * 'dipsrsResponseStatus' - -- | The response status code.
describeInstancePatchStatesResponse
    :: Int -- ^ 'dipsrsResponseStatus'
    -> DescribeInstancePatchStatesResponse
describeInstancePatchStatesResponse pResponseStatus_ =
  DescribeInstancePatchStatesResponse'
    { _dipsrsNextToken = Nothing
    , _dipsrsInstancePatchStates = Nothing
    , _dipsrsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dipsrsNextToken :: Lens' DescribeInstancePatchStatesResponse (Maybe Text)
dipsrsNextToken = lens _dipsrsNextToken (\ s a -> s{_dipsrsNextToken = a})

-- | The high-level patch state for the requested instances.
dipsrsInstancePatchStates :: Lens' DescribeInstancePatchStatesResponse [InstancePatchState]
dipsrsInstancePatchStates = lens _dipsrsInstancePatchStates (\ s a -> s{_dipsrsInstancePatchStates = a}) . _Default . _Coerce

-- | -- | The response status code.
dipsrsResponseStatus :: Lens' DescribeInstancePatchStatesResponse Int
dipsrsResponseStatus = lens _dipsrsResponseStatus (\ s a -> s{_dipsrsResponseStatus = a})

instance NFData DescribeInstancePatchStatesResponse
         where
