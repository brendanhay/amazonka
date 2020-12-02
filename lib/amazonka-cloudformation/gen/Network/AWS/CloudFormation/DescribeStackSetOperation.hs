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
-- Module      : Network.AWS.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation.
--
--
module Network.AWS.CloudFormation.DescribeStackSetOperation
    (
    -- * Creating a Request
      describeStackSetOperation
    , DescribeStackSetOperation
    -- * Request Lenses
    , dssoStackSetName
    , dssoOperationId

    -- * Destructuring the Response
    , describeStackSetOperationResponse
    , DescribeStackSetOperationResponse
    -- * Response Lenses
    , dssorsStackSetOperation
    , dssorsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackSetOperation' smart constructor.
data DescribeStackSetOperation = DescribeStackSetOperation'
  { _dssoStackSetName :: !Text
  , _dssoOperationId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackSetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssoStackSetName' - The name or the unique stack ID of the stack set for the stack operation.
--
-- * 'dssoOperationId' - The unique ID of the stack set operation.
describeStackSetOperation
    :: Text -- ^ 'dssoStackSetName'
    -> Text -- ^ 'dssoOperationId'
    -> DescribeStackSetOperation
describeStackSetOperation pStackSetName_ pOperationId_ =
  DescribeStackSetOperation'
    {_dssoStackSetName = pStackSetName_, _dssoOperationId = pOperationId_}


-- | The name or the unique stack ID of the stack set for the stack operation.
dssoStackSetName :: Lens' DescribeStackSetOperation Text
dssoStackSetName = lens _dssoStackSetName (\ s a -> s{_dssoStackSetName = a})

-- | The unique ID of the stack set operation.
dssoOperationId :: Lens' DescribeStackSetOperation Text
dssoOperationId = lens _dssoOperationId (\ s a -> s{_dssoOperationId = a})

instance AWSRequest DescribeStackSetOperation where
        type Rs DescribeStackSetOperation =
             DescribeStackSetOperationResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackSetOperationResult"
              (\ s h x ->
                 DescribeStackSetOperationResponse' <$>
                   (x .@? "StackSetOperation") <*> (pure (fromEnum s)))

instance Hashable DescribeStackSetOperation where

instance NFData DescribeStackSetOperation where

instance ToHeaders DescribeStackSetOperation where
        toHeaders = const mempty

instance ToPath DescribeStackSetOperation where
        toPath = const "/"

instance ToQuery DescribeStackSetOperation where
        toQuery DescribeStackSetOperation'{..}
          = mconcat
              ["Action" =:
                 ("DescribeStackSetOperation" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackSetName" =: _dssoStackSetName,
               "OperationId" =: _dssoOperationId]

-- | /See:/ 'describeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { _dssorsStackSetOperation :: !(Maybe StackSetOperation)
  , _dssorsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackSetOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssorsStackSetOperation' - The specified stack set operation.
--
-- * 'dssorsResponseStatus' - -- | The response status code.
describeStackSetOperationResponse
    :: Int -- ^ 'dssorsResponseStatus'
    -> DescribeStackSetOperationResponse
describeStackSetOperationResponse pResponseStatus_ =
  DescribeStackSetOperationResponse'
    { _dssorsStackSetOperation = Nothing
    , _dssorsResponseStatus = pResponseStatus_
    }


-- | The specified stack set operation.
dssorsStackSetOperation :: Lens' DescribeStackSetOperationResponse (Maybe StackSetOperation)
dssorsStackSetOperation = lens _dssorsStackSetOperation (\ s a -> s{_dssorsStackSetOperation = a})

-- | -- | The response status code.
dssorsResponseStatus :: Lens' DescribeStackSetOperationResponse Int
dssorsResponseStatus = lens _dssorsResponseStatus (\ s a -> s{_dssorsResponseStatus = a})

instance NFData DescribeStackSetOperationResponse
         where
