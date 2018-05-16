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
-- Module      : Network.AWS.CloudFormation.DescribeStackSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set.
--
--
module Network.AWS.CloudFormation.DescribeStackSet
    (
    -- * Creating a Request
      describeStackSet
    , DescribeStackSet
    -- * Request Lenses
    , desStackSetName

    -- * Destructuring the Response
    , describeStackSetResponse
    , DescribeStackSetResponse
    -- * Response Lenses
    , drsStackSet
    , drsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackSet' smart constructor.
newtype DescribeStackSet = DescribeStackSet'
  { _desStackSetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desStackSetName' - The name or unique ID of the stack set whose description you want.
describeStackSet
    :: Text -- ^ 'desStackSetName'
    -> DescribeStackSet
describeStackSet pStackSetName_ =
  DescribeStackSet' {_desStackSetName = pStackSetName_}


-- | The name or unique ID of the stack set whose description you want.
desStackSetName :: Lens' DescribeStackSet Text
desStackSetName = lens _desStackSetName (\ s a -> s{_desStackSetName = a})

instance AWSRequest DescribeStackSet where
        type Rs DescribeStackSet = DescribeStackSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackSetResult"
              (\ s h x ->
                 DescribeStackSetResponse' <$>
                   (x .@? "StackSet") <*> (pure (fromEnum s)))

instance Hashable DescribeStackSet where

instance NFData DescribeStackSet where

instance ToHeaders DescribeStackSet where
        toHeaders = const mempty

instance ToPath DescribeStackSet where
        toPath = const "/"

instance ToQuery DescribeStackSet where
        toQuery DescribeStackSet'{..}
          = mconcat
              ["Action" =: ("DescribeStackSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackSetName" =: _desStackSetName]

-- | /See:/ 'describeStackSetResponse' smart constructor.
data DescribeStackSetResponse = DescribeStackSetResponse'
  { _drsStackSet       :: !(Maybe StackSet)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsStackSet' - The specified stack set.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeStackSetResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeStackSetResponse
describeStackSetResponse pResponseStatus_ =
  DescribeStackSetResponse'
    {_drsStackSet = Nothing, _drsResponseStatus = pResponseStatus_}


-- | The specified stack set.
drsStackSet :: Lens' DescribeStackSetResponse (Maybe StackSet)
drsStackSet = lens _drsStackSet (\ s a -> s{_drsStackSet = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeStackSetResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeStackSetResponse where
