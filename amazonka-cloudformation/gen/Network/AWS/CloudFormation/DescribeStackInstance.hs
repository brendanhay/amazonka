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
-- Module      : Network.AWS.CloudFormation.DescribeStackInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack instance that's associated with the specified stack set, AWS account, and region.
--
--
-- For a list of stack instances that are associated with a specific stack set, use 'ListStackInstances' .
--
module Network.AWS.CloudFormation.DescribeStackInstance
    (
    -- * Creating a Request
      describeStackInstance
    , DescribeStackInstance
    -- * Request Lenses
    , dStackSetName
    , dStackInstanceAccount
    , dStackInstanceRegion

    -- * Destructuring the Response
    , describeStackInstanceResponse
    , DescribeStackInstanceResponse
    -- * Response Lenses
    , dsisrsStackInstance
    , dsisrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackInstance' smart constructor.
data DescribeStackInstance = DescribeStackInstance'
  { _dStackSetName         :: !Text
  , _dStackInstanceAccount :: !Text
  , _dStackInstanceRegion  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStackSetName' - The name or the unique stack ID of the stack set that you want to get stack instance information for.
--
-- * 'dStackInstanceAccount' - The ID of an AWS account that's associated with this stack instance.
--
-- * 'dStackInstanceRegion' - The name of a region that's associated with this stack instance.
describeStackInstance
    :: Text -- ^ 'dStackSetName'
    -> Text -- ^ 'dStackInstanceAccount'
    -> Text -- ^ 'dStackInstanceRegion'
    -> DescribeStackInstance
describeStackInstance pStackSetName_ pStackInstanceAccount_ pStackInstanceRegion_ =
  DescribeStackInstance'
    { _dStackSetName = pStackSetName_
    , _dStackInstanceAccount = pStackInstanceAccount_
    , _dStackInstanceRegion = pStackInstanceRegion_
    }


-- | The name or the unique stack ID of the stack set that you want to get stack instance information for.
dStackSetName :: Lens' DescribeStackInstance Text
dStackSetName = lens _dStackSetName (\ s a -> s{_dStackSetName = a})

-- | The ID of an AWS account that's associated with this stack instance.
dStackInstanceAccount :: Lens' DescribeStackInstance Text
dStackInstanceAccount = lens _dStackInstanceAccount (\ s a -> s{_dStackInstanceAccount = a})

-- | The name of a region that's associated with this stack instance.
dStackInstanceRegion :: Lens' DescribeStackInstance Text
dStackInstanceRegion = lens _dStackInstanceRegion (\ s a -> s{_dStackInstanceRegion = a})

instance AWSRequest DescribeStackInstance where
        type Rs DescribeStackInstance =
             DescribeStackInstanceResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackInstanceResult"
              (\ s h x ->
                 DescribeStackInstanceResponse' <$>
                   (x .@? "StackInstance") <*> (pure (fromEnum s)))

instance Hashable DescribeStackInstance where

instance NFData DescribeStackInstance where

instance ToHeaders DescribeStackInstance where
        toHeaders = const mempty

instance ToPath DescribeStackInstance where
        toPath = const "/"

instance ToQuery DescribeStackInstance where
        toQuery DescribeStackInstance'{..}
          = mconcat
              ["Action" =: ("DescribeStackInstance" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackSetName" =: _dStackSetName,
               "StackInstanceAccount" =: _dStackInstanceAccount,
               "StackInstanceRegion" =: _dStackInstanceRegion]

-- | /See:/ 'describeStackInstanceResponse' smart constructor.
data DescribeStackInstanceResponse = DescribeStackInstanceResponse'
  { _dsisrsStackInstance  :: !(Maybe StackInstance)
  , _dsisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsisrsStackInstance' - The stack instance that matches the specified request parameters.
--
-- * 'dsisrsResponseStatus' - -- | The response status code.
describeStackInstanceResponse
    :: Int -- ^ 'dsisrsResponseStatus'
    -> DescribeStackInstanceResponse
describeStackInstanceResponse pResponseStatus_ =
  DescribeStackInstanceResponse'
    {_dsisrsStackInstance = Nothing, _dsisrsResponseStatus = pResponseStatus_}


-- | The stack instance that matches the specified request parameters.
dsisrsStackInstance :: Lens' DescribeStackInstanceResponse (Maybe StackInstance)
dsisrsStackInstance = lens _dsisrsStackInstance (\ s a -> s{_dsisrsStackInstance = a})

-- | -- | The response status code.
dsisrsResponseStatus :: Lens' DescribeStackInstanceResponse Int
dsisrsResponseStatus = lens _dsisrsResponseStatus (\ s a -> s{_dsisrsResponseStatus = a})

instance NFData DescribeStackInstanceResponse where
