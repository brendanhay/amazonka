{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the automatic tape creation policies for a gateway. If there are no automatic tape creation policies for the gateway, it returns an empty list.
--
--
-- This operation is only supported for tape gateways.
module Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
  ( -- * Creating a Request
    listAutomaticTapeCreationPolicies,
    ListAutomaticTapeCreationPolicies,

    -- * Request Lenses
    latcpGatewayARN,

    -- * Destructuring the Response
    listAutomaticTapeCreationPoliciesResponse,
    ListAutomaticTapeCreationPoliciesResponse,

    -- * Response Lenses
    latcprsAutomaticTapeCreationPolicyInfos,
    latcprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listAutomaticTapeCreationPolicies' smart constructor.
newtype ListAutomaticTapeCreationPolicies = ListAutomaticTapeCreationPolicies'
  { _latcpGatewayARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAutomaticTapeCreationPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latcpGatewayARN' - Undocumented member.
listAutomaticTapeCreationPolicies ::
  ListAutomaticTapeCreationPolicies
listAutomaticTapeCreationPolicies =
  ListAutomaticTapeCreationPolicies' {_latcpGatewayARN = Nothing}

-- | Undocumented member.
latcpGatewayARN :: Lens' ListAutomaticTapeCreationPolicies (Maybe Text)
latcpGatewayARN = lens _latcpGatewayARN (\s a -> s {_latcpGatewayARN = a})

instance AWSRequest ListAutomaticTapeCreationPolicies where
  type
    Rs ListAutomaticTapeCreationPolicies =
      ListAutomaticTapeCreationPoliciesResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          ListAutomaticTapeCreationPoliciesResponse'
            <$> (x .?> "AutomaticTapeCreationPolicyInfos" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAutomaticTapeCreationPolicies

instance NFData ListAutomaticTapeCreationPolicies

instance ToHeaders ListAutomaticTapeCreationPolicies where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.ListAutomaticTapeCreationPolicies" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAutomaticTapeCreationPolicies where
  toJSON ListAutomaticTapeCreationPolicies' {..} =
    object (catMaybes [("GatewayARN" .=) <$> _latcpGatewayARN])

instance ToPath ListAutomaticTapeCreationPolicies where
  toPath = const "/"

instance ToQuery ListAutomaticTapeCreationPolicies where
  toQuery = const mempty

-- | /See:/ 'listAutomaticTapeCreationPoliciesResponse' smart constructor.
data ListAutomaticTapeCreationPoliciesResponse = ListAutomaticTapeCreationPoliciesResponse'
  { _latcprsAutomaticTapeCreationPolicyInfos ::
      !( Maybe
           [AutomaticTapeCreationPolicyInfo]
       ),
    _latcprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListAutomaticTapeCreationPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latcprsAutomaticTapeCreationPolicyInfos' - Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
-- * 'latcprsResponseStatus' - -- | The response status code.
listAutomaticTapeCreationPoliciesResponse ::
  -- | 'latcprsResponseStatus'
  Int ->
  ListAutomaticTapeCreationPoliciesResponse
listAutomaticTapeCreationPoliciesResponse pResponseStatus_ =
  ListAutomaticTapeCreationPoliciesResponse'
    { _latcprsAutomaticTapeCreationPolicyInfos =
        Nothing,
      _latcprsResponseStatus = pResponseStatus_
    }

-- | Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
latcprsAutomaticTapeCreationPolicyInfos :: Lens' ListAutomaticTapeCreationPoliciesResponse [AutomaticTapeCreationPolicyInfo]
latcprsAutomaticTapeCreationPolicyInfos = lens _latcprsAutomaticTapeCreationPolicyInfos (\s a -> s {_latcprsAutomaticTapeCreationPolicyInfos = a}) . _Default . _Coerce

-- | -- | The response status code.
latcprsResponseStatus :: Lens' ListAutomaticTapeCreationPoliciesResponse Int
latcprsResponseStatus = lens _latcprsResponseStatus (\s a -> s {_latcprsResponseStatus = a})

instance NFData ListAutomaticTapeCreationPoliciesResponse
