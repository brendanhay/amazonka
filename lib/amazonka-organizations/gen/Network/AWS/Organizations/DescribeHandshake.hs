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
-- Module      : Network.AWS.Organizations.DescribeHandshake
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original 'InviteAccountToOrganization' operation that generated the handshake.
--
--
-- You can access handshakes that are ACCEPTED, DECLINED, or CANCELED for only 30 days after they change to that state. They are then deleted and no longer accessible.
--
-- This operation can be called from any account in the organization.
--
module Network.AWS.Organizations.DescribeHandshake
    (
    -- * Creating a Request
      describeHandshake
    , DescribeHandshake
    -- * Request Lenses
    , dhHandshakeId

    -- * Destructuring the Response
    , describeHandshakeResponse
    , DescribeHandshakeResponse
    -- * Response Lenses
    , dhrsHandshake
    , dhrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHandshake' smart constructor.
newtype DescribeHandshake = DescribeHandshake'
  { _dhHandshakeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHandshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhHandshakeId' - The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
describeHandshake
    :: Text -- ^ 'dhHandshakeId'
    -> DescribeHandshake
describeHandshake pHandshakeId_ =
  DescribeHandshake' {_dhHandshakeId = pHandshakeId_}


-- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
dhHandshakeId :: Lens' DescribeHandshake Text
dhHandshakeId = lens _dhHandshakeId (\ s a -> s{_dhHandshakeId = a})

instance AWSRequest DescribeHandshake where
        type Rs DescribeHandshake = DescribeHandshakeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHandshakeResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable DescribeHandshake where

instance NFData DescribeHandshake where

instance ToHeaders DescribeHandshake where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribeHandshake" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHandshake where
        toJSON DescribeHandshake'{..}
          = object
              (catMaybes [Just ("HandshakeId" .= _dhHandshakeId)])

instance ToPath DescribeHandshake where
        toPath = const "/"

instance ToQuery DescribeHandshake where
        toQuery = const mempty

-- | /See:/ 'describeHandshakeResponse' smart constructor.
data DescribeHandshakeResponse = DescribeHandshakeResponse'
  { _dhrsHandshake      :: !(Maybe Handshake)
  , _dhrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHandshakeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrsHandshake' - A structure that contains information about the specified handshake.
--
-- * 'dhrsResponseStatus' - -- | The response status code.
describeHandshakeResponse
    :: Int -- ^ 'dhrsResponseStatus'
    -> DescribeHandshakeResponse
describeHandshakeResponse pResponseStatus_ =
  DescribeHandshakeResponse'
    {_dhrsHandshake = Nothing, _dhrsResponseStatus = pResponseStatus_}


-- | A structure that contains information about the specified handshake.
dhrsHandshake :: Lens' DescribeHandshakeResponse (Maybe Handshake)
dhrsHandshake = lens _dhrsHandshake (\ s a -> s{_dhrsHandshake = a})

-- | -- | The response status code.
dhrsResponseStatus :: Lens' DescribeHandshakeResponse Int
dhrsResponseStatus = lens _dhrsResponseStatus (\ s a -> s{_dhrsResponseStatus = a})

instance NFData DescribeHandshakeResponse where
