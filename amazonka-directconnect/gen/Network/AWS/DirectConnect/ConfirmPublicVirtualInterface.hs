{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Accept ownership of a public virtual interface created by another
-- customer.
--
-- After the virtual interface owner calls this function, the specified
-- virtual interface will be created and made available for handling
-- traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmPublicVirtualInterface.html>
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
    (
    -- * Request
      ConfirmPublicVirtualInterface
    -- ** Request constructor
    , confirmPublicVirtualInterface
    -- ** Request lenses
    , conVirtualInterfaceId

    -- * Response
    , ConfirmPublicVirtualInterfaceResponse
    -- ** Response constructor
    , confirmPublicVirtualInterfaceResponse
    -- ** Response lenses
    , conVirtualInterfaceState
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'confirmPublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conVirtualInterfaceId'
newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'{_conVirtualInterfaceId :: Text} deriving (Eq, Read, Show)

-- | 'ConfirmPublicVirtualInterface' smart constructor.
confirmPublicVirtualInterface :: Text -> ConfirmPublicVirtualInterface
confirmPublicVirtualInterface pVirtualInterfaceId = ConfirmPublicVirtualInterface'{_conVirtualInterfaceId = pVirtualInterfaceId};

-- | FIXME: Undocumented member.
conVirtualInterfaceId :: Lens' ConfirmPublicVirtualInterface Text
conVirtualInterfaceId = lens _conVirtualInterfaceId (\ s a -> s{_conVirtualInterfaceId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ConfirmPublicVirtualInterface
         where
        type Sv ConfirmPublicVirtualInterface = DirectConnect
        type Rs ConfirmPublicVirtualInterface =
             ConfirmPublicVirtualInterfaceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmPublicVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState"))

instance ToHeaders ConfirmPublicVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.ConfirmPublicVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmPublicVirtualInterface where
        toJSON ConfirmPublicVirtualInterface'{..}
          = object
              ["virtualInterfaceId" .= _conVirtualInterfaceId]

instance ToPath ConfirmPublicVirtualInterface where
        toPath = const "/"

instance ToQuery ConfirmPublicVirtualInterface where
        toQuery = const mempty

-- | /See:/ 'confirmPublicVirtualInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conVirtualInterfaceState'
newtype ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'{_conVirtualInterfaceState :: Maybe VirtualInterfaceState} deriving (Eq, Read, Show)

-- | 'ConfirmPublicVirtualInterfaceResponse' smart constructor.
confirmPublicVirtualInterfaceResponse :: ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'{_conVirtualInterfaceState = Nothing};

-- | FIXME: Undocumented member.
conVirtualInterfaceState :: Lens' ConfirmPublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
conVirtualInterfaceState = lens _conVirtualInterfaceState (\ s a -> s{_conVirtualInterfaceState = a});
