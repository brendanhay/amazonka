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
-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept ownership of a public virtual interface created by another customer.
--
--
-- After the virtual interface owner calls this function, the specified virtual interface will be created and made available for handling traffic.
--
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
    (
    -- * Creating a Request
      confirmPublicVirtualInterface
    , ConfirmPublicVirtualInterface
    -- * Request Lenses
    , cVirtualInterfaceId

    -- * Destructuring the Response
    , confirmPublicVirtualInterfaceResponse
    , ConfirmPublicVirtualInterfaceResponse
    -- * Response Lenses
    , crsVirtualInterfaceState
    , crsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the ConfirmPublicVirtualInterface operation.
--
--
--
-- /See:/ 'confirmPublicVirtualInterface' smart constructor.
newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'
  { _cVirtualInterfaceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmPublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVirtualInterfaceId' - Undocumented member.
confirmPublicVirtualInterface
    :: Text -- ^ 'cVirtualInterfaceId'
    -> ConfirmPublicVirtualInterface
confirmPublicVirtualInterface pVirtualInterfaceId_ =
  ConfirmPublicVirtualInterface' {_cVirtualInterfaceId = pVirtualInterfaceId_}


-- | Undocumented member.
cVirtualInterfaceId :: Lens' ConfirmPublicVirtualInterface Text
cVirtualInterfaceId = lens _cVirtualInterfaceId (\ s a -> s{_cVirtualInterfaceId = a})

instance AWSRequest ConfirmPublicVirtualInterface
         where
        type Rs ConfirmPublicVirtualInterface =
             ConfirmPublicVirtualInterfaceResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmPublicVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState") <*>
                     (pure (fromEnum s)))

instance Hashable ConfirmPublicVirtualInterface where

instance NFData ConfirmPublicVirtualInterface where

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
              (catMaybes
                 [Just
                    ("virtualInterfaceId" .= _cVirtualInterfaceId)])

instance ToPath ConfirmPublicVirtualInterface where
        toPath = const "/"

instance ToQuery ConfirmPublicVirtualInterface where
        toQuery = const mempty

-- | The response received when ConfirmPublicVirtualInterface is called.
--
--
--
-- /See:/ 'confirmPublicVirtualInterfaceResponse' smart constructor.
data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'
  { _crsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
  , _crsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmPublicVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsVirtualInterfaceState' - Undocumented member.
--
-- * 'crsResponseStatus' - -- | The response status code.
confirmPublicVirtualInterfaceResponse
    :: Int -- ^ 'crsResponseStatus'
    -> ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterfaceResponse pResponseStatus_ =
  ConfirmPublicVirtualInterfaceResponse'
    {_crsVirtualInterfaceState = Nothing, _crsResponseStatus = pResponseStatus_}


-- | Undocumented member.
crsVirtualInterfaceState :: Lens' ConfirmPublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
crsVirtualInterfaceState = lens _crsVirtualInterfaceState (\ s a -> s{_crsVirtualInterfaceState = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' ConfirmPublicVirtualInterfaceResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData ConfirmPublicVirtualInterfaceResponse
         where
