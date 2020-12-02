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
-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
--
--
module Network.AWS.DirectConnect.DeleteVirtualInterface
    (
    -- * Creating a Request
      deleteVirtualInterface
    , DeleteVirtualInterface
    -- * Request Lenses
    , delVirtualInterfaceId

    -- * Destructuring the Response
    , deleteVirtualInterfaceResponse
    , DeleteVirtualInterfaceResponse
    -- * Response Lenses
    , dvirsVirtualInterfaceState
    , dvirsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DeleteVirtualInterface operation.
--
--
--
-- /See:/ 'deleteVirtualInterface' smart constructor.
newtype DeleteVirtualInterface = DeleteVirtualInterface'
  { _delVirtualInterfaceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delVirtualInterfaceId' - Undocumented member.
deleteVirtualInterface
    :: Text -- ^ 'delVirtualInterfaceId'
    -> DeleteVirtualInterface
deleteVirtualInterface pVirtualInterfaceId_ =
  DeleteVirtualInterface' {_delVirtualInterfaceId = pVirtualInterfaceId_}


-- | Undocumented member.
delVirtualInterfaceId :: Lens' DeleteVirtualInterface Text
delVirtualInterfaceId = lens _delVirtualInterfaceId (\ s a -> s{_delVirtualInterfaceId = a})

instance AWSRequest DeleteVirtualInterface where
        type Rs DeleteVirtualInterface =
             DeleteVirtualInterfaceResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteVirtualInterface where

instance NFData DeleteVirtualInterface where

instance ToHeaders DeleteVirtualInterface where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteVirtualInterface where
        toJSON DeleteVirtualInterface'{..}
          = object
              (catMaybes
                 [Just
                    ("virtualInterfaceId" .= _delVirtualInterfaceId)])

instance ToPath DeleteVirtualInterface where
        toPath = const "/"

instance ToQuery DeleteVirtualInterface where
        toQuery = const mempty

-- | The response received when DeleteVirtualInterface is called.
--
--
--
-- /See:/ 'deleteVirtualInterfaceResponse' smart constructor.
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
  { _dvirsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
  , _dvirsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvirsVirtualInterfaceState' - Undocumented member.
--
-- * 'dvirsResponseStatus' - -- | The response status code.
deleteVirtualInterfaceResponse
    :: Int -- ^ 'dvirsResponseStatus'
    -> DeleteVirtualInterfaceResponse
deleteVirtualInterfaceResponse pResponseStatus_ =
  DeleteVirtualInterfaceResponse'
    { _dvirsVirtualInterfaceState = Nothing
    , _dvirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dvirsVirtualInterfaceState :: Lens' DeleteVirtualInterfaceResponse (Maybe VirtualInterfaceState)
dvirsVirtualInterfaceState = lens _dvirsVirtualInterfaceState (\ s a -> s{_dvirsVirtualInterfaceState = a})

-- | -- | The response status code.
dvirsResponseStatus :: Lens' DeleteVirtualInterfaceResponse Int
dvirsResponseStatus = lens _dvirsResponseStatus (\ s a -> s{_dvirsResponseStatus = a})

instance NFData DeleteVirtualInterfaceResponse where
