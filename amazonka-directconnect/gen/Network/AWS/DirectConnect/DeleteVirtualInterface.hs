{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes a virtual interface.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteVirtualInterface.html>
module Network.AWS.DirectConnect.DeleteVirtualInterface
    (
    -- * Request
      DeleteVirtualInterface
    -- ** Request constructor
    , deleteVirtualInterface
    -- ** Request lenses
    , delVirtualInterfaceId

    -- * Response
    , DeleteVirtualInterfaceResponse
    -- ** Response constructor
    , deleteVirtualInterfaceResponse
    -- ** Response lenses
    , dvirVirtualInterfaceState
    , dvirStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DeleteVirtualInterface operation.
--
-- /See:/ 'deleteVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delVirtualInterfaceId'
newtype DeleteVirtualInterface = DeleteVirtualInterface'
    { _delVirtualInterfaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualInterface' smart constructor.
deleteVirtualInterface :: Text -> DeleteVirtualInterface
deleteVirtualInterface pVirtualInterfaceId =
    DeleteVirtualInterface'
    { _delVirtualInterfaceId = pVirtualInterfaceId
    }

-- | FIXME: Undocumented member.
delVirtualInterfaceId :: Lens' DeleteVirtualInterface Text
delVirtualInterfaceId = lens _delVirtualInterfaceId (\ s a -> s{_delVirtualInterfaceId = a});

instance AWSRequest DeleteVirtualInterface where
        type Sv DeleteVirtualInterface = DirectConnect
        type Rs DeleteVirtualInterface =
             DeleteVirtualInterfaceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState") <*>
                     (pure (fromEnum s)))

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
              ["virtualInterfaceId" .= _delVirtualInterfaceId]

instance ToPath DeleteVirtualInterface where
        toPath = const "/"

instance ToQuery DeleteVirtualInterface where
        toQuery = const mempty

-- | The response received when DeleteVirtualInterface is called.
--
-- /See:/ 'deleteVirtualInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirVirtualInterfaceState'
--
-- * 'dvirStatus'
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
    { _dvirVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _dvirStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualInterfaceResponse' smart constructor.
deleteVirtualInterfaceResponse :: Int -> DeleteVirtualInterfaceResponse
deleteVirtualInterfaceResponse pStatus =
    DeleteVirtualInterfaceResponse'
    { _dvirVirtualInterfaceState = Nothing
    , _dvirStatus = pStatus
    }

-- | FIXME: Undocumented member.
dvirVirtualInterfaceState :: Lens' DeleteVirtualInterfaceResponse (Maybe VirtualInterfaceState)
dvirVirtualInterfaceState = lens _dvirVirtualInterfaceState (\ s a -> s{_dvirVirtualInterfaceState = a});

-- | FIXME: Undocumented member.
dvirStatus :: Lens' DeleteVirtualInterfaceResponse Int
dvirStatus = lens _dvirStatus (\ s a -> s{_dvirStatus = a});
