{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteVirtualInterface.html>
module Network.AWS.DirectConnect.DeleteVirtualInterface
    (
    -- * Request
      DeleteVirtualInterface
    -- ** Request constructor
    , deleteVirtualInterface
    -- ** Request lenses
    , drqVirtualInterfaceId

    -- * Response
    , DeleteVirtualInterfaceResponse
    -- ** Response constructor
    , deleteVirtualInterfaceResponse
    -- ** Response lenses
    , drsVirtualInterfaceState
    , drsStatus
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
-- * 'drqVirtualInterfaceId'
newtype DeleteVirtualInterface = DeleteVirtualInterface'
    { _drqVirtualInterfaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualInterface' smart constructor.
deleteVirtualInterface :: Text -> DeleteVirtualInterface
deleteVirtualInterface pVirtualInterfaceId =
    DeleteVirtualInterface'
    { _drqVirtualInterfaceId = pVirtualInterfaceId
    }

-- | FIXME: Undocumented member.
drqVirtualInterfaceId :: Lens' DeleteVirtualInterface Text
drqVirtualInterfaceId = lens _drqVirtualInterfaceId (\ s a -> s{_drqVirtualInterfaceId = a});

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
              ["virtualInterfaceId" .= _drqVirtualInterfaceId]

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
-- * 'drsVirtualInterfaceState'
--
-- * 'drsStatus'
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
    { _drsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _drsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualInterfaceResponse' smart constructor.
deleteVirtualInterfaceResponse :: Int -> DeleteVirtualInterfaceResponse
deleteVirtualInterfaceResponse pStatus =
    DeleteVirtualInterfaceResponse'
    { _drsVirtualInterfaceState = Nothing
    , _drsStatus = pStatus
    }

-- | FIXME: Undocumented member.
drsVirtualInterfaceState :: Lens' DeleteVirtualInterfaceResponse (Maybe VirtualInterfaceState)
drsVirtualInterfaceState = lens _drsVirtualInterfaceState (\ s a -> s{_drsVirtualInterfaceState = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteVirtualInterfaceResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
