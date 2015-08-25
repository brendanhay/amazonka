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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteVirtualInterface.html AWS API Reference> for DeleteVirtualInterface.
module Network.AWS.DirectConnect.DeleteVirtualInterface
    (
    -- * Creating a Request
      deleteVirtualInterface
    , DeleteVirtualInterface
    -- * Request Lenses
    , dVirtualInterfaceId

    -- * Destructuring the Response
    , deleteVirtualInterfaceResponse
    , DeleteVirtualInterfaceResponse
    -- * Response Lenses
    , dvirsVirtualInterfaceState
    , dvirsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DeleteVirtualInterface operation.
--
-- /See:/ 'deleteVirtualInterface' smart constructor.
newtype DeleteVirtualInterface = DeleteVirtualInterface'
    { _dVirtualInterfaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVirtualInterfaceId'
deleteVirtualInterface
    :: Text -- ^ 'dVirtualInterfaceId'
    -> DeleteVirtualInterface
deleteVirtualInterface pVirtualInterfaceId_ =
    DeleteVirtualInterface'
    { _dVirtualInterfaceId = pVirtualInterfaceId_
    }

-- | Undocumented member.
dVirtualInterfaceId :: Lens' DeleteVirtualInterface Text
dVirtualInterfaceId = lens _dVirtualInterfaceId (\ s a -> s{_dVirtualInterfaceId = a});

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
                    ("virtualInterfaceId" .= _dVirtualInterfaceId)])

instance ToPath DeleteVirtualInterface where
        toPath = const "/"

instance ToQuery DeleteVirtualInterface where
        toQuery = const mempty

-- | The response received when DeleteVirtualInterface is called.
--
-- /See:/ 'deleteVirtualInterfaceResponse' smart constructor.
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
    { _dvirsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _dvirsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvirsVirtualInterfaceState'
--
-- * 'dvirsStatus'
deleteVirtualInterfaceResponse
    :: Int -- ^ 'dvirsStatus'
    -> DeleteVirtualInterfaceResponse
deleteVirtualInterfaceResponse pStatus_ =
    DeleteVirtualInterfaceResponse'
    { _dvirsVirtualInterfaceState = Nothing
    , _dvirsStatus = pStatus_
    }

-- | Undocumented member.
dvirsVirtualInterfaceState :: Lens' DeleteVirtualInterfaceResponse (Maybe VirtualInterfaceState)
dvirsVirtualInterfaceState = lens _dvirsVirtualInterfaceState (\ s a -> s{_dvirsVirtualInterfaceState = a});

-- | The response status code.
dvirsStatus :: Lens' DeleteVirtualInterfaceResponse Int
dvirsStatus = lens _dvirsStatus (\ s a -> s{_dvirsStatus = a});
