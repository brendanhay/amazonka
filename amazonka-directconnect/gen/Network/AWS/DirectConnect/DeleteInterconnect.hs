{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
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

-- | Deletes the specified interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteInterconnect.html>
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Request
      DeleteInterconnect
    -- ** Request constructor
    , deleteInterconnect
    -- ** Request lenses
    , delInterconnectId

    -- * Response
    , DeleteInterconnectResponse
    -- ** Response constructor
    , deleteInterconnectResponse
    -- ** Response lenses
    , delInterconnectState
    , delStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DeleteInterconnect operation.
--
-- /See:/ 'deleteInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delInterconnectId'
newtype DeleteInterconnect = DeleteInterconnect'
    { _delInterconnectId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteInterconnect' smart constructor.
deleteInterconnect :: Text -> DeleteInterconnect
deleteInterconnect pInterconnectId =
    DeleteInterconnect'
    { _delInterconnectId = pInterconnectId
    }

-- | FIXME: Undocumented member.
delInterconnectId :: Lens' DeleteInterconnect Text
delInterconnectId = lens _delInterconnectId (\ s a -> s{_delInterconnectId = a});

instance AWSRequest DeleteInterconnect where
        type Sv DeleteInterconnect = DirectConnect
        type Rs DeleteInterconnect =
             DeleteInterconnectResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteInterconnectResponse' <$>
                   (x .?> "interconnectState") <*> (pure (fromEnum s)))

instance ToHeaders DeleteInterconnect where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteInterconnect" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInterconnect where
        toJSON DeleteInterconnect'{..}
          = object ["interconnectId" .= _delInterconnectId]

instance ToPath DeleteInterconnect where
        toPath = const "/"

instance ToQuery DeleteInterconnect where
        toQuery = const mempty

-- | The response received when DeleteInterconnect is called.
--
-- /See:/ 'deleteInterconnectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delInterconnectState'
--
-- * 'delStatus'
data DeleteInterconnectResponse = DeleteInterconnectResponse'
    { _delInterconnectState :: Maybe InterconnectState
    , _delStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteInterconnectResponse' smart constructor.
deleteInterconnectResponse :: Int -> DeleteInterconnectResponse
deleteInterconnectResponse pStatus =
    DeleteInterconnectResponse'
    { _delInterconnectState = Nothing
    , _delStatus = pStatus
    }

-- | FIXME: Undocumented member.
delInterconnectState :: Lens' DeleteInterconnectResponse (Maybe InterconnectState)
delInterconnectState = lens _delInterconnectState (\ s a -> s{_delInterconnectState = a});

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteInterconnectResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
