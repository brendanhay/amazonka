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
-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified interconnect.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteInterconnect.html AWS API Reference> for DeleteInterconnect.
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Creating a Request
      DeleteInterconnect
    , deleteInterconnect
    -- * Request Lenses
    , dInterconnectId

    -- * Destructuring the Response
    , DeleteInterconnectResponse
    , deleteInterconnectResponse
    -- * Response Lenses
    , drsInterconnectState
    , drsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DeleteInterconnect operation.
--
-- /See:/ 'deleteInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dInterconnectId'
newtype DeleteInterconnect = DeleteInterconnect'
    { _dInterconnectId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInterconnect' smart constructor.
deleteInterconnect :: Text -> DeleteInterconnect
deleteInterconnect pInterconnectId_ =
    DeleteInterconnect'
    { _dInterconnectId = pInterconnectId_
    }

-- | Undocumented member.
dInterconnectId :: Lens' DeleteInterconnect Text
dInterconnectId = lens _dInterconnectId (\ s a -> s{_dInterconnectId = a});

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
          = object ["interconnectId" .= _dInterconnectId]

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
-- * 'drsInterconnectState'
--
-- * 'drsStatus'
data DeleteInterconnectResponse = DeleteInterconnectResponse'
    { _drsInterconnectState :: !(Maybe InterconnectState)
    , _drsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInterconnectResponse' smart constructor.
deleteInterconnectResponse :: Int -> DeleteInterconnectResponse
deleteInterconnectResponse pStatus_ =
    DeleteInterconnectResponse'
    { _drsInterconnectState = Nothing
    , _drsStatus = pStatus_
    }

-- | Undocumented member.
drsInterconnectState :: Lens' DeleteInterconnectResponse (Maybe InterconnectState)
drsInterconnectState = lens _drsInterconnectState (\ s a -> s{_drsInterconnectState = a});

-- | Undocumented member.
drsStatus :: Lens' DeleteInterconnectResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
