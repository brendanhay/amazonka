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
-- Module      : Network.AWS.CloudWatchEvents.DeleteEventBus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified custom event bus or partner event bus. All rules associated with this event bus are also deleted. You can't delete your account's default event bus.
--
--
module Network.AWS.CloudWatchEvents.DeleteEventBus
    (
    -- * Creating a Request
      deleteEventBus
    , DeleteEventBus
    -- * Request Lenses
    , debsName

    -- * Destructuring the Response
    , deleteEventBusResponse
    , DeleteEventBusResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEventBus' smart constructor.
newtype DeleteEventBus = DeleteEventBus'
  { _debsName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventBus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'debsName' - The name of the event bus to delete.
deleteEventBus
    :: Text -- ^ 'debsName'
    -> DeleteEventBus
deleteEventBus pName_ = DeleteEventBus' {_debsName = pName_}


-- | The name of the event bus to delete.
debsName :: Lens' DeleteEventBus Text
debsName = lens _debsName (\ s a -> s{_debsName = a})

instance AWSRequest DeleteEventBus where
        type Rs DeleteEventBus = DeleteEventBusResponse
        request = postJSON cloudWatchEvents
        response = receiveNull DeleteEventBusResponse'

instance Hashable DeleteEventBus where

instance NFData DeleteEventBus where

instance ToHeaders DeleteEventBus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DeleteEventBus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEventBus where
        toJSON DeleteEventBus'{..}
          = object (catMaybes [Just ("Name" .= _debsName)])

instance ToPath DeleteEventBus where
        toPath = const "/"

instance ToQuery DeleteEventBus where
        toQuery = const mempty

-- | /See:/ 'deleteEventBusResponse' smart constructor.
data DeleteEventBusResponse =
  DeleteEventBusResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventBusResponse' with the minimum fields required to make a request.
--
deleteEventBusResponse
    :: DeleteEventBusResponse
deleteEventBusResponse = DeleteEventBusResponse'


instance NFData DeleteEventBusResponse where
