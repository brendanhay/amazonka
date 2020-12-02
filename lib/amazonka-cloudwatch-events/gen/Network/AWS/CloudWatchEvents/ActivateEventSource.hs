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
-- Module      : Network.AWS.CloudWatchEvents.ActivateEventSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a partner event source that has been deactivated. Once activated, your matching event bus will start receiving events from the event source.
--
--
module Network.AWS.CloudWatchEvents.ActivateEventSource
    (
    -- * Creating a Request
      activateEventSource
    , ActivateEventSource
    -- * Request Lenses
    , aesName

    -- * Destructuring the Response
    , activateEventSourceResponse
    , ActivateEventSourceResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'activateEventSource' smart constructor.
newtype ActivateEventSource = ActivateEventSource'
  { _aesName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivateEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aesName' - The name of the partner event source to activate.
activateEventSource
    :: Text -- ^ 'aesName'
    -> ActivateEventSource
activateEventSource pName_ = ActivateEventSource' {_aesName = pName_}


-- | The name of the partner event source to activate.
aesName :: Lens' ActivateEventSource Text
aesName = lens _aesName (\ s a -> s{_aesName = a})

instance AWSRequest ActivateEventSource where
        type Rs ActivateEventSource =
             ActivateEventSourceResponse
        request = postJSON cloudWatchEvents
        response = receiveNull ActivateEventSourceResponse'

instance Hashable ActivateEventSource where

instance NFData ActivateEventSource where

instance ToHeaders ActivateEventSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ActivateEventSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ActivateEventSource where
        toJSON ActivateEventSource'{..}
          = object (catMaybes [Just ("Name" .= _aesName)])

instance ToPath ActivateEventSource where
        toPath = const "/"

instance ToQuery ActivateEventSource where
        toQuery = const mempty

-- | /See:/ 'activateEventSourceResponse' smart constructor.
data ActivateEventSourceResponse =
  ActivateEventSourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivateEventSourceResponse' with the minimum fields required to make a request.
--
activateEventSourceResponse
    :: ActivateEventSourceResponse
activateEventSourceResponse = ActivateEventSourceResponse'


instance NFData ActivateEventSourceResponse where
