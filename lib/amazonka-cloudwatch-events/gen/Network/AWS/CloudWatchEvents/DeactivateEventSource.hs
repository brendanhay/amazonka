{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to temporarily stop receiving events from the specified partner event source. The matching event bus is not deleted.
--
--
-- When you deactivate a partner event source, the source goes into PENDING state. If it remains in PENDING state for more than two weeks, it is deleted.
--
-- To activate a deactivated partner event source, use 'ActivateEventSource' .
module Network.AWS.CloudWatchEvents.DeactivateEventSource
  ( -- * Creating a Request
    deactivateEventSource,
    DeactivateEventSource,

    -- * Request Lenses
    deaName,

    -- * Destructuring the Response
    deactivateEventSourceResponse,
    DeactivateEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deactivateEventSource' smart constructor.
newtype DeactivateEventSource = DeactivateEventSource'
  { _deaName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeactivateEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deaName' - The name of the partner event source to deactivate.
deactivateEventSource ::
  -- | 'deaName'
  Text ->
  DeactivateEventSource
deactivateEventSource pName_ =
  DeactivateEventSource' {_deaName = pName_}

-- | The name of the partner event source to deactivate.
deaName :: Lens' DeactivateEventSource Text
deaName = lens _deaName (\s a -> s {_deaName = a})

instance AWSRequest DeactivateEventSource where
  type Rs DeactivateEventSource = DeactivateEventSourceResponse
  request = postJSON cloudWatchEvents
  response = receiveNull DeactivateEventSourceResponse'

instance Hashable DeactivateEventSource

instance NFData DeactivateEventSource

instance ToHeaders DeactivateEventSource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSEvents.DeactivateEventSource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeactivateEventSource where
  toJSON DeactivateEventSource' {..} =
    object (catMaybes [Just ("Name" .= _deaName)])

instance ToPath DeactivateEventSource where
  toPath = const "/"

instance ToQuery DeactivateEventSource where
  toQuery = const mempty

-- | /See:/ 'deactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeactivateEventSourceResponse' with the minimum fields required to make a request.
deactivateEventSourceResponse ::
  DeactivateEventSourceResponse
deactivateEventSourceResponse = DeactivateEventSourceResponse'

instance NFData DeactivateEventSourceResponse
