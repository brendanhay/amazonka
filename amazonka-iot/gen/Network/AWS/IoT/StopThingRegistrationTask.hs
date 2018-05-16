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
-- Module      : Network.AWS.IoT.StopThingRegistrationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bulk thing provisioning task.
--
--
module Network.AWS.IoT.StopThingRegistrationTask
    (
    -- * Creating a Request
      stopThingRegistrationTask
    , StopThingRegistrationTask
    -- * Request Lenses
    , strtTaskId

    -- * Destructuring the Response
    , stopThingRegistrationTaskResponse
    , StopThingRegistrationTaskResponse
    -- * Response Lenses
    , srsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopThingRegistrationTask' smart constructor.
newtype StopThingRegistrationTask = StopThingRegistrationTask'
  { _strtTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopThingRegistrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strtTaskId' - The bulk thing provisioning task ID.
stopThingRegistrationTask
    :: Text -- ^ 'strtTaskId'
    -> StopThingRegistrationTask
stopThingRegistrationTask pTaskId_ =
  StopThingRegistrationTask' {_strtTaskId = pTaskId_}


-- | The bulk thing provisioning task ID.
strtTaskId :: Lens' StopThingRegistrationTask Text
strtTaskId = lens _strtTaskId (\ s a -> s{_strtTaskId = a})

instance AWSRequest StopThingRegistrationTask where
        type Rs StopThingRegistrationTask =
             StopThingRegistrationTaskResponse
        request = putJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 StopThingRegistrationTaskResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StopThingRegistrationTask where

instance NFData StopThingRegistrationTask where

instance ToHeaders StopThingRegistrationTask where
        toHeaders = const mempty

instance ToJSON StopThingRegistrationTask where
        toJSON = const (Object mempty)

instance ToPath StopThingRegistrationTask where
        toPath StopThingRegistrationTask'{..}
          = mconcat
              ["/thing-registration-tasks/", toBS _strtTaskId,
               "/cancel"]

instance ToQuery StopThingRegistrationTask where
        toQuery = const mempty

-- | /See:/ 'stopThingRegistrationTaskResponse' smart constructor.
newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
stopThingRegistrationTaskResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopThingRegistrationTaskResponse
stopThingRegistrationTaskResponse pResponseStatus_ =
  StopThingRegistrationTaskResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StopThingRegistrationTaskResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopThingRegistrationTaskResponse
         where
