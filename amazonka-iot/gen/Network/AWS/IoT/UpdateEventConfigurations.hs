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
-- Module      : Network.AWS.IoT.UpdateEventConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event configurations.
--
--
module Network.AWS.IoT.UpdateEventConfigurations
    (
    -- * Creating a Request
      updateEventConfigurations
    , UpdateEventConfigurations
    -- * Request Lenses
    , uecEventConfigurations

    -- * Destructuring the Response
    , updateEventConfigurationsResponse
    , UpdateEventConfigurationsResponse
    -- * Response Lenses
    , uecrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEventConfigurations' smart constructor.
newtype UpdateEventConfigurations = UpdateEventConfigurations'
  { _uecEventConfigurations :: Maybe (Map EventType Configuration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEventConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecEventConfigurations' - The new event configuration values.
updateEventConfigurations
    :: UpdateEventConfigurations
updateEventConfigurations =
  UpdateEventConfigurations' {_uecEventConfigurations = Nothing}


-- | The new event configuration values.
uecEventConfigurations :: Lens' UpdateEventConfigurations (HashMap EventType Configuration)
uecEventConfigurations = lens _uecEventConfigurations (\ s a -> s{_uecEventConfigurations = a}) . _Default . _Map

instance AWSRequest UpdateEventConfigurations where
        type Rs UpdateEventConfigurations =
             UpdateEventConfigurationsResponse
        request = patchJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateEventConfigurationsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateEventConfigurations where

instance NFData UpdateEventConfigurations where

instance ToHeaders UpdateEventConfigurations where
        toHeaders = const mempty

instance ToJSON UpdateEventConfigurations where
        toJSON UpdateEventConfigurations'{..}
          = object
              (catMaybes
                 [("eventConfigurations" .=) <$>
                    _uecEventConfigurations])

instance ToPath UpdateEventConfigurations where
        toPath = const "/event-configurations"

instance ToQuery UpdateEventConfigurations where
        toQuery = const mempty

-- | /See:/ 'updateEventConfigurationsResponse' smart constructor.
newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse'
  { _uecrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEventConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecrsResponseStatus' - -- | The response status code.
updateEventConfigurationsResponse
    :: Int -- ^ 'uecrsResponseStatus'
    -> UpdateEventConfigurationsResponse
updateEventConfigurationsResponse pResponseStatus_ =
  UpdateEventConfigurationsResponse' {_uecrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uecrsResponseStatus :: Lens' UpdateEventConfigurationsResponse Int
uecrsResponseStatus = lens _uecrsResponseStatus (\ s a -> s{_uecrsResponseStatus = a})

instance NFData UpdateEventConfigurationsResponse
         where
