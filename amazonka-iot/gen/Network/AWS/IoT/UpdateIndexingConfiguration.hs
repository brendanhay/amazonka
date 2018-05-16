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
-- Module      : Network.AWS.IoT.UpdateIndexingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the search configuration.
--
--
module Network.AWS.IoT.UpdateIndexingConfiguration
    (
    -- * Creating a Request
      updateIndexingConfiguration
    , UpdateIndexingConfiguration
    -- * Request Lenses
    , uicThingIndexingConfiguration

    -- * Destructuring the Response
    , updateIndexingConfigurationResponse
    , UpdateIndexingConfigurationResponse
    -- * Response Lenses
    , uicrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateIndexingConfiguration' smart constructor.
newtype UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { _uicThingIndexingConfiguration :: Maybe ThingIndexingConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIndexingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicThingIndexingConfiguration' - Thing indexing configuration.
updateIndexingConfiguration
    :: UpdateIndexingConfiguration
updateIndexingConfiguration =
  UpdateIndexingConfiguration' {_uicThingIndexingConfiguration = Nothing}


-- | Thing indexing configuration.
uicThingIndexingConfiguration :: Lens' UpdateIndexingConfiguration (Maybe ThingIndexingConfiguration)
uicThingIndexingConfiguration = lens _uicThingIndexingConfiguration (\ s a -> s{_uicThingIndexingConfiguration = a})

instance AWSRequest UpdateIndexingConfiguration where
        type Rs UpdateIndexingConfiguration =
             UpdateIndexingConfigurationResponse
        request = postJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateIndexingConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateIndexingConfiguration where

instance NFData UpdateIndexingConfiguration where

instance ToHeaders UpdateIndexingConfiguration where
        toHeaders = const mempty

instance ToJSON UpdateIndexingConfiguration where
        toJSON UpdateIndexingConfiguration'{..}
          = object
              (catMaybes
                 [("thingIndexingConfiguration" .=) <$>
                    _uicThingIndexingConfiguration])

instance ToPath UpdateIndexingConfiguration where
        toPath = const "/indexing/config"

instance ToQuery UpdateIndexingConfiguration where
        toQuery = const mempty

-- | /See:/ 'updateIndexingConfigurationResponse' smart constructor.
newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { _uicrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIndexingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicrsResponseStatus' - -- | The response status code.
updateIndexingConfigurationResponse
    :: Int -- ^ 'uicrsResponseStatus'
    -> UpdateIndexingConfigurationResponse
updateIndexingConfigurationResponse pResponseStatus_ =
  UpdateIndexingConfigurationResponse' {_uicrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uicrsResponseStatus :: Lens' UpdateIndexingConfigurationResponse Int
uicrsResponseStatus = lens _uicrsResponseStatus (\ s a -> s{_uicrsResponseStatus = a})

instance NFData UpdateIndexingConfigurationResponse
         where
