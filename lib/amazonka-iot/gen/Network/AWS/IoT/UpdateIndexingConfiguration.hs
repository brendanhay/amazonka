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
-- Module      : Network.AWS.IoT.UpdateIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the search configuration.
module Network.AWS.IoT.UpdateIndexingConfiguration
  ( -- * Creating a Request
    updateIndexingConfiguration,
    UpdateIndexingConfiguration,

    -- * Request Lenses
    uicThingGroupIndexingConfiguration,
    uicThingIndexingConfiguration,

    -- * Destructuring the Response
    updateIndexingConfigurationResponse,
    UpdateIndexingConfigurationResponse,

    -- * Response Lenses
    uicrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateIndexingConfiguration' smart constructor.
data UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { _uicThingGroupIndexingConfiguration ::
      !( Maybe
           ThingGroupIndexingConfiguration
       ),
    _uicThingIndexingConfiguration ::
      !(Maybe ThingIndexingConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateIndexingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicThingGroupIndexingConfiguration' - Thing group indexing configuration.
--
-- * 'uicThingIndexingConfiguration' - Thing indexing configuration.
updateIndexingConfiguration ::
  UpdateIndexingConfiguration
updateIndexingConfiguration =
  UpdateIndexingConfiguration'
    { _uicThingGroupIndexingConfiguration =
        Nothing,
      _uicThingIndexingConfiguration = Nothing
    }

-- | Thing group indexing configuration.
uicThingGroupIndexingConfiguration :: Lens' UpdateIndexingConfiguration (Maybe ThingGroupIndexingConfiguration)
uicThingGroupIndexingConfiguration = lens _uicThingGroupIndexingConfiguration (\s a -> s {_uicThingGroupIndexingConfiguration = a})

-- | Thing indexing configuration.
uicThingIndexingConfiguration :: Lens' UpdateIndexingConfiguration (Maybe ThingIndexingConfiguration)
uicThingIndexingConfiguration = lens _uicThingIndexingConfiguration (\s a -> s {_uicThingIndexingConfiguration = a})

instance AWSRequest UpdateIndexingConfiguration where
  type
    Rs UpdateIndexingConfiguration =
      UpdateIndexingConfigurationResponse
  request = postJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          UpdateIndexingConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateIndexingConfiguration

instance NFData UpdateIndexingConfiguration

instance ToHeaders UpdateIndexingConfiguration where
  toHeaders = const mempty

instance ToJSON UpdateIndexingConfiguration where
  toJSON UpdateIndexingConfiguration' {..} =
    object
      ( catMaybes
          [ ("thingGroupIndexingConfiguration" .=)
              <$> _uicThingGroupIndexingConfiguration,
            ("thingIndexingConfiguration" .=)
              <$> _uicThingIndexingConfiguration
          ]
      )

instance ToPath UpdateIndexingConfiguration where
  toPath = const "/indexing/config"

instance ToQuery UpdateIndexingConfiguration where
  toQuery = const mempty

-- | /See:/ 'updateIndexingConfigurationResponse' smart constructor.
newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { _uicrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateIndexingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicrsResponseStatus' - -- | The response status code.
updateIndexingConfigurationResponse ::
  -- | 'uicrsResponseStatus'
  Int ->
  UpdateIndexingConfigurationResponse
updateIndexingConfigurationResponse pResponseStatus_ =
  UpdateIndexingConfigurationResponse'
    { _uicrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uicrsResponseStatus :: Lens' UpdateIndexingConfigurationResponse Int
uicrsResponseStatus = lens _uicrsResponseStatus (\s a -> s {_uicrsResponseStatus = a})

instance NFData UpdateIndexingConfigurationResponse
