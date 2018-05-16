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
-- Module      : Network.AWS.IoT.GetIndexingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the search configuration.
--
--
module Network.AWS.IoT.GetIndexingConfiguration
    (
    -- * Creating a Request
      getIndexingConfiguration
    , GetIndexingConfiguration

    -- * Destructuring the Response
    , getIndexingConfigurationResponse
    , GetIndexingConfigurationResponse
    -- * Response Lenses
    , gicrsThingIndexingConfiguration
    , gicrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIndexingConfiguration' smart constructor.
data GetIndexingConfiguration =
  GetIndexingConfiguration'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIndexingConfiguration' with the minimum fields required to make a request.
--
getIndexingConfiguration
    :: GetIndexingConfiguration
getIndexingConfiguration = GetIndexingConfiguration'


instance AWSRequest GetIndexingConfiguration where
        type Rs GetIndexingConfiguration =
             GetIndexingConfigurationResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetIndexingConfigurationResponse' <$>
                   (x .?> "thingIndexingConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable GetIndexingConfiguration where

instance NFData GetIndexingConfiguration where

instance ToHeaders GetIndexingConfiguration where
        toHeaders = const mempty

instance ToPath GetIndexingConfiguration where
        toPath = const "/indexing/config"

instance ToQuery GetIndexingConfiguration where
        toQuery = const mempty

-- | /See:/ 'getIndexingConfigurationResponse' smart constructor.
data GetIndexingConfigurationResponse = GetIndexingConfigurationResponse'
  { _gicrsThingIndexingConfiguration :: !(Maybe ThingIndexingConfiguration)
  , _gicrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIndexingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gicrsThingIndexingConfiguration' - Thing indexing configuration.
--
-- * 'gicrsResponseStatus' - -- | The response status code.
getIndexingConfigurationResponse
    :: Int -- ^ 'gicrsResponseStatus'
    -> GetIndexingConfigurationResponse
getIndexingConfigurationResponse pResponseStatus_ =
  GetIndexingConfigurationResponse'
    { _gicrsThingIndexingConfiguration = Nothing
    , _gicrsResponseStatus = pResponseStatus_
    }


-- | Thing indexing configuration.
gicrsThingIndexingConfiguration :: Lens' GetIndexingConfigurationResponse (Maybe ThingIndexingConfiguration)
gicrsThingIndexingConfiguration = lens _gicrsThingIndexingConfiguration (\ s a -> s{_gicrsThingIndexingConfiguration = a})

-- | -- | The response status code.
gicrsResponseStatus :: Lens' GetIndexingConfigurationResponse Int
gicrsResponseStatus = lens _gicrsResponseStatus (\ s a -> s{_gicrsResponseStatus = a})

instance NFData GetIndexingConfigurationResponse
         where
