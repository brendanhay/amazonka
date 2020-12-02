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
-- Module      : Network.AWS.Greengrass.GetThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime configuration of a thing.
module Network.AWS.Greengrass.GetThingRuntimeConfiguration
  ( -- * Creating a Request
    getThingRuntimeConfiguration,
    GetThingRuntimeConfiguration,

    -- * Request Lenses
    gtrcThingName,

    -- * Destructuring the Response
    getThingRuntimeConfigurationResponse,
    GetThingRuntimeConfigurationResponse,

    -- * Response Lenses
    gtrcrsRuntimeConfiguration,
    gtrcrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getThingRuntimeConfiguration' smart constructor.
newtype GetThingRuntimeConfiguration = GetThingRuntimeConfiguration'
  { _gtrcThingName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetThingRuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrcThingName' - The thing name.
getThingRuntimeConfiguration ::
  -- | 'gtrcThingName'
  Text ->
  GetThingRuntimeConfiguration
getThingRuntimeConfiguration pThingName_ =
  GetThingRuntimeConfiguration' {_gtrcThingName = pThingName_}

-- | The thing name.
gtrcThingName :: Lens' GetThingRuntimeConfiguration Text
gtrcThingName = lens _gtrcThingName (\s a -> s {_gtrcThingName = a})

instance AWSRequest GetThingRuntimeConfiguration where
  type
    Rs GetThingRuntimeConfiguration =
      GetThingRuntimeConfigurationResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetThingRuntimeConfigurationResponse'
            <$> (x .?> "RuntimeConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable GetThingRuntimeConfiguration

instance NFData GetThingRuntimeConfiguration

instance ToHeaders GetThingRuntimeConfiguration where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetThingRuntimeConfiguration where
  toPath GetThingRuntimeConfiguration' {..} =
    mconcat
      ["/greengrass/things/", toBS _gtrcThingName, "/runtimeconfig"]

instance ToQuery GetThingRuntimeConfiguration where
  toQuery = const mempty

-- | /See:/ 'getThingRuntimeConfigurationResponse' smart constructor.
data GetThingRuntimeConfigurationResponse = GetThingRuntimeConfigurationResponse'
  { _gtrcrsRuntimeConfiguration ::
      !( Maybe
           RuntimeConfiguration
       ),
    _gtrcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetThingRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrcrsRuntimeConfiguration' - Runtime configuration for a thing.
--
-- * 'gtrcrsResponseStatus' - -- | The response status code.
getThingRuntimeConfigurationResponse ::
  -- | 'gtrcrsResponseStatus'
  Int ->
  GetThingRuntimeConfigurationResponse
getThingRuntimeConfigurationResponse pResponseStatus_ =
  GetThingRuntimeConfigurationResponse'
    { _gtrcrsRuntimeConfiguration =
        Nothing,
      _gtrcrsResponseStatus = pResponseStatus_
    }

-- | Runtime configuration for a thing.
gtrcrsRuntimeConfiguration :: Lens' GetThingRuntimeConfigurationResponse (Maybe RuntimeConfiguration)
gtrcrsRuntimeConfiguration = lens _gtrcrsRuntimeConfiguration (\s a -> s {_gtrcrsRuntimeConfiguration = a})

-- | -- | The response status code.
gtrcrsResponseStatus :: Lens' GetThingRuntimeConfigurationResponse Int
gtrcrsResponseStatus = lens _gtrcrsResponseStatus (\s a -> s {_gtrcrsResponseStatus = a})

instance NFData GetThingRuntimeConfigurationResponse
