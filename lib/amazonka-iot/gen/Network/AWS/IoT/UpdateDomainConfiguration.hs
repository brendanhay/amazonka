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
-- Module      : Network.AWS.IoT.UpdateDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates values stored in the domain configuration. Domain configurations for default endpoints can't be updated.
module Network.AWS.IoT.UpdateDomainConfiguration
  ( -- * Creating a Request
    updateDomainConfiguration,
    UpdateDomainConfiguration,

    -- * Request Lenses
    udcAuthorizerConfig,
    udcDomainConfigurationStatus,
    udcRemoveAuthorizerConfig,
    udcDomainConfigurationName,

    -- * Destructuring the Response
    updateDomainConfigurationResponse,
    UpdateDomainConfigurationResponse,

    -- * Response Lenses
    udcrsDomainConfigurationName,
    udcrsDomainConfigurationARN,
    udcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { _udcAuthorizerConfig ::
      !(Maybe AuthorizerConfig),
    _udcDomainConfigurationStatus ::
      !(Maybe DomainConfigurationStatus),
    _udcRemoveAuthorizerConfig ::
      !(Maybe Bool),
    _udcDomainConfigurationName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDomainConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcAuthorizerConfig' - An object that specifies the authorization service for a domain.
--
-- * 'udcDomainConfigurationStatus' - The status to which the domain configuration should be updated.
--
-- * 'udcRemoveAuthorizerConfig' - Removes the authorization configuration from a domain.
--
-- * 'udcDomainConfigurationName' - The name of the domain configuration to be updated.
updateDomainConfiguration ::
  -- | 'udcDomainConfigurationName'
  Text ->
  UpdateDomainConfiguration
updateDomainConfiguration pDomainConfigurationName_ =
  UpdateDomainConfiguration'
    { _udcAuthorizerConfig = Nothing,
      _udcDomainConfigurationStatus = Nothing,
      _udcRemoveAuthorizerConfig = Nothing,
      _udcDomainConfigurationName = pDomainConfigurationName_
    }

-- | An object that specifies the authorization service for a domain.
udcAuthorizerConfig :: Lens' UpdateDomainConfiguration (Maybe AuthorizerConfig)
udcAuthorizerConfig = lens _udcAuthorizerConfig (\s a -> s {_udcAuthorizerConfig = a})

-- | The status to which the domain configuration should be updated.
udcDomainConfigurationStatus :: Lens' UpdateDomainConfiguration (Maybe DomainConfigurationStatus)
udcDomainConfigurationStatus = lens _udcDomainConfigurationStatus (\s a -> s {_udcDomainConfigurationStatus = a})

-- | Removes the authorization configuration from a domain.
udcRemoveAuthorizerConfig :: Lens' UpdateDomainConfiguration (Maybe Bool)
udcRemoveAuthorizerConfig = lens _udcRemoveAuthorizerConfig (\s a -> s {_udcRemoveAuthorizerConfig = a})

-- | The name of the domain configuration to be updated.
udcDomainConfigurationName :: Lens' UpdateDomainConfiguration Text
udcDomainConfigurationName = lens _udcDomainConfigurationName (\s a -> s {_udcDomainConfigurationName = a})

instance AWSRequest UpdateDomainConfiguration where
  type
    Rs UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request = putJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            <$> (x .?> "domainConfigurationName")
            <*> (x .?> "domainConfigurationArn")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateDomainConfiguration

instance NFData UpdateDomainConfiguration

instance ToHeaders UpdateDomainConfiguration where
  toHeaders = const mempty

instance ToJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration' {..} =
    object
      ( catMaybes
          [ ("authorizerConfig" .=) <$> _udcAuthorizerConfig,
            ("domainConfigurationStatus" .=) <$> _udcDomainConfigurationStatus,
            ("removeAuthorizerConfig" .=) <$> _udcRemoveAuthorizerConfig
          ]
      )

instance ToPath UpdateDomainConfiguration where
  toPath UpdateDomainConfiguration' {..} =
    mconcat
      ["/domainConfigurations/", toBS _udcDomainConfigurationName]

instance ToQuery UpdateDomainConfiguration where
  toQuery = const mempty

-- | /See:/ 'updateDomainConfigurationResponse' smart constructor.
data UpdateDomainConfigurationResponse = UpdateDomainConfigurationResponse'
  { _udcrsDomainConfigurationName ::
      !(Maybe Text),
    _udcrsDomainConfigurationARN ::
      !(Maybe Text),
    _udcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcrsDomainConfigurationName' - The name of the domain configuration that was updated.
--
-- * 'udcrsDomainConfigurationARN' - The ARN of the domain configuration that was updated.
--
-- * 'udcrsResponseStatus' - -- | The response status code.
updateDomainConfigurationResponse ::
  -- | 'udcrsResponseStatus'
  Int ->
  UpdateDomainConfigurationResponse
updateDomainConfigurationResponse pResponseStatus_ =
  UpdateDomainConfigurationResponse'
    { _udcrsDomainConfigurationName =
        Nothing,
      _udcrsDomainConfigurationARN = Nothing,
      _udcrsResponseStatus = pResponseStatus_
    }

-- | The name of the domain configuration that was updated.
udcrsDomainConfigurationName :: Lens' UpdateDomainConfigurationResponse (Maybe Text)
udcrsDomainConfigurationName = lens _udcrsDomainConfigurationName (\s a -> s {_udcrsDomainConfigurationName = a})

-- | The ARN of the domain configuration that was updated.
udcrsDomainConfigurationARN :: Lens' UpdateDomainConfigurationResponse (Maybe Text)
udcrsDomainConfigurationARN = lens _udcrsDomainConfigurationARN (\s a -> s {_udcrsDomainConfigurationARN = a})

-- | -- | The response status code.
udcrsResponseStatus :: Lens' UpdateDomainConfigurationResponse Int
udcrsResponseStatus = lens _udcrsResponseStatus (\s a -> s {_udcrsResponseStatus = a})

instance NFData UpdateDomainConfigurationResponse
