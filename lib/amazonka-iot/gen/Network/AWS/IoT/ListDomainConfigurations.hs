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
-- Module      : Network.AWS.IoT.ListDomainConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of domain configurations for the user. This list is sorted alphabetically by domain configuration name.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDomainConfigurations
  ( -- * Creating a Request
    listDomainConfigurations,
    ListDomainConfigurations,

    -- * Request Lenses
    ldcMarker,
    ldcServiceType,
    ldcPageSize,

    -- * Destructuring the Response
    listDomainConfigurationsResponse,
    ListDomainConfigurationsResponse,

    -- * Response Lenses
    ldcrsDomainConfigurations,
    ldcrsNextMarker,
    ldcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { _ldcMarker ::
      !(Maybe Text),
    _ldcServiceType :: !(Maybe ServiceType),
    _ldcPageSize :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomainConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcMarker' - The marker for the next set of results.
--
-- * 'ldcServiceType' - The type of service delivered by the endpoint.
--
-- * 'ldcPageSize' - The result page size.
listDomainConfigurations ::
  ListDomainConfigurations
listDomainConfigurations =
  ListDomainConfigurations'
    { _ldcMarker = Nothing,
      _ldcServiceType = Nothing,
      _ldcPageSize = Nothing
    }

-- | The marker for the next set of results.
ldcMarker :: Lens' ListDomainConfigurations (Maybe Text)
ldcMarker = lens _ldcMarker (\s a -> s {_ldcMarker = a})

-- | The type of service delivered by the endpoint.
ldcServiceType :: Lens' ListDomainConfigurations (Maybe ServiceType)
ldcServiceType = lens _ldcServiceType (\s a -> s {_ldcServiceType = a})

-- | The result page size.
ldcPageSize :: Lens' ListDomainConfigurations (Maybe Natural)
ldcPageSize = lens _ldcPageSize (\s a -> s {_ldcPageSize = a}) . mapping _Nat

instance AWSPager ListDomainConfigurations where
  page rq rs
    | stop (rs ^. ldcrsNextMarker) = Nothing
    | stop (rs ^. ldcrsDomainConfigurations) = Nothing
    | otherwise = Just $ rq & ldcMarker .~ rs ^. ldcrsNextMarker

instance AWSRequest ListDomainConfigurations where
  type Rs ListDomainConfigurations = ListDomainConfigurationsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListDomainConfigurationsResponse'
            <$> (x .?> "domainConfigurations" .!@ mempty)
            <*> (x .?> "nextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDomainConfigurations

instance NFData ListDomainConfigurations

instance ToHeaders ListDomainConfigurations where
  toHeaders = const mempty

instance ToPath ListDomainConfigurations where
  toPath = const "/domainConfigurations"

instance ToQuery ListDomainConfigurations where
  toQuery ListDomainConfigurations' {..} =
    mconcat
      [ "marker" =: _ldcMarker,
        "serviceType" =: _ldcServiceType,
        "pageSize" =: _ldcPageSize
      ]

-- | /See:/ 'listDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { _ldcrsDomainConfigurations ::
      !( Maybe
           [DomainConfigurationSummary]
       ),
    _ldcrsNextMarker ::
      !(Maybe Text),
    _ldcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomainConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcrsDomainConfigurations' - A list of objects that contain summary information about the user's domain configurations.
--
-- * 'ldcrsNextMarker' - The marker for the next set of results.
--
-- * 'ldcrsResponseStatus' - -- | The response status code.
listDomainConfigurationsResponse ::
  -- | 'ldcrsResponseStatus'
  Int ->
  ListDomainConfigurationsResponse
listDomainConfigurationsResponse pResponseStatus_ =
  ListDomainConfigurationsResponse'
    { _ldcrsDomainConfigurations =
        Nothing,
      _ldcrsNextMarker = Nothing,
      _ldcrsResponseStatus = pResponseStatus_
    }

-- | A list of objects that contain summary information about the user's domain configurations.
ldcrsDomainConfigurations :: Lens' ListDomainConfigurationsResponse [DomainConfigurationSummary]
ldcrsDomainConfigurations = lens _ldcrsDomainConfigurations (\s a -> s {_ldcrsDomainConfigurations = a}) . _Default . _Coerce

-- | The marker for the next set of results.
ldcrsNextMarker :: Lens' ListDomainConfigurationsResponse (Maybe Text)
ldcrsNextMarker = lens _ldcrsNextMarker (\s a -> s {_ldcrsNextMarker = a})

-- | -- | The response status code.
ldcrsResponseStatus :: Lens' ListDomainConfigurationsResponse Int
ldcrsResponseStatus = lens _ldcrsResponseStatus (\s a -> s {_ldcrsResponseStatus = a})

instance NFData ListDomainConfigurationsResponse
