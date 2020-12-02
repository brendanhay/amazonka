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
-- Module      : Network.AWS.CloudSearch.UpdateDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateDomainEndpointOptions
  ( -- * Creating a Request
    updateDomainEndpointOptions,
    UpdateDomainEndpointOptions,

    -- * Request Lenses
    udeoDomainName,
    udeoDomainEndpointOptions,

    -- * Destructuring the Response
    updateDomainEndpointOptionsResponse,
    UpdateDomainEndpointOptionsResponse,

    -- * Response Lenses
    udeorsDomainEndpointOptions,
    udeorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'UpdateDomainEndpointOptions' @ operation. Specifies the name of the domain you want to update and the domain endpoint options.
--
--
--
-- /See:/ 'updateDomainEndpointOptions' smart constructor.
data UpdateDomainEndpointOptions = UpdateDomainEndpointOptions'
  { _udeoDomainName ::
      !Text,
    _udeoDomainEndpointOptions ::
      !DomainEndpointOptions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDomainEndpointOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udeoDomainName' - A string that represents the name of a domain.
--
-- * 'udeoDomainEndpointOptions' - Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07.
updateDomainEndpointOptions ::
  -- | 'udeoDomainName'
  Text ->
  -- | 'udeoDomainEndpointOptions'
  DomainEndpointOptions ->
  UpdateDomainEndpointOptions
updateDomainEndpointOptions pDomainName_ pDomainEndpointOptions_ =
  UpdateDomainEndpointOptions'
    { _udeoDomainName = pDomainName_,
      _udeoDomainEndpointOptions = pDomainEndpointOptions_
    }

-- | A string that represents the name of a domain.
udeoDomainName :: Lens' UpdateDomainEndpointOptions Text
udeoDomainName = lens _udeoDomainName (\s a -> s {_udeoDomainName = a})

-- | Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07.
udeoDomainEndpointOptions :: Lens' UpdateDomainEndpointOptions DomainEndpointOptions
udeoDomainEndpointOptions = lens _udeoDomainEndpointOptions (\s a -> s {_udeoDomainEndpointOptions = a})

instance AWSRequest UpdateDomainEndpointOptions where
  type
    Rs UpdateDomainEndpointOptions =
      UpdateDomainEndpointOptionsResponse
  request = postQuery cloudSearch
  response =
    receiveXMLWrapper
      "UpdateDomainEndpointOptionsResult"
      ( \s h x ->
          UpdateDomainEndpointOptionsResponse'
            <$> (x .@? "DomainEndpointOptions") <*> (pure (fromEnum s))
      )

instance Hashable UpdateDomainEndpointOptions

instance NFData UpdateDomainEndpointOptions

instance ToHeaders UpdateDomainEndpointOptions where
  toHeaders = const mempty

instance ToPath UpdateDomainEndpointOptions where
  toPath = const "/"

instance ToQuery UpdateDomainEndpointOptions where
  toQuery UpdateDomainEndpointOptions' {..} =
    mconcat
      [ "Action" =: ("UpdateDomainEndpointOptions" :: ByteString),
        "Version" =: ("2013-01-01" :: ByteString),
        "DomainName" =: _udeoDomainName,
        "DomainEndpointOptions" =: _udeoDomainEndpointOptions
      ]

-- | The result of a @UpdateDomainEndpointOptions@ request. Contains the configuration and status of the domain's endpoint options.
--
--
--
-- /See:/ 'updateDomainEndpointOptionsResponse' smart constructor.
data UpdateDomainEndpointOptionsResponse = UpdateDomainEndpointOptionsResponse'
  { _udeorsDomainEndpointOptions ::
      !( Maybe
           DomainEndpointOptionsStatus
       ),
    _udeorsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDomainEndpointOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udeorsDomainEndpointOptions' - The newly-configured domain endpoint options.
--
-- * 'udeorsResponseStatus' - -- | The response status code.
updateDomainEndpointOptionsResponse ::
  -- | 'udeorsResponseStatus'
  Int ->
  UpdateDomainEndpointOptionsResponse
updateDomainEndpointOptionsResponse pResponseStatus_ =
  UpdateDomainEndpointOptionsResponse'
    { _udeorsDomainEndpointOptions =
        Nothing,
      _udeorsResponseStatus = pResponseStatus_
    }

-- | The newly-configured domain endpoint options.
udeorsDomainEndpointOptions :: Lens' UpdateDomainEndpointOptionsResponse (Maybe DomainEndpointOptionsStatus)
udeorsDomainEndpointOptions = lens _udeorsDomainEndpointOptions (\s a -> s {_udeorsDomainEndpointOptions = a})

-- | -- | The response status code.
udeorsResponseStatus :: Lens' UpdateDomainEndpointOptionsResponse Int
udeorsResponseStatus = lens _udeorsResponseStatus (\s a -> s {_udeorsResponseStatus = a})

instance NFData UpdateDomainEndpointOptionsResponse
