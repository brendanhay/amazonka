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
-- Module      : Network.AWS.CloudSearch.DescribeDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeDomainEndpointOptions
  ( -- * Creating a Request
    describeDomainEndpointOptions,
    DescribeDomainEndpointOptions,

    -- * Request Lenses
    ddeoDeployed,
    ddeoDomainName,

    -- * Destructuring the Response
    describeDomainEndpointOptionsResponse,
    DescribeDomainEndpointOptionsResponse,

    -- * Response Lenses
    ddeorsDomainEndpointOptions,
    ddeorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeDomainEndpointOptions' @ operation. Specify the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to @true@ .
--
--
--
-- /See:/ 'describeDomainEndpointOptions' smart constructor.
data DescribeDomainEndpointOptions = DescribeDomainEndpointOptions'
  { _ddeoDeployed ::
      !(Maybe Bool),
    _ddeoDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomainEndpointOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddeoDeployed' - Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
--
-- * 'ddeoDomainName' - A string that represents the name of a domain.
describeDomainEndpointOptions ::
  -- | 'ddeoDomainName'
  Text ->
  DescribeDomainEndpointOptions
describeDomainEndpointOptions pDomainName_ =
  DescribeDomainEndpointOptions'
    { _ddeoDeployed = Nothing,
      _ddeoDomainName = pDomainName_
    }

-- | Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
ddeoDeployed :: Lens' DescribeDomainEndpointOptions (Maybe Bool)
ddeoDeployed = lens _ddeoDeployed (\s a -> s {_ddeoDeployed = a})

-- | A string that represents the name of a domain.
ddeoDomainName :: Lens' DescribeDomainEndpointOptions Text
ddeoDomainName = lens _ddeoDomainName (\s a -> s {_ddeoDomainName = a})

instance AWSRequest DescribeDomainEndpointOptions where
  type
    Rs DescribeDomainEndpointOptions =
      DescribeDomainEndpointOptionsResponse
  request = postQuery cloudSearch
  response =
    receiveXMLWrapper
      "DescribeDomainEndpointOptionsResult"
      ( \s h x ->
          DescribeDomainEndpointOptionsResponse'
            <$> (x .@? "DomainEndpointOptions") <*> (pure (fromEnum s))
      )

instance Hashable DescribeDomainEndpointOptions

instance NFData DescribeDomainEndpointOptions

instance ToHeaders DescribeDomainEndpointOptions where
  toHeaders = const mempty

instance ToPath DescribeDomainEndpointOptions where
  toPath = const "/"

instance ToQuery DescribeDomainEndpointOptions where
  toQuery DescribeDomainEndpointOptions' {..} =
    mconcat
      [ "Action" =: ("DescribeDomainEndpointOptions" :: ByteString),
        "Version" =: ("2013-01-01" :: ByteString),
        "Deployed" =: _ddeoDeployed,
        "DomainName" =: _ddeoDomainName
      ]

-- | The result of a @DescribeDomainEndpointOptions@ request. Contains the status and configuration of a search domain's endpoint options.
--
--
--
-- /See:/ 'describeDomainEndpointOptionsResponse' smart constructor.
data DescribeDomainEndpointOptionsResponse = DescribeDomainEndpointOptionsResponse'
  { _ddeorsDomainEndpointOptions ::
      !( Maybe
           DomainEndpointOptionsStatus
       ),
    _ddeorsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomainEndpointOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddeorsDomainEndpointOptions' - The status and configuration of a search domain's endpoint options.
--
-- * 'ddeorsResponseStatus' - -- | The response status code.
describeDomainEndpointOptionsResponse ::
  -- | 'ddeorsResponseStatus'
  Int ->
  DescribeDomainEndpointOptionsResponse
describeDomainEndpointOptionsResponse pResponseStatus_ =
  DescribeDomainEndpointOptionsResponse'
    { _ddeorsDomainEndpointOptions =
        Nothing,
      _ddeorsResponseStatus = pResponseStatus_
    }

-- | The status and configuration of a search domain's endpoint options.
ddeorsDomainEndpointOptions :: Lens' DescribeDomainEndpointOptionsResponse (Maybe DomainEndpointOptionsStatus)
ddeorsDomainEndpointOptions = lens _ddeorsDomainEndpointOptions (\s a -> s {_ddeorsDomainEndpointOptions = a})

-- | -- | The response status code.
ddeorsResponseStatus :: Lens' DescribeDomainEndpointOptionsResponse Int
ddeorsResponseStatus = lens _ddeorsResponseStatus (\s a -> s {_ddeorsResponseStatus = a})

instance NFData DescribeDomainEndpointOptionsResponse
