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
-- Module      : Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cross-cluster search connection from a source domain to a destination domain.
module Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
  ( -- * Creating a Request
    createOutboundCrossClusterSearchConnection,
    CreateOutboundCrossClusterSearchConnection,

    -- * Request Lenses
    coccscSourceDomainInfo,
    coccscDestinationDomainInfo,
    coccscConnectionAlias,

    -- * Destructuring the Response
    createOutboundCrossClusterSearchConnectionResponse,
    CreateOutboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    coccscrsDestinationDomainInfo,
    coccscrsConnectionAlias,
    coccscrsCrossClusterSearchConnectionId,
    coccscrsConnectionStatus,
    coccscrsSourceDomainInfo,
    coccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'CreateOutboundCrossClusterSearchConnection' @ operation.
--
--
--
-- /See:/ 'createOutboundCrossClusterSearchConnection' smart constructor.
data CreateOutboundCrossClusterSearchConnection = CreateOutboundCrossClusterSearchConnection'
  { _coccscSourceDomainInfo ::
      !DomainInformation,
    _coccscDestinationDomainInfo ::
      !DomainInformation,
    _coccscConnectionAlias ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateOutboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coccscSourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- * 'coccscDestinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- * 'coccscConnectionAlias' - Specifies the connection alias that will be used by the customer for this connection.
createOutboundCrossClusterSearchConnection ::
  -- | 'coccscSourceDomainInfo'
  DomainInformation ->
  -- | 'coccscDestinationDomainInfo'
  DomainInformation ->
  -- | 'coccscConnectionAlias'
  Text ->
  CreateOutboundCrossClusterSearchConnection
createOutboundCrossClusterSearchConnection
  pSourceDomainInfo_
  pDestinationDomainInfo_
  pConnectionAlias_ =
    CreateOutboundCrossClusterSearchConnection'
      { _coccscSourceDomainInfo =
          pSourceDomainInfo_,
        _coccscDestinationDomainInfo =
          pDestinationDomainInfo_,
        _coccscConnectionAlias = pConnectionAlias_
      }

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
coccscSourceDomainInfo :: Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
coccscSourceDomainInfo = lens _coccscSourceDomainInfo (\s a -> s {_coccscSourceDomainInfo = a})

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
coccscDestinationDomainInfo :: Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
coccscDestinationDomainInfo = lens _coccscDestinationDomainInfo (\s a -> s {_coccscDestinationDomainInfo = a})

-- | Specifies the connection alias that will be used by the customer for this connection.
coccscConnectionAlias :: Lens' CreateOutboundCrossClusterSearchConnection Text
coccscConnectionAlias = lens _coccscConnectionAlias (\s a -> s {_coccscConnectionAlias = a})

instance AWSRequest CreateOutboundCrossClusterSearchConnection where
  type
    Rs CreateOutboundCrossClusterSearchConnection =
      CreateOutboundCrossClusterSearchConnectionResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          CreateOutboundCrossClusterSearchConnectionResponse'
            <$> (x .?> "DestinationDomainInfo")
            <*> (x .?> "ConnectionAlias")
            <*> (x .?> "CrossClusterSearchConnectionId")
            <*> (x .?> "ConnectionStatus")
            <*> (x .?> "SourceDomainInfo")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateOutboundCrossClusterSearchConnection

instance NFData CreateOutboundCrossClusterSearchConnection

instance ToHeaders CreateOutboundCrossClusterSearchConnection where
  toHeaders = const mempty

instance ToJSON CreateOutboundCrossClusterSearchConnection where
  toJSON CreateOutboundCrossClusterSearchConnection' {..} =
    object
      ( catMaybes
          [ Just ("SourceDomainInfo" .= _coccscSourceDomainInfo),
            Just ("DestinationDomainInfo" .= _coccscDestinationDomainInfo),
            Just ("ConnectionAlias" .= _coccscConnectionAlias)
          ]
      )

instance ToPath CreateOutboundCrossClusterSearchConnection where
  toPath = const "/2015-01-01/es/ccs/outboundConnection"

instance ToQuery CreateOutboundCrossClusterSearchConnection where
  toQuery = const mempty

-- | The result of a @'CreateOutboundCrossClusterSearchConnection' @ request. Contains the details of the newly created cross-cluster search connection.
--
--
--
-- /See:/ 'createOutboundCrossClusterSearchConnectionResponse' smart constructor.
data CreateOutboundCrossClusterSearchConnectionResponse = CreateOutboundCrossClusterSearchConnectionResponse'
  { _coccscrsDestinationDomainInfo ::
      !( Maybe
           DomainInformation
       ),
    _coccscrsConnectionAlias ::
      !( Maybe
           Text
       ),
    _coccscrsCrossClusterSearchConnectionId ::
      !( Maybe
           Text
       ),
    _coccscrsConnectionStatus ::
      !( Maybe
           OutboundCrossClusterSearchConnectionStatus
       ),
    _coccscrsSourceDomainInfo ::
      !( Maybe
           DomainInformation
       ),
    _coccscrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateOutboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coccscrsDestinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- * 'coccscrsConnectionAlias' - Specifies the connection alias provided during the create connection request.
--
-- * 'coccscrsCrossClusterSearchConnectionId' - Unique id for the created outbound connection, which is used for subsequent operations on connection.
--
-- * 'coccscrsConnectionStatus' - Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
--
-- * 'coccscrsSourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- * 'coccscrsResponseStatus' - -- | The response status code.
createOutboundCrossClusterSearchConnectionResponse ::
  -- | 'coccscrsResponseStatus'
  Int ->
  CreateOutboundCrossClusterSearchConnectionResponse
createOutboundCrossClusterSearchConnectionResponse pResponseStatus_ =
  CreateOutboundCrossClusterSearchConnectionResponse'
    { _coccscrsDestinationDomainInfo =
        Nothing,
      _coccscrsConnectionAlias = Nothing,
      _coccscrsCrossClusterSearchConnectionId =
        Nothing,
      _coccscrsConnectionStatus = Nothing,
      _coccscrsSourceDomainInfo = Nothing,
      _coccscrsResponseStatus = pResponseStatus_
    }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
coccscrsDestinationDomainInfo :: Lens' CreateOutboundCrossClusterSearchConnectionResponse (Maybe DomainInformation)
coccscrsDestinationDomainInfo = lens _coccscrsDestinationDomainInfo (\s a -> s {_coccscrsDestinationDomainInfo = a})

-- | Specifies the connection alias provided during the create connection request.
coccscrsConnectionAlias :: Lens' CreateOutboundCrossClusterSearchConnectionResponse (Maybe Text)
coccscrsConnectionAlias = lens _coccscrsConnectionAlias (\s a -> s {_coccscrsConnectionAlias = a})

-- | Unique id for the created outbound connection, which is used for subsequent operations on connection.
coccscrsCrossClusterSearchConnectionId :: Lens' CreateOutboundCrossClusterSearchConnectionResponse (Maybe Text)
coccscrsCrossClusterSearchConnectionId = lens _coccscrsCrossClusterSearchConnectionId (\s a -> s {_coccscrsCrossClusterSearchConnectionId = a})

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
coccscrsConnectionStatus :: Lens' CreateOutboundCrossClusterSearchConnectionResponse (Maybe OutboundCrossClusterSearchConnectionStatus)
coccscrsConnectionStatus = lens _coccscrsConnectionStatus (\s a -> s {_coccscrsConnectionStatus = a})

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
coccscrsSourceDomainInfo :: Lens' CreateOutboundCrossClusterSearchConnectionResponse (Maybe DomainInformation)
coccscrsSourceDomainInfo = lens _coccscrsSourceDomainInfo (\s a -> s {_coccscrsSourceDomainInfo = a})

-- | -- | The response status code.
coccscrsResponseStatus :: Lens' CreateOutboundCrossClusterSearchConnectionResponse Int
coccscrsResponseStatus = lens _coccscrsResponseStatus (\s a -> s {_coccscrsResponseStatus = a})

instance NFData CreateOutboundCrossClusterSearchConnectionResponse
