{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection where

import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies details of an outbound connection.
--
--
--
-- /See:/ 'outboundCrossClusterSearchConnection' smart constructor.
data OutboundCrossClusterSearchConnection = OutboundCrossClusterSearchConnection'
  { _occscDestinationDomainInfo ::
      !( Maybe
           DomainInformation
       ),
    _occscConnectionAlias ::
      !(Maybe Text),
    _occscCrossClusterSearchConnectionId ::
      !(Maybe Text),
    _occscConnectionStatus ::
      !( Maybe
           OutboundCrossClusterSearchConnectionStatus
       ),
    _occscSourceDomainInfo ::
      !( Maybe
           DomainInformation
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'occscDestinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- * 'occscConnectionAlias' - Specifies the connection alias for the outbound cross-cluster search connection.
--
-- * 'occscCrossClusterSearchConnectionId' - Specifies the connection id for the outbound cross-cluster search connection.
--
-- * 'occscConnectionStatus' - Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- * 'occscSourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
outboundCrossClusterSearchConnection ::
  OutboundCrossClusterSearchConnection
outboundCrossClusterSearchConnection =
  OutboundCrossClusterSearchConnection'
    { _occscDestinationDomainInfo =
        Nothing,
      _occscConnectionAlias = Nothing,
      _occscCrossClusterSearchConnectionId = Nothing,
      _occscConnectionStatus = Nothing,
      _occscSourceDomainInfo = Nothing
    }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
occscDestinationDomainInfo :: Lens' OutboundCrossClusterSearchConnection (Maybe DomainInformation)
occscDestinationDomainInfo = lens _occscDestinationDomainInfo (\s a -> s {_occscDestinationDomainInfo = a})

-- | Specifies the connection alias for the outbound cross-cluster search connection.
occscConnectionAlias :: Lens' OutboundCrossClusterSearchConnection (Maybe Text)
occscConnectionAlias = lens _occscConnectionAlias (\s a -> s {_occscConnectionAlias = a})

-- | Specifies the connection id for the outbound cross-cluster search connection.
occscCrossClusterSearchConnectionId :: Lens' OutboundCrossClusterSearchConnection (Maybe Text)
occscCrossClusterSearchConnectionId = lens _occscCrossClusterSearchConnectionId (\s a -> s {_occscCrossClusterSearchConnectionId = a})

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
occscConnectionStatus :: Lens' OutboundCrossClusterSearchConnection (Maybe OutboundCrossClusterSearchConnectionStatus)
occscConnectionStatus = lens _occscConnectionStatus (\s a -> s {_occscConnectionStatus = a})

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
occscSourceDomainInfo :: Lens' OutboundCrossClusterSearchConnection (Maybe DomainInformation)
occscSourceDomainInfo = lens _occscSourceDomainInfo (\s a -> s {_occscSourceDomainInfo = a})

instance FromJSON OutboundCrossClusterSearchConnection where
  parseJSON =
    withObject
      "OutboundCrossClusterSearchConnection"
      ( \x ->
          OutboundCrossClusterSearchConnection'
            <$> (x .:? "DestinationDomainInfo")
            <*> (x .:? "ConnectionAlias")
            <*> (x .:? "CrossClusterSearchConnectionId")
            <*> (x .:? "ConnectionStatus")
            <*> (x .:? "SourceDomainInfo")
      )

instance Hashable OutboundCrossClusterSearchConnection

instance NFData OutboundCrossClusterSearchConnection
