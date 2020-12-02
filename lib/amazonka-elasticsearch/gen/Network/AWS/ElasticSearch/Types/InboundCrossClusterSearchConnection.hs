{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection where

import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies details of an inbound connection.
--
--
--
-- /See:/ 'inboundCrossClusterSearchConnection' smart constructor.
data InboundCrossClusterSearchConnection = InboundCrossClusterSearchConnection'
  { _iccscDestinationDomainInfo ::
      !( Maybe
           DomainInformation
       ),
    _iccscCrossClusterSearchConnectionId ::
      !(Maybe Text),
    _iccscConnectionStatus ::
      !( Maybe
           InboundCrossClusterSearchConnectionStatus
       ),
    _iccscSourceDomainInfo ::
      !( Maybe
           DomainInformation
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iccscDestinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- * 'iccscCrossClusterSearchConnectionId' - Specifies the connection id for the inbound cross-cluster search connection.
--
-- * 'iccscConnectionStatus' - Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- * 'iccscSourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
inboundCrossClusterSearchConnection ::
  InboundCrossClusterSearchConnection
inboundCrossClusterSearchConnection =
  InboundCrossClusterSearchConnection'
    { _iccscDestinationDomainInfo =
        Nothing,
      _iccscCrossClusterSearchConnectionId = Nothing,
      _iccscConnectionStatus = Nothing,
      _iccscSourceDomainInfo = Nothing
    }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
iccscDestinationDomainInfo :: Lens' InboundCrossClusterSearchConnection (Maybe DomainInformation)
iccscDestinationDomainInfo = lens _iccscDestinationDomainInfo (\s a -> s {_iccscDestinationDomainInfo = a})

-- | Specifies the connection id for the inbound cross-cluster search connection.
iccscCrossClusterSearchConnectionId :: Lens' InboundCrossClusterSearchConnection (Maybe Text)
iccscCrossClusterSearchConnectionId = lens _iccscCrossClusterSearchConnectionId (\s a -> s {_iccscCrossClusterSearchConnectionId = a})

-- | Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
iccscConnectionStatus :: Lens' InboundCrossClusterSearchConnection (Maybe InboundCrossClusterSearchConnectionStatus)
iccscConnectionStatus = lens _iccscConnectionStatus (\s a -> s {_iccscConnectionStatus = a})

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
iccscSourceDomainInfo :: Lens' InboundCrossClusterSearchConnection (Maybe DomainInformation)
iccscSourceDomainInfo = lens _iccscSourceDomainInfo (\s a -> s {_iccscSourceDomainInfo = a})

instance FromJSON InboundCrossClusterSearchConnection where
  parseJSON =
    withObject
      "InboundCrossClusterSearchConnection"
      ( \x ->
          InboundCrossClusterSearchConnection'
            <$> (x .:? "DestinationDomainInfo")
            <*> (x .:? "CrossClusterSearchConnectionId")
            <*> (x .:? "ConnectionStatus")
            <*> (x .:? "SourceDomainInfo")
      )

instance Hashable InboundCrossClusterSearchConnection

instance NFData InboundCrossClusterSearchConnection
