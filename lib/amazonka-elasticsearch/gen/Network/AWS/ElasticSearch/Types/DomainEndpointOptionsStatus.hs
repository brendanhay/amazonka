{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus where

import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configured endpoint options for the domain and their current status.
--
--
--
-- /See:/ 'domainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { _deosOptions ::
      !DomainEndpointOptions,
    _deosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainEndpointOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deosOptions' - Options to configure endpoint for the Elasticsearch domain.
--
-- * 'deosStatus' - The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
domainEndpointOptionsStatus ::
  -- | 'deosOptions'
  DomainEndpointOptions ->
  -- | 'deosStatus'
  OptionStatus ->
  DomainEndpointOptionsStatus
domainEndpointOptionsStatus pOptions_ pStatus_ =
  DomainEndpointOptionsStatus'
    { _deosOptions = pOptions_,
      _deosStatus = pStatus_
    }

-- | Options to configure endpoint for the Elasticsearch domain.
deosOptions :: Lens' DomainEndpointOptionsStatus DomainEndpointOptions
deosOptions = lens _deosOptions (\s a -> s {_deosOptions = a})

-- | The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
deosStatus :: Lens' DomainEndpointOptionsStatus OptionStatus
deosStatus = lens _deosStatus (\s a -> s {_deosStatus = a})

instance FromJSON DomainEndpointOptionsStatus where
  parseJSON =
    withObject
      "DomainEndpointOptionsStatus"
      ( \x ->
          DomainEndpointOptionsStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable DomainEndpointOptionsStatus

instance NFData DomainEndpointOptionsStatus
