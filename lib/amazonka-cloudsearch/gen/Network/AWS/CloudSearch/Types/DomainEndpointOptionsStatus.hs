{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus where

import Network.AWS.CloudSearch.Types.DomainEndpointOptions
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration and status of the domain's endpoint options.
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
-- * 'deosOptions' - The domain endpoint options configured for the domain.
--
-- * 'deosStatus' - The status of the configured domain endpoint options.
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

-- | The domain endpoint options configured for the domain.
deosOptions :: Lens' DomainEndpointOptionsStatus DomainEndpointOptions
deosOptions = lens _deosOptions (\s a -> s {_deosOptions = a})

-- | The status of the configured domain endpoint options.
deosStatus :: Lens' DomainEndpointOptionsStatus OptionStatus
deosStatus = lens _deosStatus (\s a -> s {_deosStatus = a})

instance FromXML DomainEndpointOptionsStatus where
  parseXML x =
    DomainEndpointOptionsStatus'
      <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable DomainEndpointOptionsStatus

instance NFData DomainEndpointOptionsStatus
