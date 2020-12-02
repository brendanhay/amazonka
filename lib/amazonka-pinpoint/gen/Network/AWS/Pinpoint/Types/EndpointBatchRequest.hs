{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointBatchRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointBatchItem
import Network.AWS.Prelude

-- | Specifies a batch of endpoints to create or update and the settings and attributes to set or change for each endpoint.
--
--
--
-- /See:/ 'endpointBatchRequest' smart constructor.
newtype EndpointBatchRequest = EndpointBatchRequest'
  { _ebrItem ::
      [EndpointBatchItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebrItem' - An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
endpointBatchRequest ::
  EndpointBatchRequest
endpointBatchRequest = EndpointBatchRequest' {_ebrItem = mempty}

-- | An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
ebrItem :: Lens' EndpointBatchRequest [EndpointBatchItem]
ebrItem = lens _ebrItem (\s a -> s {_ebrItem = a}) . _Coerce

instance Hashable EndpointBatchRequest

instance NFData EndpointBatchRequest

instance ToJSON EndpointBatchRequest where
  toJSON EndpointBatchRequest' {..} =
    object (catMaybes [Just ("Item" .= _ebrItem)])
