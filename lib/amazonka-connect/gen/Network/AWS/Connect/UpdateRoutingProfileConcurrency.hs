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
-- Module      : Network.AWS.Connect.UpdateRoutingProfileConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the channels that agents can handle in the Contact Control Panel (CCP) for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileConcurrency
  ( -- * Creating a Request
    updateRoutingProfileConcurrency,
    UpdateRoutingProfileConcurrency,

    -- * Request Lenses
    urpcInstanceId,
    urpcRoutingProfileId,
    urpcMediaConcurrencies,

    -- * Destructuring the Response
    updateRoutingProfileConcurrencyResponse,
    UpdateRoutingProfileConcurrencyResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoutingProfileConcurrency' smart constructor.
data UpdateRoutingProfileConcurrency = UpdateRoutingProfileConcurrency'
  { _urpcInstanceId ::
      !Text,
    _urpcRoutingProfileId ::
      !Text,
    _urpcMediaConcurrencies ::
      ![MediaConcurrency]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileConcurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpcInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'urpcRoutingProfileId' - The identifier of the routing profile.
--
-- * 'urpcMediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP).
updateRoutingProfileConcurrency ::
  -- | 'urpcInstanceId'
  Text ->
  -- | 'urpcRoutingProfileId'
  Text ->
  UpdateRoutingProfileConcurrency
updateRoutingProfileConcurrency pInstanceId_ pRoutingProfileId_ =
  UpdateRoutingProfileConcurrency'
    { _urpcInstanceId = pInstanceId_,
      _urpcRoutingProfileId = pRoutingProfileId_,
      _urpcMediaConcurrencies = mempty
    }

-- | The identifier of the Amazon Connect instance.
urpcInstanceId :: Lens' UpdateRoutingProfileConcurrency Text
urpcInstanceId = lens _urpcInstanceId (\s a -> s {_urpcInstanceId = a})

-- | The identifier of the routing profile.
urpcRoutingProfileId :: Lens' UpdateRoutingProfileConcurrency Text
urpcRoutingProfileId = lens _urpcRoutingProfileId (\s a -> s {_urpcRoutingProfileId = a})

-- | The channels agents can handle in the Contact Control Panel (CCP).
urpcMediaConcurrencies :: Lens' UpdateRoutingProfileConcurrency [MediaConcurrency]
urpcMediaConcurrencies = lens _urpcMediaConcurrencies (\s a -> s {_urpcMediaConcurrencies = a}) . _Coerce

instance AWSRequest UpdateRoutingProfileConcurrency where
  type
    Rs UpdateRoutingProfileConcurrency =
      UpdateRoutingProfileConcurrencyResponse
  request = postJSON connect
  response = receiveNull UpdateRoutingProfileConcurrencyResponse'

instance Hashable UpdateRoutingProfileConcurrency

instance NFData UpdateRoutingProfileConcurrency

instance ToHeaders UpdateRoutingProfileConcurrency where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateRoutingProfileConcurrency where
  toJSON UpdateRoutingProfileConcurrency' {..} =
    object
      ( catMaybes
          [Just ("MediaConcurrencies" .= _urpcMediaConcurrencies)]
      )

instance ToPath UpdateRoutingProfileConcurrency where
  toPath UpdateRoutingProfileConcurrency' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _urpcInstanceId,
        "/",
        toBS _urpcRoutingProfileId,
        "/concurrency"
      ]

instance ToQuery UpdateRoutingProfileConcurrency where
  toQuery = const mempty

-- | /See:/ 'updateRoutingProfileConcurrencyResponse' smart constructor.
data UpdateRoutingProfileConcurrencyResponse = UpdateRoutingProfileConcurrencyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileConcurrencyResponse' with the minimum fields required to make a request.
updateRoutingProfileConcurrencyResponse ::
  UpdateRoutingProfileConcurrencyResponse
updateRoutingProfileConcurrencyResponse =
  UpdateRoutingProfileConcurrencyResponse'

instance NFData UpdateRoutingProfileConcurrencyResponse
