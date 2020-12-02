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
-- Module      : Network.AWS.MediaLive.UpdateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a multiplex.
module Network.AWS.MediaLive.UpdateMultiplex
  ( -- * Creating a Request
    updateMultiplex,
    UpdateMultiplex,

    -- * Request Lenses
    umName,
    umMultiplexSettings,
    umMultiplexId,

    -- * Destructuring the Response
    updateMultiplexResponse,
    UpdateMultiplexResponse,

    -- * Response Lenses
    umrsMultiplex,
    umrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to update a multiplex.
--
-- /See:/ 'updateMultiplex' smart constructor.
data UpdateMultiplex = UpdateMultiplex'
  { _umName :: !(Maybe Text),
    _umMultiplexSettings :: !(Maybe MultiplexSettings),
    _umMultiplexId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umName' - Name of the multiplex.
--
-- * 'umMultiplexSettings' - The new settings for a multiplex.
--
-- * 'umMultiplexId' - ID of the multiplex to update.
updateMultiplex ::
  -- | 'umMultiplexId'
  Text ->
  UpdateMultiplex
updateMultiplex pMultiplexId_ =
  UpdateMultiplex'
    { _umName = Nothing,
      _umMultiplexSettings = Nothing,
      _umMultiplexId = pMultiplexId_
    }

-- | Name of the multiplex.
umName :: Lens' UpdateMultiplex (Maybe Text)
umName = lens _umName (\s a -> s {_umName = a})

-- | The new settings for a multiplex.
umMultiplexSettings :: Lens' UpdateMultiplex (Maybe MultiplexSettings)
umMultiplexSettings = lens _umMultiplexSettings (\s a -> s {_umMultiplexSettings = a})

-- | ID of the multiplex to update.
umMultiplexId :: Lens' UpdateMultiplex Text
umMultiplexId = lens _umMultiplexId (\s a -> s {_umMultiplexId = a})

instance AWSRequest UpdateMultiplex where
  type Rs UpdateMultiplex = UpdateMultiplexResponse
  request = putJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          UpdateMultiplexResponse'
            <$> (x .?> "multiplex") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMultiplex

instance NFData UpdateMultiplex

instance ToHeaders UpdateMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateMultiplex where
  toJSON UpdateMultiplex' {..} =
    object
      ( catMaybes
          [ ("name" .=) <$> _umName,
            ("multiplexSettings" .=) <$> _umMultiplexSettings
          ]
      )

instance ToPath UpdateMultiplex where
  toPath UpdateMultiplex' {..} =
    mconcat ["/prod/multiplexes/", toBS _umMultiplexId]

instance ToQuery UpdateMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'updateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { _umrsMultiplex ::
      !(Maybe Multiplex),
    _umrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umrsMultiplex' - The updated multiplex.
--
-- * 'umrsResponseStatus' - -- | The response status code.
updateMultiplexResponse ::
  -- | 'umrsResponseStatus'
  Int ->
  UpdateMultiplexResponse
updateMultiplexResponse pResponseStatus_ =
  UpdateMultiplexResponse'
    { _umrsMultiplex = Nothing,
      _umrsResponseStatus = pResponseStatus_
    }

-- | The updated multiplex.
umrsMultiplex :: Lens' UpdateMultiplexResponse (Maybe Multiplex)
umrsMultiplex = lens _umrsMultiplex (\s a -> s {_umrsMultiplex = a})

-- | -- | The response status code.
umrsResponseStatus :: Lens' UpdateMultiplexResponse Int
umrsResponseStatus = lens _umrsResponseStatus (\s a -> s {_umrsResponseStatus = a})

instance NFData UpdateMultiplexResponse
