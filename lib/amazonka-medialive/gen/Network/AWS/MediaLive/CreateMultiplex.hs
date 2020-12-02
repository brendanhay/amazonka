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
-- Module      : Network.AWS.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Network.AWS.MediaLive.CreateMultiplex
  ( -- * Creating a Request
    createMultiplex,
    CreateMultiplex,

    -- * Request Lenses
    cmTags,
    cmRequestId,
    cmMultiplexSettings,
    cmAvailabilityZones,
    cmName,

    -- * Destructuring the Response
    createMultiplexResponse,
    CreateMultiplexResponse,

    -- * Response Lenses
    cmrsMultiplex,
    cmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to create a multiplex.
--
-- /See:/ 'createMultiplex' smart constructor.
data CreateMultiplex = CreateMultiplex'
  { _cmTags ::
      !(Maybe (Map Text (Text))),
    _cmRequestId :: !Text,
    _cmMultiplexSettings :: !MultiplexSettings,
    _cmAvailabilityZones :: ![Text],
    _cmName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmTags' - A collection of key-value pairs.
--
-- * 'cmRequestId' - Unique request ID. This prevents retries from creating multiple resources.
--
-- * 'cmMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'cmAvailabilityZones' - A list of availability zones for the multiplex. You must specify exactly two.
--
-- * 'cmName' - Name of multiplex.
createMultiplex ::
  -- | 'cmRequestId'
  Text ->
  -- | 'cmMultiplexSettings'
  MultiplexSettings ->
  -- | 'cmName'
  Text ->
  CreateMultiplex
createMultiplex pRequestId_ pMultiplexSettings_ pName_ =
  CreateMultiplex'
    { _cmTags = Nothing,
      _cmRequestId = pRequestId_,
      _cmMultiplexSettings = pMultiplexSettings_,
      _cmAvailabilityZones = mempty,
      _cmName = pName_
    }

-- | A collection of key-value pairs.
cmTags :: Lens' CreateMultiplex (HashMap Text (Text))
cmTags = lens _cmTags (\s a -> s {_cmTags = a}) . _Default . _Map

-- | Unique request ID. This prevents retries from creating multiple resources.
cmRequestId :: Lens' CreateMultiplex Text
cmRequestId = lens _cmRequestId (\s a -> s {_cmRequestId = a})

-- | Configuration for a multiplex event.
cmMultiplexSettings :: Lens' CreateMultiplex MultiplexSettings
cmMultiplexSettings = lens _cmMultiplexSettings (\s a -> s {_cmMultiplexSettings = a})

-- | A list of availability zones for the multiplex. You must specify exactly two.
cmAvailabilityZones :: Lens' CreateMultiplex [Text]
cmAvailabilityZones = lens _cmAvailabilityZones (\s a -> s {_cmAvailabilityZones = a}) . _Coerce

-- | Name of multiplex.
cmName :: Lens' CreateMultiplex Text
cmName = lens _cmName (\s a -> s {_cmName = a})

instance AWSRequest CreateMultiplex where
  type Rs CreateMultiplex = CreateMultiplexResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          CreateMultiplexResponse'
            <$> (x .?> "multiplex") <*> (pure (fromEnum s))
      )

instance Hashable CreateMultiplex

instance NFData CreateMultiplex

instance ToHeaders CreateMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateMultiplex where
  toJSON CreateMultiplex' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _cmTags,
            Just ("requestId" .= _cmRequestId),
            Just ("multiplexSettings" .= _cmMultiplexSettings),
            Just ("availabilityZones" .= _cmAvailabilityZones),
            Just ("name" .= _cmName)
          ]
      )

instance ToPath CreateMultiplex where
  toPath = const "/prod/multiplexes"

instance ToQuery CreateMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'createMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { _cmrsMultiplex ::
      !(Maybe Multiplex),
    _cmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsMultiplex' - The newly created multiplex.
--
-- * 'cmrsResponseStatus' - -- | The response status code.
createMultiplexResponse ::
  -- | 'cmrsResponseStatus'
  Int ->
  CreateMultiplexResponse
createMultiplexResponse pResponseStatus_ =
  CreateMultiplexResponse'
    { _cmrsMultiplex = Nothing,
      _cmrsResponseStatus = pResponseStatus_
    }

-- | The newly created multiplex.
cmrsMultiplex :: Lens' CreateMultiplexResponse (Maybe Multiplex)
cmrsMultiplex = lens _cmrsMultiplex (\s a -> s {_cmrsMultiplex = a})

-- | -- | The response status code.
cmrsResponseStatus :: Lens' CreateMultiplexResponse Int
cmrsResponseStatus = lens _cmrsResponseStatus (\s a -> s {_cmrsResponseStatus = a})

instance NFData CreateMultiplexResponse
