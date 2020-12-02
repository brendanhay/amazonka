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
-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an event source mapping. You can get the identifier of a mapping from the output of 'ListEventSourceMappings' .
module Network.AWS.Lambda.GetEventSourceMapping
  ( -- * Creating a Request
    getEventSourceMapping,
    GetEventSourceMapping,

    -- * Request Lenses
    gesmUUId,

    -- * Destructuring the Response
    eventSourceMappingConfiguration,
    EventSourceMappingConfiguration,

    -- * Response Lenses
    esmcEventSourceARN,
    esmcState,
    esmcStartingPositionTimestamp,
    esmcFunctionARN,
    esmcTopics,
    esmcQueues,
    esmcBisectBatchOnFunctionError,
    esmcUUId,
    esmcParallelizationFactor,
    esmcLastProcessingResult,
    esmcMaximumRetryAttempts,
    esmcBatchSize,
    esmcStateTransitionReason,
    esmcMaximumBatchingWindowInSeconds,
    esmcSourceAccessConfigurations,
    esmcMaximumRecordAgeInSeconds,
    esmcLastModified,
    esmcDestinationConfig,
    esmcStartingPosition,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEventSourceMapping' smart constructor.
newtype GetEventSourceMapping = GetEventSourceMapping'
  { _gesmUUId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesmUUId' - The identifier of the event source mapping.
getEventSourceMapping ::
  -- | 'gesmUUId'
  Text ->
  GetEventSourceMapping
getEventSourceMapping pUUId_ =
  GetEventSourceMapping' {_gesmUUId = pUUId_}

-- | The identifier of the event source mapping.
gesmUUId :: Lens' GetEventSourceMapping Text
gesmUUId = lens _gesmUUId (\s a -> s {_gesmUUId = a})

instance AWSRequest GetEventSourceMapping where
  type Rs GetEventSourceMapping = EventSourceMappingConfiguration
  request = get lambda
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable GetEventSourceMapping

instance NFData GetEventSourceMapping

instance ToHeaders GetEventSourceMapping where
  toHeaders = const mempty

instance ToPath GetEventSourceMapping where
  toPath GetEventSourceMapping' {..} =
    mconcat ["/2015-03-31/event-source-mappings/", toBS _gesmUUId]

instance ToQuery GetEventSourceMapping where
  toQuery = const mempty
