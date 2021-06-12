{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an event source mapping. You can get the
-- identifier of a mapping from the output of ListEventSourceMappings.
module Network.AWS.Lambda.GetEventSourceMapping
  ( -- * Creating a Request
    GetEventSourceMapping (..),
    newGetEventSourceMapping,

    -- * Request Lenses
    getEventSourceMapping_uuid,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEventSourceMapping' smart constructor.
data GetEventSourceMapping = GetEventSourceMapping'
  { -- | The identifier of the event source mapping.
    uuid :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEventSourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uuid', 'getEventSourceMapping_uuid' - The identifier of the event source mapping.
newGetEventSourceMapping ::
  -- | 'uuid'
  Core.Text ->
  GetEventSourceMapping
newGetEventSourceMapping pUUID_ =
  GetEventSourceMapping' {uuid = pUUID_}

-- | The identifier of the event source mapping.
getEventSourceMapping_uuid :: Lens.Lens' GetEventSourceMapping Core.Text
getEventSourceMapping_uuid = Lens.lens (\GetEventSourceMapping' {uuid} -> uuid) (\s@GetEventSourceMapping' {} a -> s {uuid = a} :: GetEventSourceMapping)

instance Core.AWSRequest GetEventSourceMapping where
  type
    AWSResponse GetEventSourceMapping =
      EventSourceMappingConfiguration
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetEventSourceMapping

instance Core.NFData GetEventSourceMapping

instance Core.ToHeaders GetEventSourceMapping where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetEventSourceMapping where
  toPath GetEventSourceMapping' {..} =
    Core.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Core.toBS uuid
      ]

instance Core.ToQuery GetEventSourceMapping where
  toQuery = Core.const Core.mempty
