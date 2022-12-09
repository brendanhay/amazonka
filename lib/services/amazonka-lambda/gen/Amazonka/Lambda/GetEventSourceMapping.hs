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
-- Module      : Amazonka.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an event source mapping. You can get the
-- identifier of a mapping from the output of ListEventSourceMappings.
module Amazonka.Lambda.GetEventSourceMapping
  ( -- * Creating a Request
    GetEventSourceMapping (..),
    newGetEventSourceMapping,

    -- * Request Lenses
    getEventSourceMapping_uuid,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventSourceMapping' smart constructor.
data GetEventSourceMapping = GetEventSourceMapping'
  { -- | The identifier of the event source mapping.
    uuid :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetEventSourceMapping
newGetEventSourceMapping pUUID_ =
  GetEventSourceMapping' {uuid = pUUID_}

-- | The identifier of the event source mapping.
getEventSourceMapping_uuid :: Lens.Lens' GetEventSourceMapping Prelude.Text
getEventSourceMapping_uuid = Lens.lens (\GetEventSourceMapping' {uuid} -> uuid) (\s@GetEventSourceMapping' {} a -> s {uuid = a} :: GetEventSourceMapping)

instance Core.AWSRequest GetEventSourceMapping where
  type
    AWSResponse GetEventSourceMapping =
      EventSourceMappingConfiguration
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetEventSourceMapping where
  hashWithSalt _salt GetEventSourceMapping' {..} =
    _salt `Prelude.hashWithSalt` uuid

instance Prelude.NFData GetEventSourceMapping where
  rnf GetEventSourceMapping' {..} = Prelude.rnf uuid

instance Data.ToHeaders GetEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetEventSourceMapping where
  toPath GetEventSourceMapping' {..} =
    Prelude.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Data.toBS uuid
      ]

instance Data.ToQuery GetEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
