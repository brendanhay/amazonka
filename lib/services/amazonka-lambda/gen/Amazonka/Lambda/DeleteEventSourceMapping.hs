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
-- Module      : Amazonka.Lambda.DeleteEventSourceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an
-- <https://docs.aws.amazon.com/lambda/latest/dg/intro-invocation-modes.html event source mapping>.
-- You can get the identifier of a mapping from the output of
-- ListEventSourceMappings.
--
-- When you delete an event source mapping, it enters a @Deleting@ state
-- and might not be completely deleted for several seconds.
module Amazonka.Lambda.DeleteEventSourceMapping
  ( -- * Creating a Request
    DeleteEventSourceMapping (..),
    newDeleteEventSourceMapping,

    -- * Request Lenses
    deleteEventSourceMapping_uuid,

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

-- | /See:/ 'newDeleteEventSourceMapping' smart constructor.
data DeleteEventSourceMapping = DeleteEventSourceMapping'
  { -- | The identifier of the event source mapping.
    uuid :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventSourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uuid', 'deleteEventSourceMapping_uuid' - The identifier of the event source mapping.
newDeleteEventSourceMapping ::
  -- | 'uuid'
  Prelude.Text ->
  DeleteEventSourceMapping
newDeleteEventSourceMapping pUUID_ =
  DeleteEventSourceMapping' {uuid = pUUID_}

-- | The identifier of the event source mapping.
deleteEventSourceMapping_uuid :: Lens.Lens' DeleteEventSourceMapping Prelude.Text
deleteEventSourceMapping_uuid = Lens.lens (\DeleteEventSourceMapping' {uuid} -> uuid) (\s@DeleteEventSourceMapping' {} a -> s {uuid = a} :: DeleteEventSourceMapping)

instance Core.AWSRequest DeleteEventSourceMapping where
  type
    AWSResponse DeleteEventSourceMapping =
      EventSourceMappingConfiguration
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteEventSourceMapping where
  hashWithSalt _salt DeleteEventSourceMapping' {..} =
    _salt `Prelude.hashWithSalt` uuid

instance Prelude.NFData DeleteEventSourceMapping where
  rnf DeleteEventSourceMapping' {..} = Prelude.rnf uuid

instance Data.ToHeaders DeleteEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteEventSourceMapping where
  toPath DeleteEventSourceMapping' {..} =
    Prelude.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Data.toBS uuid
      ]

instance Data.ToQuery DeleteEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
