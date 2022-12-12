{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pipes.Types.PipeTargetEventBridgeEventBusParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetEventBridgeEventBusParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an EventBridge event bus as a target.
--
-- /See:/ 'newPipeTargetEventBridgeEventBusParameters' smart constructor.
data PipeTargetEventBridgeEventBusParameters = PipeTargetEventBridgeEventBusParameters'
  { -- | A free-form string, with a maximum of 128 characters, used to decide
    -- what fields to expect in the event detail.
    detailType :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
    -- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
    -- EndpointId is @abcde.veo@.
    --
    -- When using Java, you must include @auth-crt@ on the class path.
    endpointId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Amazon Web Services resources, identified by Amazon Resource Name (ARN),
    -- which the event primarily concerns. Any number, including zero, may be
    -- present.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The source of the event.
    source :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The time stamp of the event, per
    -- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
    -- is provided, the time stamp of the
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
    -- call is used.
    time :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetEventBridgeEventBusParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailType', 'pipeTargetEventBridgeEventBusParameters_detailType' - A free-form string, with a maximum of 128 characters, used to decide
-- what fields to expect in the event detail.
--
-- 'endpointId', 'pipeTargetEventBridgeEventBusParameters_endpointId' - The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
-- EndpointId is @abcde.veo@.
--
-- When using Java, you must include @auth-crt@ on the class path.
--
-- 'resources', 'pipeTargetEventBridgeEventBusParameters_resources' - Amazon Web Services resources, identified by Amazon Resource Name (ARN),
-- which the event primarily concerns. Any number, including zero, may be
-- present.
--
-- 'source', 'pipeTargetEventBridgeEventBusParameters_source' - The source of the event.
--
-- 'time', 'pipeTargetEventBridgeEventBusParameters_time' - The time stamp of the event, per
-- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
-- is provided, the time stamp of the
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- call is used.
newPipeTargetEventBridgeEventBusParameters ::
  PipeTargetEventBridgeEventBusParameters
newPipeTargetEventBridgeEventBusParameters =
  PipeTargetEventBridgeEventBusParameters'
    { detailType =
        Prelude.Nothing,
      endpointId = Prelude.Nothing,
      resources = Prelude.Nothing,
      source = Prelude.Nothing,
      time = Prelude.Nothing
    }

-- | A free-form string, with a maximum of 128 characters, used to decide
-- what fields to expect in the event detail.
pipeTargetEventBridgeEventBusParameters_detailType :: Lens.Lens' PipeTargetEventBridgeEventBusParameters (Prelude.Maybe Prelude.Text)
pipeTargetEventBridgeEventBusParameters_detailType = Lens.lens (\PipeTargetEventBridgeEventBusParameters' {detailType} -> detailType) (\s@PipeTargetEventBridgeEventBusParameters' {} a -> s {detailType = a} :: PipeTargetEventBridgeEventBusParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
-- EndpointId is @abcde.veo@.
--
-- When using Java, you must include @auth-crt@ on the class path.
pipeTargetEventBridgeEventBusParameters_endpointId :: Lens.Lens' PipeTargetEventBridgeEventBusParameters (Prelude.Maybe Prelude.Text)
pipeTargetEventBridgeEventBusParameters_endpointId = Lens.lens (\PipeTargetEventBridgeEventBusParameters' {endpointId} -> endpointId) (\s@PipeTargetEventBridgeEventBusParameters' {} a -> s {endpointId = a} :: PipeTargetEventBridgeEventBusParameters) Prelude.. Lens.mapping Data._Sensitive

-- | Amazon Web Services resources, identified by Amazon Resource Name (ARN),
-- which the event primarily concerns. Any number, including zero, may be
-- present.
pipeTargetEventBridgeEventBusParameters_resources :: Lens.Lens' PipeTargetEventBridgeEventBusParameters (Prelude.Maybe [Prelude.Text])
pipeTargetEventBridgeEventBusParameters_resources = Lens.lens (\PipeTargetEventBridgeEventBusParameters' {resources} -> resources) (\s@PipeTargetEventBridgeEventBusParameters' {} a -> s {resources = a} :: PipeTargetEventBridgeEventBusParameters) Prelude.. Lens.mapping Lens.coerced

-- | The source of the event.
pipeTargetEventBridgeEventBusParameters_source :: Lens.Lens' PipeTargetEventBridgeEventBusParameters (Prelude.Maybe Prelude.Text)
pipeTargetEventBridgeEventBusParameters_source = Lens.lens (\PipeTargetEventBridgeEventBusParameters' {source} -> source) (\s@PipeTargetEventBridgeEventBusParameters' {} a -> s {source = a} :: PipeTargetEventBridgeEventBusParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The time stamp of the event, per
-- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
-- is provided, the time stamp of the
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- call is used.
pipeTargetEventBridgeEventBusParameters_time :: Lens.Lens' PipeTargetEventBridgeEventBusParameters (Prelude.Maybe Prelude.Text)
pipeTargetEventBridgeEventBusParameters_time = Lens.lens (\PipeTargetEventBridgeEventBusParameters' {time} -> time) (\s@PipeTargetEventBridgeEventBusParameters' {} a -> s {time = a} :: PipeTargetEventBridgeEventBusParameters)

instance
  Data.FromJSON
    PipeTargetEventBridgeEventBusParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetEventBridgeEventBusParameters"
      ( \x ->
          PipeTargetEventBridgeEventBusParameters'
            Prelude.<$> (x Data..:? "DetailType")
            Prelude.<*> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "Resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Time")
      )

instance
  Prelude.Hashable
    PipeTargetEventBridgeEventBusParameters
  where
  hashWithSalt
    _salt
    PipeTargetEventBridgeEventBusParameters' {..} =
      _salt `Prelude.hashWithSalt` detailType
        `Prelude.hashWithSalt` endpointId
        `Prelude.hashWithSalt` resources
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` time

instance
  Prelude.NFData
    PipeTargetEventBridgeEventBusParameters
  where
  rnf PipeTargetEventBridgeEventBusParameters' {..} =
    Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf time

instance
  Data.ToJSON
    PipeTargetEventBridgeEventBusParameters
  where
  toJSON PipeTargetEventBridgeEventBusParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DetailType" Data..=) Prelude.<$> detailType,
            ("EndpointId" Data..=) Prelude.<$> endpointId,
            ("Resources" Data..=) Prelude.<$> resources,
            ("Source" Data..=) Prelude.<$> source,
            ("Time" Data..=) Prelude.<$> time
          ]
      )
