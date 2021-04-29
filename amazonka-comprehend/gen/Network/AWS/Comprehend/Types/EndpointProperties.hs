{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Comprehend.Types.EndpointProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointProperties where

import Network.AWS.Comprehend.Types.EndpointStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies information about the specified endpoint.
--
-- /See:/ 'newEndpointProperties' smart constructor.
data EndpointProperties = EndpointProperties'
  { -- | The number of inference units currently used by the model using this
    -- endpoint.
    currentInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the status of the endpoint. Because the endpoint updates and
    -- creation are asynchronous, so customers will need to wait for the
    -- endpoint to be @Ready@ status before making inference requests.
    status :: Prelude.Maybe EndpointStatus,
    -- | The creation date and time of the endpoint.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | Specifies a reason for failure in cases of @Failed@ status.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint is
    -- attached.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the endpoint was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentInferenceUnits', 'endpointProperties_currentInferenceUnits' - The number of inference units currently used by the model using this
-- endpoint.
--
-- 'status', 'endpointProperties_status' - Specifies the status of the endpoint. Because the endpoint updates and
-- creation are asynchronous, so customers will need to wait for the
-- endpoint to be @Ready@ status before making inference requests.
--
-- 'creationTime', 'endpointProperties_creationTime' - The creation date and time of the endpoint.
--
-- 'desiredInferenceUnits', 'endpointProperties_desiredInferenceUnits' - The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
--
-- 'message', 'endpointProperties_message' - Specifies a reason for failure in cases of @Failed@ status.
--
-- 'modelArn', 'endpointProperties_modelArn' - The Amazon Resource Number (ARN) of the model to which the endpoint is
-- attached.
--
-- 'lastModifiedTime', 'endpointProperties_lastModifiedTime' - The date and time that the endpoint was last modified.
--
-- 'endpointArn', 'endpointProperties_endpointArn' - The Amazon Resource Number (ARN) of the endpoint.
newEndpointProperties ::
  EndpointProperties
newEndpointProperties =
  EndpointProperties'
    { currentInferenceUnits =
        Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      desiredInferenceUnits = Prelude.Nothing,
      message = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | The number of inference units currently used by the model using this
-- endpoint.
endpointProperties_currentInferenceUnits :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Natural)
endpointProperties_currentInferenceUnits = Lens.lens (\EndpointProperties' {currentInferenceUnits} -> currentInferenceUnits) (\s@EndpointProperties' {} a -> s {currentInferenceUnits = a} :: EndpointProperties)

-- | Specifies the status of the endpoint. Because the endpoint updates and
-- creation are asynchronous, so customers will need to wait for the
-- endpoint to be @Ready@ status before making inference requests.
endpointProperties_status :: Lens.Lens' EndpointProperties (Prelude.Maybe EndpointStatus)
endpointProperties_status = Lens.lens (\EndpointProperties' {status} -> status) (\s@EndpointProperties' {} a -> s {status = a} :: EndpointProperties)

-- | The creation date and time of the endpoint.
endpointProperties_creationTime :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.UTCTime)
endpointProperties_creationTime = Lens.lens (\EndpointProperties' {creationTime} -> creationTime) (\s@EndpointProperties' {} a -> s {creationTime = a} :: EndpointProperties) Prelude.. Lens.mapping Prelude._Time

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
endpointProperties_desiredInferenceUnits :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Natural)
endpointProperties_desiredInferenceUnits = Lens.lens (\EndpointProperties' {desiredInferenceUnits} -> desiredInferenceUnits) (\s@EndpointProperties' {} a -> s {desiredInferenceUnits = a} :: EndpointProperties)

-- | Specifies a reason for failure in cases of @Failed@ status.
endpointProperties_message :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_message = Lens.lens (\EndpointProperties' {message} -> message) (\s@EndpointProperties' {} a -> s {message = a} :: EndpointProperties)

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is
-- attached.
endpointProperties_modelArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_modelArn = Lens.lens (\EndpointProperties' {modelArn} -> modelArn) (\s@EndpointProperties' {} a -> s {modelArn = a} :: EndpointProperties)

-- | The date and time that the endpoint was last modified.
endpointProperties_lastModifiedTime :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.UTCTime)
endpointProperties_lastModifiedTime = Lens.lens (\EndpointProperties' {lastModifiedTime} -> lastModifiedTime) (\s@EndpointProperties' {} a -> s {lastModifiedTime = a} :: EndpointProperties) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Number (ARN) of the endpoint.
endpointProperties_endpointArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_endpointArn = Lens.lens (\EndpointProperties' {endpointArn} -> endpointArn) (\s@EndpointProperties' {} a -> s {endpointArn = a} :: EndpointProperties)

instance Prelude.FromJSON EndpointProperties where
  parseJSON =
    Prelude.withObject
      "EndpointProperties"
      ( \x ->
          EndpointProperties'
            Prelude.<$> (x Prelude..:? "CurrentInferenceUnits")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "DesiredInferenceUnits")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "ModelArn")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "EndpointArn")
      )

instance Prelude.Hashable EndpointProperties

instance Prelude.NFData EndpointProperties
