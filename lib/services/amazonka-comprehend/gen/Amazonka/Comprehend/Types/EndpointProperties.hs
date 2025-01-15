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
-- Module      : Amazonka.Comprehend.Types.EndpointProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EndpointProperties where

import Amazonka.Comprehend.Types.EndpointStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies information about the specified endpoint. For information
-- about endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
--
-- /See:/ 'newEndpointProperties' smart constructor.
data EndpointProperties = EndpointProperties'
  { -- | The creation date and time of the endpoint.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The number of inference units currently used by the model using this
    -- endpoint.
    currentInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to trained custom
    -- models encrypted with a customer managed key (ModelKmsKeyId).
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Data access role ARN to use in case the new model is encrypted with a
    -- customer KMS key.
    desiredDataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | ARN of the new model to use for updating an existing endpoint. This ARN
    -- is going to be different from the model ARN when the update is in
    -- progress
    desiredModelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the endpoint was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies a reason for failure in cases of @Failed@ status.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint is
    -- attached.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of the endpoint. Because the endpoint updates and
    -- creation are asynchronous, so customers will need to wait for the
    -- endpoint to be @Ready@ status before making inference requests.
    status :: Prelude.Maybe EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'endpointProperties_creationTime' - The creation date and time of the endpoint.
--
-- 'currentInferenceUnits', 'endpointProperties_currentInferenceUnits' - The number of inference units currently used by the model using this
-- endpoint.
--
-- 'dataAccessRoleArn', 'endpointProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to trained custom
-- models encrypted with a customer managed key (ModelKmsKeyId).
--
-- 'desiredDataAccessRoleArn', 'endpointProperties_desiredDataAccessRoleArn' - Data access role ARN to use in case the new model is encrypted with a
-- customer KMS key.
--
-- 'desiredInferenceUnits', 'endpointProperties_desiredInferenceUnits' - The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
--
-- 'desiredModelArn', 'endpointProperties_desiredModelArn' - ARN of the new model to use for updating an existing endpoint. This ARN
-- is going to be different from the model ARN when the update is in
-- progress
--
-- 'endpointArn', 'endpointProperties_endpointArn' - The Amazon Resource Number (ARN) of the endpoint.
--
-- 'lastModifiedTime', 'endpointProperties_lastModifiedTime' - The date and time that the endpoint was last modified.
--
-- 'message', 'endpointProperties_message' - Specifies a reason for failure in cases of @Failed@ status.
--
-- 'modelArn', 'endpointProperties_modelArn' - The Amazon Resource Number (ARN) of the model to which the endpoint is
-- attached.
--
-- 'status', 'endpointProperties_status' - Specifies the status of the endpoint. Because the endpoint updates and
-- creation are asynchronous, so customers will need to wait for the
-- endpoint to be @Ready@ status before making inference requests.
newEndpointProperties ::
  EndpointProperties
newEndpointProperties =
  EndpointProperties'
    { creationTime = Prelude.Nothing,
      currentInferenceUnits = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      desiredDataAccessRoleArn = Prelude.Nothing,
      desiredInferenceUnits = Prelude.Nothing,
      desiredModelArn = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      message = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The creation date and time of the endpoint.
endpointProperties_creationTime :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.UTCTime)
endpointProperties_creationTime = Lens.lens (\EndpointProperties' {creationTime} -> creationTime) (\s@EndpointProperties' {} a -> s {creationTime = a} :: EndpointProperties) Prelude.. Lens.mapping Data._Time

-- | The number of inference units currently used by the model using this
-- endpoint.
endpointProperties_currentInferenceUnits :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Natural)
endpointProperties_currentInferenceUnits = Lens.lens (\EndpointProperties' {currentInferenceUnits} -> currentInferenceUnits) (\s@EndpointProperties' {} a -> s {currentInferenceUnits = a} :: EndpointProperties)

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to trained custom
-- models encrypted with a customer managed key (ModelKmsKeyId).
endpointProperties_dataAccessRoleArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_dataAccessRoleArn = Lens.lens (\EndpointProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EndpointProperties' {} a -> s {dataAccessRoleArn = a} :: EndpointProperties)

-- | Data access role ARN to use in case the new model is encrypted with a
-- customer KMS key.
endpointProperties_desiredDataAccessRoleArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_desiredDataAccessRoleArn = Lens.lens (\EndpointProperties' {desiredDataAccessRoleArn} -> desiredDataAccessRoleArn) (\s@EndpointProperties' {} a -> s {desiredDataAccessRoleArn = a} :: EndpointProperties)

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
endpointProperties_desiredInferenceUnits :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Natural)
endpointProperties_desiredInferenceUnits = Lens.lens (\EndpointProperties' {desiredInferenceUnits} -> desiredInferenceUnits) (\s@EndpointProperties' {} a -> s {desiredInferenceUnits = a} :: EndpointProperties)

-- | ARN of the new model to use for updating an existing endpoint. This ARN
-- is going to be different from the model ARN when the update is in
-- progress
endpointProperties_desiredModelArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_desiredModelArn = Lens.lens (\EndpointProperties' {desiredModelArn} -> desiredModelArn) (\s@EndpointProperties' {} a -> s {desiredModelArn = a} :: EndpointProperties)

-- | The Amazon Resource Number (ARN) of the endpoint.
endpointProperties_endpointArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_endpointArn = Lens.lens (\EndpointProperties' {endpointArn} -> endpointArn) (\s@EndpointProperties' {} a -> s {endpointArn = a} :: EndpointProperties)

-- | The date and time that the endpoint was last modified.
endpointProperties_lastModifiedTime :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.UTCTime)
endpointProperties_lastModifiedTime = Lens.lens (\EndpointProperties' {lastModifiedTime} -> lastModifiedTime) (\s@EndpointProperties' {} a -> s {lastModifiedTime = a} :: EndpointProperties) Prelude.. Lens.mapping Data._Time

-- | Specifies a reason for failure in cases of @Failed@ status.
endpointProperties_message :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_message = Lens.lens (\EndpointProperties' {message} -> message) (\s@EndpointProperties' {} a -> s {message = a} :: EndpointProperties)

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is
-- attached.
endpointProperties_modelArn :: Lens.Lens' EndpointProperties (Prelude.Maybe Prelude.Text)
endpointProperties_modelArn = Lens.lens (\EndpointProperties' {modelArn} -> modelArn) (\s@EndpointProperties' {} a -> s {modelArn = a} :: EndpointProperties)

-- | Specifies the status of the endpoint. Because the endpoint updates and
-- creation are asynchronous, so customers will need to wait for the
-- endpoint to be @Ready@ status before making inference requests.
endpointProperties_status :: Lens.Lens' EndpointProperties (Prelude.Maybe EndpointStatus)
endpointProperties_status = Lens.lens (\EndpointProperties' {status} -> status) (\s@EndpointProperties' {} a -> s {status = a} :: EndpointProperties)

instance Data.FromJSON EndpointProperties where
  parseJSON =
    Data.withObject
      "EndpointProperties"
      ( \x ->
          EndpointProperties'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CurrentInferenceUnits")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "DesiredDataAccessRoleArn")
            Prelude.<*> (x Data..:? "DesiredInferenceUnits")
            Prelude.<*> (x Data..:? "DesiredModelArn")
            Prelude.<*> (x Data..:? "EndpointArn")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ModelArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable EndpointProperties where
  hashWithSalt _salt EndpointProperties' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` currentInferenceUnits
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` desiredDataAccessRoleArn
      `Prelude.hashWithSalt` desiredInferenceUnits
      `Prelude.hashWithSalt` desiredModelArn
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData EndpointProperties where
  rnf EndpointProperties' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf currentInferenceUnits `Prelude.seq`
        Prelude.rnf dataAccessRoleArn `Prelude.seq`
          Prelude.rnf desiredDataAccessRoleArn `Prelude.seq`
            Prelude.rnf desiredInferenceUnits `Prelude.seq`
              Prelude.rnf desiredModelArn `Prelude.seq`
                Prelude.rnf endpointArn `Prelude.seq`
                  Prelude.rnf lastModifiedTime `Prelude.seq`
                    Prelude.rnf message `Prelude.seq`
                      Prelude.rnf modelArn `Prelude.seq`
                        Prelude.rnf status
