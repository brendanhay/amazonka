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
-- Module      : Amazonka.SageMaker.Types.EndpointMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointStatus

-- | The metadata of the endpoint.
--
-- /See:/ 'newEndpointMetadata' smart constructor.
data EndpointMetadata = EndpointMetadata'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Prelude.Maybe Prelude.Text,
    -- | The status of the endpoint. For possible values of the status of an
    -- endpoint, see EndpointSummary$EndpointStatus.
    endpointStatus :: Prelude.Maybe EndpointStatus,
    -- | If the status of the endpoint is @Failed@, or the status is @InService@
    -- but update operation fails, this provides the reason why it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigName', 'endpointMetadata_endpointConfigName' - The name of the endpoint configuration.
--
-- 'endpointStatus', 'endpointMetadata_endpointStatus' - The status of the endpoint. For possible values of the status of an
-- endpoint, see EndpointSummary$EndpointStatus.
--
-- 'failureReason', 'endpointMetadata_failureReason' - If the status of the endpoint is @Failed@, or the status is @InService@
-- but update operation fails, this provides the reason why it failed.
--
-- 'endpointName', 'endpointMetadata_endpointName' - The name of the endpoint.
newEndpointMetadata ::
  -- | 'endpointName'
  Prelude.Text ->
  EndpointMetadata
newEndpointMetadata pEndpointName_ =
  EndpointMetadata'
    { endpointConfigName =
        Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      endpointName = pEndpointName_
    }

-- | The name of the endpoint configuration.
endpointMetadata_endpointConfigName :: Lens.Lens' EndpointMetadata (Prelude.Maybe Prelude.Text)
endpointMetadata_endpointConfigName = Lens.lens (\EndpointMetadata' {endpointConfigName} -> endpointConfigName) (\s@EndpointMetadata' {} a -> s {endpointConfigName = a} :: EndpointMetadata)

-- | The status of the endpoint. For possible values of the status of an
-- endpoint, see EndpointSummary$EndpointStatus.
endpointMetadata_endpointStatus :: Lens.Lens' EndpointMetadata (Prelude.Maybe EndpointStatus)
endpointMetadata_endpointStatus = Lens.lens (\EndpointMetadata' {endpointStatus} -> endpointStatus) (\s@EndpointMetadata' {} a -> s {endpointStatus = a} :: EndpointMetadata)

-- | If the status of the endpoint is @Failed@, or the status is @InService@
-- but update operation fails, this provides the reason why it failed.
endpointMetadata_failureReason :: Lens.Lens' EndpointMetadata (Prelude.Maybe Prelude.Text)
endpointMetadata_failureReason = Lens.lens (\EndpointMetadata' {failureReason} -> failureReason) (\s@EndpointMetadata' {} a -> s {failureReason = a} :: EndpointMetadata)

-- | The name of the endpoint.
endpointMetadata_endpointName :: Lens.Lens' EndpointMetadata Prelude.Text
endpointMetadata_endpointName = Lens.lens (\EndpointMetadata' {endpointName} -> endpointName) (\s@EndpointMetadata' {} a -> s {endpointName = a} :: EndpointMetadata)

instance Data.FromJSON EndpointMetadata where
  parseJSON =
    Data.withObject
      "EndpointMetadata"
      ( \x ->
          EndpointMetadata'
            Prelude.<$> (x Data..:? "EndpointConfigName")
            Prelude.<*> (x Data..:? "EndpointStatus")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "EndpointName")
      )

instance Prelude.Hashable EndpointMetadata where
  hashWithSalt _salt EndpointMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` endpointConfigName
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` endpointName

instance Prelude.NFData EndpointMetadata where
  rnf EndpointMetadata' {..} =
    Prelude.rnf endpointConfigName
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf endpointName
