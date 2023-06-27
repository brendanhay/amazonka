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
-- Module      : Amazonka.SageMaker.Types.ModelDashboardEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDashboardEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointStatus

-- | An endpoint that hosts a model displayed in the Amazon SageMaker Model
-- Dashboard.
--
-- /See:/ 'newModelDashboardEndpoint' smart constructor.
data ModelDashboardEndpoint = ModelDashboardEndpoint'
  { -- | The endpoint name.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | A timestamp that indicates when the endpoint was created.
    creationTime :: Data.POSIX,
    -- | The last time the endpoint was modified.
    lastModifiedTime :: Data.POSIX,
    -- | The endpoint status.
    endpointStatus :: EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDashboardEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'modelDashboardEndpoint_endpointName' - The endpoint name.
--
-- 'endpointArn', 'modelDashboardEndpoint_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
--
-- 'creationTime', 'modelDashboardEndpoint_creationTime' - A timestamp that indicates when the endpoint was created.
--
-- 'lastModifiedTime', 'modelDashboardEndpoint_lastModifiedTime' - The last time the endpoint was modified.
--
-- 'endpointStatus', 'modelDashboardEndpoint_endpointStatus' - The endpoint status.
newModelDashboardEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'endpointStatus'
  EndpointStatus ->
  ModelDashboardEndpoint
newModelDashboardEndpoint
  pEndpointName_
  pEndpointArn_
  pCreationTime_
  pLastModifiedTime_
  pEndpointStatus_ =
    ModelDashboardEndpoint'
      { endpointName =
          pEndpointName_,
        endpointArn = pEndpointArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        endpointStatus = pEndpointStatus_
      }

-- | The endpoint name.
modelDashboardEndpoint_endpointName :: Lens.Lens' ModelDashboardEndpoint Prelude.Text
modelDashboardEndpoint_endpointName = Lens.lens (\ModelDashboardEndpoint' {endpointName} -> endpointName) (\s@ModelDashboardEndpoint' {} a -> s {endpointName = a} :: ModelDashboardEndpoint)

-- | The Amazon Resource Name (ARN) of the endpoint.
modelDashboardEndpoint_endpointArn :: Lens.Lens' ModelDashboardEndpoint Prelude.Text
modelDashboardEndpoint_endpointArn = Lens.lens (\ModelDashboardEndpoint' {endpointArn} -> endpointArn) (\s@ModelDashboardEndpoint' {} a -> s {endpointArn = a} :: ModelDashboardEndpoint)

-- | A timestamp that indicates when the endpoint was created.
modelDashboardEndpoint_creationTime :: Lens.Lens' ModelDashboardEndpoint Prelude.UTCTime
modelDashboardEndpoint_creationTime = Lens.lens (\ModelDashboardEndpoint' {creationTime} -> creationTime) (\s@ModelDashboardEndpoint' {} a -> s {creationTime = a} :: ModelDashboardEndpoint) Prelude.. Data._Time

-- | The last time the endpoint was modified.
modelDashboardEndpoint_lastModifiedTime :: Lens.Lens' ModelDashboardEndpoint Prelude.UTCTime
modelDashboardEndpoint_lastModifiedTime = Lens.lens (\ModelDashboardEndpoint' {lastModifiedTime} -> lastModifiedTime) (\s@ModelDashboardEndpoint' {} a -> s {lastModifiedTime = a} :: ModelDashboardEndpoint) Prelude.. Data._Time

-- | The endpoint status.
modelDashboardEndpoint_endpointStatus :: Lens.Lens' ModelDashboardEndpoint EndpointStatus
modelDashboardEndpoint_endpointStatus = Lens.lens (\ModelDashboardEndpoint' {endpointStatus} -> endpointStatus) (\s@ModelDashboardEndpoint' {} a -> s {endpointStatus = a} :: ModelDashboardEndpoint)

instance Data.FromJSON ModelDashboardEndpoint where
  parseJSON =
    Data.withObject
      "ModelDashboardEndpoint"
      ( \x ->
          ModelDashboardEndpoint'
            Prelude.<$> (x Data..: "EndpointName")
            Prelude.<*> (x Data..: "EndpointArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "EndpointStatus")
      )

instance Prelude.Hashable ModelDashboardEndpoint where
  hashWithSalt _salt ModelDashboardEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` endpointStatus

instance Prelude.NFData ModelDashboardEndpoint where
  rnf ModelDashboardEndpoint' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf endpointStatus
