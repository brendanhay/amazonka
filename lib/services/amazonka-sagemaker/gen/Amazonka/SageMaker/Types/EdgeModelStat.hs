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
-- Module      : Amazonka.SageMaker.Types.EdgeModelStat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeModelStat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status of edge devices with this model.
--
-- /See:/ 'newEdgeModelStat' smart constructor.
data EdgeModelStat = EdgeModelStat'
  { -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The model version.
    modelVersion :: Prelude.Text,
    -- | The number of devices that have this model version and do not have a
    -- heart beat.
    offlineDeviceCount :: Prelude.Integer,
    -- | The number of devices that have this model version and have a heart
    -- beat.
    connectedDeviceCount :: Prelude.Integer,
    -- | The number of devices that have this model version, a heart beat, and
    -- are currently running.
    activeDeviceCount :: Prelude.Integer,
    -- | The number of devices with this model version and are producing sample
    -- data.
    samplingDeviceCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeModelStat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'edgeModelStat_modelName' - The name of the model.
--
-- 'modelVersion', 'edgeModelStat_modelVersion' - The model version.
--
-- 'offlineDeviceCount', 'edgeModelStat_offlineDeviceCount' - The number of devices that have this model version and do not have a
-- heart beat.
--
-- 'connectedDeviceCount', 'edgeModelStat_connectedDeviceCount' - The number of devices that have this model version and have a heart
-- beat.
--
-- 'activeDeviceCount', 'edgeModelStat_activeDeviceCount' - The number of devices that have this model version, a heart beat, and
-- are currently running.
--
-- 'samplingDeviceCount', 'edgeModelStat_samplingDeviceCount' - The number of devices with this model version and are producing sample
-- data.
newEdgeModelStat ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  -- | 'offlineDeviceCount'
  Prelude.Integer ->
  -- | 'connectedDeviceCount'
  Prelude.Integer ->
  -- | 'activeDeviceCount'
  Prelude.Integer ->
  -- | 'samplingDeviceCount'
  Prelude.Integer ->
  EdgeModelStat
newEdgeModelStat
  pModelName_
  pModelVersion_
  pOfflineDeviceCount_
  pConnectedDeviceCount_
  pActiveDeviceCount_
  pSamplingDeviceCount_ =
    EdgeModelStat'
      { modelName = pModelName_,
        modelVersion = pModelVersion_,
        offlineDeviceCount = pOfflineDeviceCount_,
        connectedDeviceCount = pConnectedDeviceCount_,
        activeDeviceCount = pActiveDeviceCount_,
        samplingDeviceCount = pSamplingDeviceCount_
      }

-- | The name of the model.
edgeModelStat_modelName :: Lens.Lens' EdgeModelStat Prelude.Text
edgeModelStat_modelName = Lens.lens (\EdgeModelStat' {modelName} -> modelName) (\s@EdgeModelStat' {} a -> s {modelName = a} :: EdgeModelStat)

-- | The model version.
edgeModelStat_modelVersion :: Lens.Lens' EdgeModelStat Prelude.Text
edgeModelStat_modelVersion = Lens.lens (\EdgeModelStat' {modelVersion} -> modelVersion) (\s@EdgeModelStat' {} a -> s {modelVersion = a} :: EdgeModelStat)

-- | The number of devices that have this model version and do not have a
-- heart beat.
edgeModelStat_offlineDeviceCount :: Lens.Lens' EdgeModelStat Prelude.Integer
edgeModelStat_offlineDeviceCount = Lens.lens (\EdgeModelStat' {offlineDeviceCount} -> offlineDeviceCount) (\s@EdgeModelStat' {} a -> s {offlineDeviceCount = a} :: EdgeModelStat)

-- | The number of devices that have this model version and have a heart
-- beat.
edgeModelStat_connectedDeviceCount :: Lens.Lens' EdgeModelStat Prelude.Integer
edgeModelStat_connectedDeviceCount = Lens.lens (\EdgeModelStat' {connectedDeviceCount} -> connectedDeviceCount) (\s@EdgeModelStat' {} a -> s {connectedDeviceCount = a} :: EdgeModelStat)

-- | The number of devices that have this model version, a heart beat, and
-- are currently running.
edgeModelStat_activeDeviceCount :: Lens.Lens' EdgeModelStat Prelude.Integer
edgeModelStat_activeDeviceCount = Lens.lens (\EdgeModelStat' {activeDeviceCount} -> activeDeviceCount) (\s@EdgeModelStat' {} a -> s {activeDeviceCount = a} :: EdgeModelStat)

-- | The number of devices with this model version and are producing sample
-- data.
edgeModelStat_samplingDeviceCount :: Lens.Lens' EdgeModelStat Prelude.Integer
edgeModelStat_samplingDeviceCount = Lens.lens (\EdgeModelStat' {samplingDeviceCount} -> samplingDeviceCount) (\s@EdgeModelStat' {} a -> s {samplingDeviceCount = a} :: EdgeModelStat)

instance Data.FromJSON EdgeModelStat where
  parseJSON =
    Data.withObject
      "EdgeModelStat"
      ( \x ->
          EdgeModelStat'
            Prelude.<$> (x Data..: "ModelName")
            Prelude.<*> (x Data..: "ModelVersion")
            Prelude.<*> (x Data..: "OfflineDeviceCount")
            Prelude.<*> (x Data..: "ConnectedDeviceCount")
            Prelude.<*> (x Data..: "ActiveDeviceCount")
            Prelude.<*> (x Data..: "SamplingDeviceCount")
      )

instance Prelude.Hashable EdgeModelStat where
  hashWithSalt _salt EdgeModelStat' {..} =
    _salt
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` offlineDeviceCount
      `Prelude.hashWithSalt` connectedDeviceCount
      `Prelude.hashWithSalt` activeDeviceCount
      `Prelude.hashWithSalt` samplingDeviceCount

instance Prelude.NFData EdgeModelStat where
  rnf EdgeModelStat' {..} =
    Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf offlineDeviceCount
      `Prelude.seq` Prelude.rnf connectedDeviceCount
      `Prelude.seq` Prelude.rnf activeDeviceCount
      `Prelude.seq` Prelude.rnf samplingDeviceCount
