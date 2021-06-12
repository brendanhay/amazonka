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
-- Module      : Network.AWS.SageMaker.Types.EdgeModelStat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgeModelStat where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status of edge devices with this model.
--
-- /See:/ 'newEdgeModelStat' smart constructor.
data EdgeModelStat = EdgeModelStat'
  { -- | The name of the model.
    modelName :: Core.Text,
    -- | The model version.
    modelVersion :: Core.Text,
    -- | The number of devices that have this model version and do not have a
    -- heart beat.
    offlineDeviceCount :: Core.Integer,
    -- | The number of devices that have this model version and have a heart
    -- beat.
    connectedDeviceCount :: Core.Integer,
    -- | The number of devices that have this model version, a heart beat, and
    -- are currently running.
    activeDeviceCount :: Core.Integer,
    -- | The number of devices with this model version and are producing sample
    -- data.
    samplingDeviceCount :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'modelVersion'
  Core.Text ->
  -- | 'offlineDeviceCount'
  Core.Integer ->
  -- | 'connectedDeviceCount'
  Core.Integer ->
  -- | 'activeDeviceCount'
  Core.Integer ->
  -- | 'samplingDeviceCount'
  Core.Integer ->
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
edgeModelStat_modelName :: Lens.Lens' EdgeModelStat Core.Text
edgeModelStat_modelName = Lens.lens (\EdgeModelStat' {modelName} -> modelName) (\s@EdgeModelStat' {} a -> s {modelName = a} :: EdgeModelStat)

-- | The model version.
edgeModelStat_modelVersion :: Lens.Lens' EdgeModelStat Core.Text
edgeModelStat_modelVersion = Lens.lens (\EdgeModelStat' {modelVersion} -> modelVersion) (\s@EdgeModelStat' {} a -> s {modelVersion = a} :: EdgeModelStat)

-- | The number of devices that have this model version and do not have a
-- heart beat.
edgeModelStat_offlineDeviceCount :: Lens.Lens' EdgeModelStat Core.Integer
edgeModelStat_offlineDeviceCount = Lens.lens (\EdgeModelStat' {offlineDeviceCount} -> offlineDeviceCount) (\s@EdgeModelStat' {} a -> s {offlineDeviceCount = a} :: EdgeModelStat)

-- | The number of devices that have this model version and have a heart
-- beat.
edgeModelStat_connectedDeviceCount :: Lens.Lens' EdgeModelStat Core.Integer
edgeModelStat_connectedDeviceCount = Lens.lens (\EdgeModelStat' {connectedDeviceCount} -> connectedDeviceCount) (\s@EdgeModelStat' {} a -> s {connectedDeviceCount = a} :: EdgeModelStat)

-- | The number of devices that have this model version, a heart beat, and
-- are currently running.
edgeModelStat_activeDeviceCount :: Lens.Lens' EdgeModelStat Core.Integer
edgeModelStat_activeDeviceCount = Lens.lens (\EdgeModelStat' {activeDeviceCount} -> activeDeviceCount) (\s@EdgeModelStat' {} a -> s {activeDeviceCount = a} :: EdgeModelStat)

-- | The number of devices with this model version and are producing sample
-- data.
edgeModelStat_samplingDeviceCount :: Lens.Lens' EdgeModelStat Core.Integer
edgeModelStat_samplingDeviceCount = Lens.lens (\EdgeModelStat' {samplingDeviceCount} -> samplingDeviceCount) (\s@EdgeModelStat' {} a -> s {samplingDeviceCount = a} :: EdgeModelStat)

instance Core.FromJSON EdgeModelStat where
  parseJSON =
    Core.withObject
      "EdgeModelStat"
      ( \x ->
          EdgeModelStat'
            Core.<$> (x Core..: "ModelName")
            Core.<*> (x Core..: "ModelVersion")
            Core.<*> (x Core..: "OfflineDeviceCount")
            Core.<*> (x Core..: "ConnectedDeviceCount")
            Core.<*> (x Core..: "ActiveDeviceCount")
            Core.<*> (x Core..: "SamplingDeviceCount")
      )

instance Core.Hashable EdgeModelStat

instance Core.NFData EdgeModelStat
