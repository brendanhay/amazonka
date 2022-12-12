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
-- Module      : Amazonka.Panorama.Types.NodeInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.NodeInstanceStatus
import qualified Amazonka.Prelude as Prelude

-- | A node instance.
--
-- /See:/ 'newNodeInstance' smart constructor.
data NodeInstance = NodeInstance'
  { -- | The node\'s ID.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s name.
    nodeName :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s package name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s package patch version.
    packagePatchVersion :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s package version.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s current status.
    currentStatus :: NodeInstanceStatus,
    -- | The instance\'s ID.
    nodeInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeId', 'nodeInstance_nodeId' - The node\'s ID.
--
-- 'nodeName', 'nodeInstance_nodeName' - The instance\'s name.
--
-- 'packageName', 'nodeInstance_packageName' - The instance\'s package name.
--
-- 'packagePatchVersion', 'nodeInstance_packagePatchVersion' - The instance\'s package patch version.
--
-- 'packageVersion', 'nodeInstance_packageVersion' - The instance\'s package version.
--
-- 'currentStatus', 'nodeInstance_currentStatus' - The instance\'s current status.
--
-- 'nodeInstanceId', 'nodeInstance_nodeInstanceId' - The instance\'s ID.
newNodeInstance ::
  -- | 'currentStatus'
  NodeInstanceStatus ->
  -- | 'nodeInstanceId'
  Prelude.Text ->
  NodeInstance
newNodeInstance pCurrentStatus_ pNodeInstanceId_ =
  NodeInstance'
    { nodeId = Prelude.Nothing,
      nodeName = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packagePatchVersion = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      currentStatus = pCurrentStatus_,
      nodeInstanceId = pNodeInstanceId_
    }

-- | The node\'s ID.
nodeInstance_nodeId :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_nodeId = Lens.lens (\NodeInstance' {nodeId} -> nodeId) (\s@NodeInstance' {} a -> s {nodeId = a} :: NodeInstance)

-- | The instance\'s name.
nodeInstance_nodeName :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_nodeName = Lens.lens (\NodeInstance' {nodeName} -> nodeName) (\s@NodeInstance' {} a -> s {nodeName = a} :: NodeInstance)

-- | The instance\'s package name.
nodeInstance_packageName :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packageName = Lens.lens (\NodeInstance' {packageName} -> packageName) (\s@NodeInstance' {} a -> s {packageName = a} :: NodeInstance)

-- | The instance\'s package patch version.
nodeInstance_packagePatchVersion :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packagePatchVersion = Lens.lens (\NodeInstance' {packagePatchVersion} -> packagePatchVersion) (\s@NodeInstance' {} a -> s {packagePatchVersion = a} :: NodeInstance)

-- | The instance\'s package version.
nodeInstance_packageVersion :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packageVersion = Lens.lens (\NodeInstance' {packageVersion} -> packageVersion) (\s@NodeInstance' {} a -> s {packageVersion = a} :: NodeInstance)

-- | The instance\'s current status.
nodeInstance_currentStatus :: Lens.Lens' NodeInstance NodeInstanceStatus
nodeInstance_currentStatus = Lens.lens (\NodeInstance' {currentStatus} -> currentStatus) (\s@NodeInstance' {} a -> s {currentStatus = a} :: NodeInstance)

-- | The instance\'s ID.
nodeInstance_nodeInstanceId :: Lens.Lens' NodeInstance Prelude.Text
nodeInstance_nodeInstanceId = Lens.lens (\NodeInstance' {nodeInstanceId} -> nodeInstanceId) (\s@NodeInstance' {} a -> s {nodeInstanceId = a} :: NodeInstance)

instance Data.FromJSON NodeInstance where
  parseJSON =
    Data.withObject
      "NodeInstance"
      ( \x ->
          NodeInstance'
            Prelude.<$> (x Data..:? "NodeId")
            Prelude.<*> (x Data..:? "NodeName")
            Prelude.<*> (x Data..:? "PackageName")
            Prelude.<*> (x Data..:? "PackagePatchVersion")
            Prelude.<*> (x Data..:? "PackageVersion")
            Prelude.<*> (x Data..: "CurrentStatus")
            Prelude.<*> (x Data..: "NodeInstanceId")
      )

instance Prelude.Hashable NodeInstance where
  hashWithSalt _salt NodeInstance' {..} =
    _salt `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` nodeName
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packagePatchVersion
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` currentStatus
      `Prelude.hashWithSalt` nodeInstanceId

instance Prelude.NFData NodeInstance where
  rnf NodeInstance' {..} =
    Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf nodeName
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packagePatchVersion
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf currentStatus
      `Prelude.seq` Prelude.rnf nodeInstanceId
