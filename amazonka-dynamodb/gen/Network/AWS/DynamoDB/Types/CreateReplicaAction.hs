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
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicaAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicaAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a replica to be added.
--
-- /See:/ 'newCreateReplicaAction' smart constructor.
data CreateReplicaAction = CreateReplicaAction'
  { -- | The Region of the replica to be added.
    regionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'createReplicaAction_regionName' - The Region of the replica to be added.
newCreateReplicaAction ::
  -- | 'regionName'
  Core.Text ->
  CreateReplicaAction
newCreateReplicaAction pRegionName_ =
  CreateReplicaAction' {regionName = pRegionName_}

-- | The Region of the replica to be added.
createReplicaAction_regionName :: Lens.Lens' CreateReplicaAction Core.Text
createReplicaAction_regionName = Lens.lens (\CreateReplicaAction' {regionName} -> regionName) (\s@CreateReplicaAction' {} a -> s {regionName = a} :: CreateReplicaAction)

instance Core.Hashable CreateReplicaAction

instance Core.NFData CreateReplicaAction

instance Core.ToJSON CreateReplicaAction where
  toJSON CreateReplicaAction' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RegionName" Core..= regionName)]
      )
