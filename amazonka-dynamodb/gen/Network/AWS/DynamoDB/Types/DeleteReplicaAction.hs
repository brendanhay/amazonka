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
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicaAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a replica to be removed.
--
-- /See:/ 'newDeleteReplicaAction' smart constructor.
data DeleteReplicaAction = DeleteReplicaAction'
  { -- | The Region of the replica to be removed.
    regionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'deleteReplicaAction_regionName' - The Region of the replica to be removed.
newDeleteReplicaAction ::
  -- | 'regionName'
  Core.Text ->
  DeleteReplicaAction
newDeleteReplicaAction pRegionName_ =
  DeleteReplicaAction' {regionName = pRegionName_}

-- | The Region of the replica to be removed.
deleteReplicaAction_regionName :: Lens.Lens' DeleteReplicaAction Core.Text
deleteReplicaAction_regionName = Lens.lens (\DeleteReplicaAction' {regionName} -> regionName) (\s@DeleteReplicaAction' {} a -> s {regionName = a} :: DeleteReplicaAction)

instance Core.Hashable DeleteReplicaAction

instance Core.NFData DeleteReplicaAction

instance Core.ToJSON DeleteReplicaAction where
  toJSON DeleteReplicaAction' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RegionName" Core..= regionName)]
      )
