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
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicaAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a replica to be removed.
--
-- /See:/ 'newDeleteReplicaAction' smart constructor.
data DeleteReplicaAction = DeleteReplicaAction'
  { -- | The Region of the replica to be removed.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteReplicaAction
newDeleteReplicaAction pRegionName_ =
  DeleteReplicaAction' {regionName = pRegionName_}

-- | The Region of the replica to be removed.
deleteReplicaAction_regionName :: Lens.Lens' DeleteReplicaAction Prelude.Text
deleteReplicaAction_regionName = Lens.lens (\DeleteReplicaAction' {regionName} -> regionName) (\s@DeleteReplicaAction' {} a -> s {regionName = a} :: DeleteReplicaAction)

instance Prelude.Hashable DeleteReplicaAction

instance Prelude.NFData DeleteReplicaAction

instance Prelude.ToJSON DeleteReplicaAction where
  toJSON DeleteReplicaAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("RegionName" Prelude..= regionName)]
      )
