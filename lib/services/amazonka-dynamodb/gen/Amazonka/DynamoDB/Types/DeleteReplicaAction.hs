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
-- Module      : Amazonka.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.DeleteReplicaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a replica to be removed.
--
-- /See:/ 'newDeleteReplicaAction' smart constructor.
data DeleteReplicaAction = DeleteReplicaAction'
  { -- | The Region of the replica to be removed.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable DeleteReplicaAction where
  hashWithSalt _salt DeleteReplicaAction' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData DeleteReplicaAction where
  rnf DeleteReplicaAction' {..} = Prelude.rnf regionName

instance Data.ToJSON DeleteReplicaAction where
  toJSON DeleteReplicaAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RegionName" Data..= regionName)]
      )
