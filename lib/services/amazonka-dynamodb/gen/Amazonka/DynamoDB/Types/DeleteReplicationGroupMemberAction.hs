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
-- Module      : Amazonka.DynamoDB.Types.DeleteReplicationGroupMemberAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.DeleteReplicationGroupMemberAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a replica to be deleted.
--
-- /See:/ 'newDeleteReplicationGroupMemberAction' smart constructor.
data DeleteReplicationGroupMemberAction = DeleteReplicationGroupMemberAction'
  { -- | The Region where the replica exists.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationGroupMemberAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'deleteReplicationGroupMemberAction_regionName' - The Region where the replica exists.
newDeleteReplicationGroupMemberAction ::
  -- | 'regionName'
  Prelude.Text ->
  DeleteReplicationGroupMemberAction
newDeleteReplicationGroupMemberAction pRegionName_ =
  DeleteReplicationGroupMemberAction'
    { regionName =
        pRegionName_
    }

-- | The Region where the replica exists.
deleteReplicationGroupMemberAction_regionName :: Lens.Lens' DeleteReplicationGroupMemberAction Prelude.Text
deleteReplicationGroupMemberAction_regionName = Lens.lens (\DeleteReplicationGroupMemberAction' {regionName} -> regionName) (\s@DeleteReplicationGroupMemberAction' {} a -> s {regionName = a} :: DeleteReplicationGroupMemberAction)

instance
  Prelude.Hashable
    DeleteReplicationGroupMemberAction
  where
  hashWithSalt
    _salt
    DeleteReplicationGroupMemberAction' {..} =
      _salt `Prelude.hashWithSalt` regionName

instance
  Prelude.NFData
    DeleteReplicationGroupMemberAction
  where
  rnf DeleteReplicationGroupMemberAction' {..} =
    Prelude.rnf regionName

instance
  Data.ToJSON
    DeleteReplicationGroupMemberAction
  where
  toJSON DeleteReplicationGroupMemberAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RegionName" Data..= regionName)]
      )
