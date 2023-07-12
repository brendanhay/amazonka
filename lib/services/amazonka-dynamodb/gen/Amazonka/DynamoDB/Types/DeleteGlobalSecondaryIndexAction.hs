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
-- Module      : Amazonka.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.DeleteGlobalSecondaryIndexAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a global secondary index to be deleted from an existing
-- table.
--
-- /See:/ 'newDeleteGlobalSecondaryIndexAction' smart constructor.
data DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { -- | The name of the global secondary index to be deleted.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalSecondaryIndexAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'deleteGlobalSecondaryIndexAction_indexName' - The name of the global secondary index to be deleted.
newDeleteGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Prelude.Text ->
  DeleteGlobalSecondaryIndexAction
newDeleteGlobalSecondaryIndexAction pIndexName_ =
  DeleteGlobalSecondaryIndexAction'
    { indexName =
        pIndexName_
    }

-- | The name of the global secondary index to be deleted.
deleteGlobalSecondaryIndexAction_indexName :: Lens.Lens' DeleteGlobalSecondaryIndexAction Prelude.Text
deleteGlobalSecondaryIndexAction_indexName = Lens.lens (\DeleteGlobalSecondaryIndexAction' {indexName} -> indexName) (\s@DeleteGlobalSecondaryIndexAction' {} a -> s {indexName = a} :: DeleteGlobalSecondaryIndexAction)

instance
  Prelude.Hashable
    DeleteGlobalSecondaryIndexAction
  where
  hashWithSalt
    _salt
    DeleteGlobalSecondaryIndexAction' {..} =
      _salt `Prelude.hashWithSalt` indexName

instance
  Prelude.NFData
    DeleteGlobalSecondaryIndexAction
  where
  rnf DeleteGlobalSecondaryIndexAction' {..} =
    Prelude.rnf indexName

instance Data.ToJSON DeleteGlobalSecondaryIndexAction where
  toJSON DeleteGlobalSecondaryIndexAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("IndexName" Data..= indexName)]
      )
