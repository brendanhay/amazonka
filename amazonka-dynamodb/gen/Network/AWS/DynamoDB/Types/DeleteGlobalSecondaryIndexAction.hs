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
-- Module      : Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a global secondary index to be deleted from an existing
-- table.
--
-- /See:/ 'newDeleteGlobalSecondaryIndexAction' smart constructor.
data DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { -- | The name of the global secondary index to be deleted.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.NFData
    DeleteGlobalSecondaryIndexAction

instance
  Prelude.ToJSON
    DeleteGlobalSecondaryIndexAction
  where
  toJSON DeleteGlobalSecondaryIndexAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("IndexName" Prelude..= indexName)]
      )
