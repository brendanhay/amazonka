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
-- Module      : Amazonka.DynamoDB.Types.GlobalSecondaryIndexUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalSecondaryIndexUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents one of the following:
--
-- -   A new global secondary index to be added to an existing table.
--
-- -   New provisioned throughput parameters for an existing global
--     secondary index.
--
-- -   An existing global secondary index to be removed from an existing
--     table.
--
-- /See:/ 'newGlobalSecondaryIndexUpdate' smart constructor.
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
  { -- | The parameters required for creating a global secondary index on an
    -- existing table:
    --
    -- -   @IndexName @
    --
    -- -   @KeySchema @
    --
    -- -   @AttributeDefinitions @
    --
    -- -   @Projection @
    --
    -- -   @ProvisionedThroughput @
    create :: Prelude.Maybe CreateGlobalSecondaryIndexAction,
    -- | The name of an existing global secondary index to be removed.
    delete' :: Prelude.Maybe DeleteGlobalSecondaryIndexAction,
    -- | The name of an existing global secondary index, along with new
    -- provisioned throughput settings to be applied to that index.
    update :: Prelude.Maybe UpdateGlobalSecondaryIndexAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'create', 'globalSecondaryIndexUpdate_create' - The parameters required for creating a global secondary index on an
-- existing table:
--
-- -   @IndexName @
--
-- -   @KeySchema @
--
-- -   @AttributeDefinitions @
--
-- -   @Projection @
--
-- -   @ProvisionedThroughput @
--
-- 'delete'', 'globalSecondaryIndexUpdate_delete' - The name of an existing global secondary index to be removed.
--
-- 'update', 'globalSecondaryIndexUpdate_update' - The name of an existing global secondary index, along with new
-- provisioned throughput settings to be applied to that index.
newGlobalSecondaryIndexUpdate ::
  GlobalSecondaryIndexUpdate
newGlobalSecondaryIndexUpdate =
  GlobalSecondaryIndexUpdate'
    { create =
        Prelude.Nothing,
      delete' = Prelude.Nothing,
      update = Prelude.Nothing
    }

-- | The parameters required for creating a global secondary index on an
-- existing table:
--
-- -   @IndexName @
--
-- -   @KeySchema @
--
-- -   @AttributeDefinitions @
--
-- -   @Projection @
--
-- -   @ProvisionedThroughput @
globalSecondaryIndexUpdate_create :: Lens.Lens' GlobalSecondaryIndexUpdate (Prelude.Maybe CreateGlobalSecondaryIndexAction)
globalSecondaryIndexUpdate_create = Lens.lens (\GlobalSecondaryIndexUpdate' {create} -> create) (\s@GlobalSecondaryIndexUpdate' {} a -> s {create = a} :: GlobalSecondaryIndexUpdate)

-- | The name of an existing global secondary index to be removed.
globalSecondaryIndexUpdate_delete :: Lens.Lens' GlobalSecondaryIndexUpdate (Prelude.Maybe DeleteGlobalSecondaryIndexAction)
globalSecondaryIndexUpdate_delete = Lens.lens (\GlobalSecondaryIndexUpdate' {delete'} -> delete') (\s@GlobalSecondaryIndexUpdate' {} a -> s {delete' = a} :: GlobalSecondaryIndexUpdate)

-- | The name of an existing global secondary index, along with new
-- provisioned throughput settings to be applied to that index.
globalSecondaryIndexUpdate_update :: Lens.Lens' GlobalSecondaryIndexUpdate (Prelude.Maybe UpdateGlobalSecondaryIndexAction)
globalSecondaryIndexUpdate_update = Lens.lens (\GlobalSecondaryIndexUpdate' {update} -> update) (\s@GlobalSecondaryIndexUpdate' {} a -> s {update = a} :: GlobalSecondaryIndexUpdate)

instance Prelude.Hashable GlobalSecondaryIndexUpdate where
  hashWithSalt _salt GlobalSecondaryIndexUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` create
      `Prelude.hashWithSalt` delete'
      `Prelude.hashWithSalt` update

instance Prelude.NFData GlobalSecondaryIndexUpdate where
  rnf GlobalSecondaryIndexUpdate' {..} =
    Prelude.rnf create
      `Prelude.seq` Prelude.rnf delete'
      `Prelude.seq` Prelude.rnf update

instance Data.ToJSON GlobalSecondaryIndexUpdate where
  toJSON GlobalSecondaryIndexUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Create" Data..=) Prelude.<$> create,
            ("Delete" Data..=) Prelude.<$> delete',
            ("Update" Data..=) Prelude.<$> update
          ]
      )
