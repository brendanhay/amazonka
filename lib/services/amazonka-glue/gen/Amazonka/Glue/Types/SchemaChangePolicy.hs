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
-- Module      : Amazonka.Glue.Types.SchemaChangePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaChangePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DeleteBehavior
import Amazonka.Glue.Types.UpdateBehavior
import qualified Amazonka.Prelude as Prelude

-- | A policy that specifies update and deletion behaviors for the crawler.
--
-- /See:/ 'newSchemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { -- | The deletion behavior when the crawler finds a deleted object.
    deleteBehavior :: Prelude.Maybe DeleteBehavior,
    -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Prelude.Maybe UpdateBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaChangePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteBehavior', 'schemaChangePolicy_deleteBehavior' - The deletion behavior when the crawler finds a deleted object.
--
-- 'updateBehavior', 'schemaChangePolicy_updateBehavior' - The update behavior when the crawler finds a changed schema.
newSchemaChangePolicy ::
  SchemaChangePolicy
newSchemaChangePolicy =
  SchemaChangePolicy'
    { deleteBehavior =
        Prelude.Nothing,
      updateBehavior = Prelude.Nothing
    }

-- | The deletion behavior when the crawler finds a deleted object.
schemaChangePolicy_deleteBehavior :: Lens.Lens' SchemaChangePolicy (Prelude.Maybe DeleteBehavior)
schemaChangePolicy_deleteBehavior = Lens.lens (\SchemaChangePolicy' {deleteBehavior} -> deleteBehavior) (\s@SchemaChangePolicy' {} a -> s {deleteBehavior = a} :: SchemaChangePolicy)

-- | The update behavior when the crawler finds a changed schema.
schemaChangePolicy_updateBehavior :: Lens.Lens' SchemaChangePolicy (Prelude.Maybe UpdateBehavior)
schemaChangePolicy_updateBehavior = Lens.lens (\SchemaChangePolicy' {updateBehavior} -> updateBehavior) (\s@SchemaChangePolicy' {} a -> s {updateBehavior = a} :: SchemaChangePolicy)

instance Data.FromJSON SchemaChangePolicy where
  parseJSON =
    Data.withObject
      "SchemaChangePolicy"
      ( \x ->
          SchemaChangePolicy'
            Prelude.<$> (x Data..:? "DeleteBehavior")
            Prelude.<*> (x Data..:? "UpdateBehavior")
      )

instance Prelude.Hashable SchemaChangePolicy where
  hashWithSalt _salt SchemaChangePolicy' {..} =
    _salt `Prelude.hashWithSalt` deleteBehavior
      `Prelude.hashWithSalt` updateBehavior

instance Prelude.NFData SchemaChangePolicy where
  rnf SchemaChangePolicy' {..} =
    Prelude.rnf deleteBehavior
      `Prelude.seq` Prelude.rnf updateBehavior

instance Data.ToJSON SchemaChangePolicy where
  toJSON SchemaChangePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteBehavior" Data..=)
              Prelude.<$> deleteBehavior,
            ("UpdateBehavior" Data..=)
              Prelude.<$> updateBehavior
          ]
      )
