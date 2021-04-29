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
-- Module      : Network.AWS.Glue.Types.SchemaChangePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaChangePolicy where

import Network.AWS.Glue.Types.DeleteBehavior
import Network.AWS.Glue.Types.UpdateBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A policy that specifies update and deletion behaviors for the crawler.
--
-- /See:/ 'newSchemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Prelude.Maybe UpdateBehavior,
    -- | The deletion behavior when the crawler finds a deleted object.
    deleteBehavior :: Prelude.Maybe DeleteBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SchemaChangePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateBehavior', 'schemaChangePolicy_updateBehavior' - The update behavior when the crawler finds a changed schema.
--
-- 'deleteBehavior', 'schemaChangePolicy_deleteBehavior' - The deletion behavior when the crawler finds a deleted object.
newSchemaChangePolicy ::
  SchemaChangePolicy
newSchemaChangePolicy =
  SchemaChangePolicy'
    { updateBehavior =
        Prelude.Nothing,
      deleteBehavior = Prelude.Nothing
    }

-- | The update behavior when the crawler finds a changed schema.
schemaChangePolicy_updateBehavior :: Lens.Lens' SchemaChangePolicy (Prelude.Maybe UpdateBehavior)
schemaChangePolicy_updateBehavior = Lens.lens (\SchemaChangePolicy' {updateBehavior} -> updateBehavior) (\s@SchemaChangePolicy' {} a -> s {updateBehavior = a} :: SchemaChangePolicy)

-- | The deletion behavior when the crawler finds a deleted object.
schemaChangePolicy_deleteBehavior :: Lens.Lens' SchemaChangePolicy (Prelude.Maybe DeleteBehavior)
schemaChangePolicy_deleteBehavior = Lens.lens (\SchemaChangePolicy' {deleteBehavior} -> deleteBehavior) (\s@SchemaChangePolicy' {} a -> s {deleteBehavior = a} :: SchemaChangePolicy)

instance Prelude.FromJSON SchemaChangePolicy where
  parseJSON =
    Prelude.withObject
      "SchemaChangePolicy"
      ( \x ->
          SchemaChangePolicy'
            Prelude.<$> (x Prelude..:? "UpdateBehavior")
            Prelude.<*> (x Prelude..:? "DeleteBehavior")
      )

instance Prelude.Hashable SchemaChangePolicy

instance Prelude.NFData SchemaChangePolicy

instance Prelude.ToJSON SchemaChangePolicy where
  toJSON SchemaChangePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UpdateBehavior" Prelude..=)
              Prelude.<$> updateBehavior,
            ("DeleteBehavior" Prelude..=)
              Prelude.<$> deleteBehavior
          ]
      )
