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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.DeleteBehavior
import Network.AWS.Glue.Types.UpdateBehavior
import qualified Network.AWS.Lens as Lens

-- | A policy that specifies update and deletion behaviors for the crawler.
--
-- /See:/ 'newSchemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Core.Maybe UpdateBehavior,
    -- | The deletion behavior when the crawler finds a deleted object.
    deleteBehavior :: Core.Maybe DeleteBehavior
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { updateBehavior = Core.Nothing,
      deleteBehavior = Core.Nothing
    }

-- | The update behavior when the crawler finds a changed schema.
schemaChangePolicy_updateBehavior :: Lens.Lens' SchemaChangePolicy (Core.Maybe UpdateBehavior)
schemaChangePolicy_updateBehavior = Lens.lens (\SchemaChangePolicy' {updateBehavior} -> updateBehavior) (\s@SchemaChangePolicy' {} a -> s {updateBehavior = a} :: SchemaChangePolicy)

-- | The deletion behavior when the crawler finds a deleted object.
schemaChangePolicy_deleteBehavior :: Lens.Lens' SchemaChangePolicy (Core.Maybe DeleteBehavior)
schemaChangePolicy_deleteBehavior = Lens.lens (\SchemaChangePolicy' {deleteBehavior} -> deleteBehavior) (\s@SchemaChangePolicy' {} a -> s {deleteBehavior = a} :: SchemaChangePolicy)

instance Core.FromJSON SchemaChangePolicy where
  parseJSON =
    Core.withObject
      "SchemaChangePolicy"
      ( \x ->
          SchemaChangePolicy'
            Core.<$> (x Core..:? "UpdateBehavior")
            Core.<*> (x Core..:? "DeleteBehavior")
      )

instance Core.Hashable SchemaChangePolicy

instance Core.NFData SchemaChangePolicy

instance Core.ToJSON SchemaChangePolicy where
  toJSON SchemaChangePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UpdateBehavior" Core..=) Core.<$> updateBehavior,
            ("DeleteBehavior" Core..=) Core.<$> deleteBehavior
          ]
      )
