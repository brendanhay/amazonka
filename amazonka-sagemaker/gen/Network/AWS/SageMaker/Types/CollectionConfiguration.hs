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
-- Module      : Network.AWS.SageMaker.Types.CollectionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CollectionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information for the Debugger output tensor collections.
--
-- /See:/ 'newCollectionConfiguration' smart constructor.
data CollectionConfiguration = CollectionConfiguration'
  { -- | Parameter values for the tensor collection. The allowed parameters are
    -- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
    -- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
    collectionParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the tensor collection. The name must be unique relative to
    -- other rule configuration names.
    collectionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CollectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionParameters', 'collectionConfiguration_collectionParameters' - Parameter values for the tensor collection. The allowed parameters are
-- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
-- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
--
-- 'collectionName', 'collectionConfiguration_collectionName' - The name of the tensor collection. The name must be unique relative to
-- other rule configuration names.
newCollectionConfiguration ::
  CollectionConfiguration
newCollectionConfiguration =
  CollectionConfiguration'
    { collectionParameters =
        Core.Nothing,
      collectionName = Core.Nothing
    }

-- | Parameter values for the tensor collection. The allowed parameters are
-- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
-- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
collectionConfiguration_collectionParameters :: Lens.Lens' CollectionConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
collectionConfiguration_collectionParameters = Lens.lens (\CollectionConfiguration' {collectionParameters} -> collectionParameters) (\s@CollectionConfiguration' {} a -> s {collectionParameters = a} :: CollectionConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The name of the tensor collection. The name must be unique relative to
-- other rule configuration names.
collectionConfiguration_collectionName :: Lens.Lens' CollectionConfiguration (Core.Maybe Core.Text)
collectionConfiguration_collectionName = Lens.lens (\CollectionConfiguration' {collectionName} -> collectionName) (\s@CollectionConfiguration' {} a -> s {collectionName = a} :: CollectionConfiguration)

instance Core.FromJSON CollectionConfiguration where
  parseJSON =
    Core.withObject
      "CollectionConfiguration"
      ( \x ->
          CollectionConfiguration'
            Core.<$> ( x Core..:? "CollectionParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CollectionName")
      )

instance Core.Hashable CollectionConfiguration

instance Core.NFData CollectionConfiguration

instance Core.ToJSON CollectionConfiguration where
  toJSON CollectionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CollectionParameters" Core..=)
              Core.<$> collectionParameters,
            ("CollectionName" Core..=) Core.<$> collectionName
          ]
      )
