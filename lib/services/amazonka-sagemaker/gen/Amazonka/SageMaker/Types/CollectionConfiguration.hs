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
-- Module      : Amazonka.SageMaker.Types.CollectionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CollectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for the Amazon SageMaker Debugger output
-- tensor collections.
--
-- /See:/ 'newCollectionConfiguration' smart constructor.
data CollectionConfiguration = CollectionConfiguration'
  { -- | The name of the tensor collection. The name must be unique relative to
    -- other rule configuration names.
    collectionName :: Prelude.Maybe Prelude.Text,
    -- | Parameter values for the tensor collection. The allowed parameters are
    -- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
    -- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
    collectionParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'collectionConfiguration_collectionName' - The name of the tensor collection. The name must be unique relative to
-- other rule configuration names.
--
-- 'collectionParameters', 'collectionConfiguration_collectionParameters' - Parameter values for the tensor collection. The allowed parameters are
-- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
-- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
newCollectionConfiguration ::
  CollectionConfiguration
newCollectionConfiguration =
  CollectionConfiguration'
    { collectionName =
        Prelude.Nothing,
      collectionParameters = Prelude.Nothing
    }

-- | The name of the tensor collection. The name must be unique relative to
-- other rule configuration names.
collectionConfiguration_collectionName :: Lens.Lens' CollectionConfiguration (Prelude.Maybe Prelude.Text)
collectionConfiguration_collectionName = Lens.lens (\CollectionConfiguration' {collectionName} -> collectionName) (\s@CollectionConfiguration' {} a -> s {collectionName = a} :: CollectionConfiguration)

-- | Parameter values for the tensor collection. The allowed parameters are
-- @\"name\"@, @\"include_regex\"@, @\"reduction_config\"@,
-- @\"save_config\"@, @\"tensor_names\"@, and @\"save_histogram\"@.
collectionConfiguration_collectionParameters :: Lens.Lens' CollectionConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
collectionConfiguration_collectionParameters = Lens.lens (\CollectionConfiguration' {collectionParameters} -> collectionParameters) (\s@CollectionConfiguration' {} a -> s {collectionParameters = a} :: CollectionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CollectionConfiguration where
  parseJSON =
    Data.withObject
      "CollectionConfiguration"
      ( \x ->
          CollectionConfiguration'
            Prelude.<$> (x Data..:? "CollectionName")
            Prelude.<*> ( x Data..:? "CollectionParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CollectionConfiguration where
  hashWithSalt _salt CollectionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` collectionParameters

instance Prelude.NFData CollectionConfiguration where
  rnf CollectionConfiguration' {..} =
    Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf collectionParameters

instance Data.ToJSON CollectionConfiguration where
  toJSON CollectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CollectionName" Data..=)
              Prelude.<$> collectionName,
            ("CollectionParameters" Data..=)
              Prelude.<$> collectionParameters
          ]
      )
