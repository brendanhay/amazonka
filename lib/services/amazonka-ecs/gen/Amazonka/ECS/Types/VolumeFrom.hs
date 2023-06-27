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
-- Module      : Amazonka.ECS.Types.VolumeFrom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.VolumeFrom where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on a data volume from another container in the same task
-- definition.
--
-- /See:/ 'newVolumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
  { -- | If this value is @true@, the container has read-only access to the
    -- volume. If this value is @false@, then the container can write to the
    -- volume. The default value is @false@.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of another container within the same task definition to mount
    -- volumes from.
    sourceContainer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeFrom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOnly', 'volumeFrom_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
--
-- 'sourceContainer', 'volumeFrom_sourceContainer' - The name of another container within the same task definition to mount
-- volumes from.
newVolumeFrom ::
  VolumeFrom
newVolumeFrom =
  VolumeFrom'
    { readOnly = Prelude.Nothing,
      sourceContainer = Prelude.Nothing
    }

-- | If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
volumeFrom_readOnly :: Lens.Lens' VolumeFrom (Prelude.Maybe Prelude.Bool)
volumeFrom_readOnly = Lens.lens (\VolumeFrom' {readOnly} -> readOnly) (\s@VolumeFrom' {} a -> s {readOnly = a} :: VolumeFrom)

-- | The name of another container within the same task definition to mount
-- volumes from.
volumeFrom_sourceContainer :: Lens.Lens' VolumeFrom (Prelude.Maybe Prelude.Text)
volumeFrom_sourceContainer = Lens.lens (\VolumeFrom' {sourceContainer} -> sourceContainer) (\s@VolumeFrom' {} a -> s {sourceContainer = a} :: VolumeFrom)

instance Data.FromJSON VolumeFrom where
  parseJSON =
    Data.withObject
      "VolumeFrom"
      ( \x ->
          VolumeFrom'
            Prelude.<$> (x Data..:? "readOnly")
            Prelude.<*> (x Data..:? "sourceContainer")
      )

instance Prelude.Hashable VolumeFrom where
  hashWithSalt _salt VolumeFrom' {..} =
    _salt
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` sourceContainer

instance Prelude.NFData VolumeFrom where
  rnf VolumeFrom' {..} =
    Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf sourceContainer

instance Data.ToJSON VolumeFrom where
  toJSON VolumeFrom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("readOnly" Data..=) Prelude.<$> readOnly,
            ("sourceContainer" Data..=)
              Prelude.<$> sourceContainer
          ]
      )
