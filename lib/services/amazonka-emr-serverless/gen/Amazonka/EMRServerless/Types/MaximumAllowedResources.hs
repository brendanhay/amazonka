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
-- Module      : Amazonka.EMRServerless.Types.MaximumAllowedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.MaximumAllowedResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The maximum allowed cumulative resources for an application. No new
-- resources will be created once the limit is hit.
--
-- /See:/ 'newMaximumAllowedResources' smart constructor.
data MaximumAllowedResources = MaximumAllowedResources'
  { -- | The maximum allowed disk for an application.
    disk :: Prelude.Maybe Prelude.Text,
    -- | The maximum allowed CPU for an application.
    cpu :: Prelude.Text,
    -- | The maximum allowed resources for an application.
    memory :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaximumAllowedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disk', 'maximumAllowedResources_disk' - The maximum allowed disk for an application.
--
-- 'cpu', 'maximumAllowedResources_cpu' - The maximum allowed CPU for an application.
--
-- 'memory', 'maximumAllowedResources_memory' - The maximum allowed resources for an application.
newMaximumAllowedResources ::
  -- | 'cpu'
  Prelude.Text ->
  -- | 'memory'
  Prelude.Text ->
  MaximumAllowedResources
newMaximumAllowedResources pCpu_ pMemory_ =
  MaximumAllowedResources'
    { disk = Prelude.Nothing,
      cpu = pCpu_,
      memory = pMemory_
    }

-- | The maximum allowed disk for an application.
maximumAllowedResources_disk :: Lens.Lens' MaximumAllowedResources (Prelude.Maybe Prelude.Text)
maximumAllowedResources_disk = Lens.lens (\MaximumAllowedResources' {disk} -> disk) (\s@MaximumAllowedResources' {} a -> s {disk = a} :: MaximumAllowedResources)

-- | The maximum allowed CPU for an application.
maximumAllowedResources_cpu :: Lens.Lens' MaximumAllowedResources Prelude.Text
maximumAllowedResources_cpu = Lens.lens (\MaximumAllowedResources' {cpu} -> cpu) (\s@MaximumAllowedResources' {} a -> s {cpu = a} :: MaximumAllowedResources)

-- | The maximum allowed resources for an application.
maximumAllowedResources_memory :: Lens.Lens' MaximumAllowedResources Prelude.Text
maximumAllowedResources_memory = Lens.lens (\MaximumAllowedResources' {memory} -> memory) (\s@MaximumAllowedResources' {} a -> s {memory = a} :: MaximumAllowedResources)

instance Data.FromJSON MaximumAllowedResources where
  parseJSON =
    Data.withObject
      "MaximumAllowedResources"
      ( \x ->
          MaximumAllowedResources'
            Prelude.<$> (x Data..:? "disk")
            Prelude.<*> (x Data..: "cpu")
            Prelude.<*> (x Data..: "memory")
      )

instance Prelude.Hashable MaximumAllowedResources where
  hashWithSalt _salt MaximumAllowedResources' {..} =
    _salt
      `Prelude.hashWithSalt` disk
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memory

instance Prelude.NFData MaximumAllowedResources where
  rnf MaximumAllowedResources' {..} =
    Prelude.rnf disk
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memory

instance Data.ToJSON MaximumAllowedResources where
  toJSON MaximumAllowedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("disk" Data..=) Prelude.<$> disk,
            Prelude.Just ("cpu" Data..= cpu),
            Prelude.Just ("memory" Data..= memory)
          ]
      )
