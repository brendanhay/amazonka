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
-- Module      : Amazonka.ImageBuilder.Types.Container
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A container encapsulates the runtime environment for an application.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | Containers and container images are Region-specific. This is the Region
    -- context for the container.
    region :: Prelude.Maybe Prelude.Text,
    -- | A list of URIs for containers created in the context Region.
    imageUris :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'container_region' - Containers and container images are Region-specific. This is the Region
-- context for the container.
--
-- 'imageUris', 'container_imageUris' - A list of URIs for containers created in the context Region.
newContainer ::
  Container
newContainer =
  Container'
    { region = Prelude.Nothing,
      imageUris = Prelude.Nothing
    }

-- | Containers and container images are Region-specific. This is the Region
-- context for the container.
container_region :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_region = Lens.lens (\Container' {region} -> region) (\s@Container' {} a -> s {region = a} :: Container)

-- | A list of URIs for containers created in the context Region.
container_imageUris :: Lens.Lens' Container (Prelude.Maybe [Prelude.Text])
container_imageUris = Lens.lens (\Container' {imageUris} -> imageUris) (\s@Container' {} a -> s {imageUris = a} :: Container) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Core..:? "region")
            Prelude.<*> (x Core..:? "imageUris" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` imageUris

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf imageUris
