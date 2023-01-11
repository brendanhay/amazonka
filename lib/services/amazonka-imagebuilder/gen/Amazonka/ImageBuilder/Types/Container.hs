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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container encapsulates the runtime environment for an application.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | A list of URIs for containers created in the context Region.
    imageUris :: Prelude.Maybe [Prelude.Text],
    -- | Containers and container images are Region-specific. This is the Region
    -- context for the container.
    region :: Prelude.Maybe Prelude.Text
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
-- 'imageUris', 'container_imageUris' - A list of URIs for containers created in the context Region.
--
-- 'region', 'container_region' - Containers and container images are Region-specific. This is the Region
-- context for the container.
newContainer ::
  Container
newContainer =
  Container'
    { imageUris = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | A list of URIs for containers created in the context Region.
container_imageUris :: Lens.Lens' Container (Prelude.Maybe [Prelude.Text])
container_imageUris = Lens.lens (\Container' {imageUris} -> imageUris) (\s@Container' {} a -> s {imageUris = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | Containers and container images are Region-specific. This is the Region
-- context for the container.
container_region :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_region = Lens.lens (\Container' {region} -> region) (\s@Container' {} a -> s {region = a} :: Container)

instance Data.FromJSON Container where
  parseJSON =
    Data.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Data..:? "imageUris" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "region")
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt `Prelude.hashWithSalt` imageUris
      `Prelude.hashWithSalt` region

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf imageUris
      `Prelude.seq` Prelude.rnf region
