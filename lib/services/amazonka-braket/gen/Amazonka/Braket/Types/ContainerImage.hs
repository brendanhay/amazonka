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
-- Module      : Amazonka.Braket.Types.ContainerImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.ContainerImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The container image used to create an Amazon Braket job.
--
-- /See:/ 'newContainerImage' smart constructor.
data ContainerImage = ContainerImage'
  { -- | The URI locating the container image.
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'containerImage_uri' - The URI locating the container image.
newContainerImage ::
  -- | 'uri'
  Prelude.Text ->
  ContainerImage
newContainerImage pUri_ =
  ContainerImage' {uri = pUri_}

-- | The URI locating the container image.
containerImage_uri :: Lens.Lens' ContainerImage Prelude.Text
containerImage_uri = Lens.lens (\ContainerImage' {uri} -> uri) (\s@ContainerImage' {} a -> s {uri = a} :: ContainerImage)

instance Core.FromJSON ContainerImage where
  parseJSON =
    Core.withObject
      "ContainerImage"
      ( \x ->
          ContainerImage' Prelude.<$> (x Core..: "uri")
      )

instance Prelude.Hashable ContainerImage where
  hashWithSalt _salt ContainerImage' {..} =
    _salt `Prelude.hashWithSalt` uri

instance Prelude.NFData ContainerImage where
  rnf ContainerImage' {..} = Prelude.rnf uri

instance Core.ToJSON ContainerImage where
  toJSON ContainerImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("uri" Core..= uri)]
      )
