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
-- Module      : Amazonka.AppStream.Types.ImagePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImagePermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the permissions for an image.
--
-- /See:/ 'newImagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { -- | Indicates whether the image can be used for a fleet.
    allowFleet :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the image can be used for an image builder.
    allowImageBuilder :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowFleet', 'imagePermissions_allowFleet' - Indicates whether the image can be used for a fleet.
--
-- 'allowImageBuilder', 'imagePermissions_allowImageBuilder' - Indicates whether the image can be used for an image builder.
newImagePermissions ::
  ImagePermissions
newImagePermissions =
  ImagePermissions'
    { allowFleet = Prelude.Nothing,
      allowImageBuilder = Prelude.Nothing
    }

-- | Indicates whether the image can be used for a fleet.
imagePermissions_allowFleet :: Lens.Lens' ImagePermissions (Prelude.Maybe Prelude.Bool)
imagePermissions_allowFleet = Lens.lens (\ImagePermissions' {allowFleet} -> allowFleet) (\s@ImagePermissions' {} a -> s {allowFleet = a} :: ImagePermissions)

-- | Indicates whether the image can be used for an image builder.
imagePermissions_allowImageBuilder :: Lens.Lens' ImagePermissions (Prelude.Maybe Prelude.Bool)
imagePermissions_allowImageBuilder = Lens.lens (\ImagePermissions' {allowImageBuilder} -> allowImageBuilder) (\s@ImagePermissions' {} a -> s {allowImageBuilder = a} :: ImagePermissions)

instance Data.FromJSON ImagePermissions where
  parseJSON =
    Data.withObject
      "ImagePermissions"
      ( \x ->
          ImagePermissions'
            Prelude.<$> (x Data..:? "allowFleet")
            Prelude.<*> (x Data..:? "allowImageBuilder")
      )

instance Prelude.Hashable ImagePermissions where
  hashWithSalt _salt ImagePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` allowFleet
      `Prelude.hashWithSalt` allowImageBuilder

instance Prelude.NFData ImagePermissions where
  rnf ImagePermissions' {..} =
    Prelude.rnf allowFleet `Prelude.seq`
      Prelude.rnf allowImageBuilder

instance Data.ToJSON ImagePermissions where
  toJSON ImagePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowFleet" Data..=) Prelude.<$> allowFleet,
            ("allowImageBuilder" Data..=)
              Prelude.<$> allowImageBuilder
          ]
      )
