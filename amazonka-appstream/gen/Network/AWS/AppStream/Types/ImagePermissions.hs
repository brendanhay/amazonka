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
-- Module      : Network.AWS.AppStream.Types.ImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImagePermissions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the permissions for an image.
--
-- /See:/ 'newImagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { -- | Indicates whether the image can be used for an image builder.
    allowImageBuilder :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the image can be used for a fleet.
    allowFleet :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowImageBuilder', 'imagePermissions_allowImageBuilder' - Indicates whether the image can be used for an image builder.
--
-- 'allowFleet', 'imagePermissions_allowFleet' - Indicates whether the image can be used for a fleet.
newImagePermissions ::
  ImagePermissions
newImagePermissions =
  ImagePermissions'
    { allowImageBuilder =
        Prelude.Nothing,
      allowFleet = Prelude.Nothing
    }

-- | Indicates whether the image can be used for an image builder.
imagePermissions_allowImageBuilder :: Lens.Lens' ImagePermissions (Prelude.Maybe Prelude.Bool)
imagePermissions_allowImageBuilder = Lens.lens (\ImagePermissions' {allowImageBuilder} -> allowImageBuilder) (\s@ImagePermissions' {} a -> s {allowImageBuilder = a} :: ImagePermissions)

-- | Indicates whether the image can be used for a fleet.
imagePermissions_allowFleet :: Lens.Lens' ImagePermissions (Prelude.Maybe Prelude.Bool)
imagePermissions_allowFleet = Lens.lens (\ImagePermissions' {allowFleet} -> allowFleet) (\s@ImagePermissions' {} a -> s {allowFleet = a} :: ImagePermissions)

instance Prelude.FromJSON ImagePermissions where
  parseJSON =
    Prelude.withObject
      "ImagePermissions"
      ( \x ->
          ImagePermissions'
            Prelude.<$> (x Prelude..:? "allowImageBuilder")
            Prelude.<*> (x Prelude..:? "allowFleet")
      )

instance Prelude.Hashable ImagePermissions

instance Prelude.NFData ImagePermissions

instance Prelude.ToJSON ImagePermissions where
  toJSON ImagePermissions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("allowImageBuilder" Prelude..=)
              Prelude.<$> allowImageBuilder,
            ("allowFleet" Prelude..=) Prelude.<$> allowFleet
          ]
      )
