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
-- Module      : Amazonka.MediaConvert.Types.AllowedRenditionSize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AllowedRenditionSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.RequiredFlag
import qualified Amazonka.Prelude as Prelude

-- | Use Allowed renditions to specify a list of possible resolutions in your
-- ABR stack. * MediaConvert will create an ABR stack exclusively from the
-- list of resolutions that you specify. * Some resolutions in the Allowed
-- renditions list may not be included, however you can force a resolution
-- to be included by setting Required to ENABLED. * You must specify at
-- least one resolution that is greater than or equal to any resolutions
-- that you specify in Min top rendition size or Min bottom rendition size.
-- * If you specify Allowed renditions, you must not specify a separate
-- rule for Force include renditions.
--
-- /See:/ 'newAllowedRenditionSize' smart constructor.
data AllowedRenditionSize = AllowedRenditionSize'
  { -- | Use Height to define the video resolution height, in pixels, for this
    -- rule.
    height :: Prelude.Maybe Prelude.Natural,
    -- | Set to ENABLED to force a rendition to be included.
    required :: Prelude.Maybe RequiredFlag,
    -- | Use Width to define the video resolution width, in pixels, for this
    -- rule.
    width :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowedRenditionSize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'allowedRenditionSize_height' - Use Height to define the video resolution height, in pixels, for this
-- rule.
--
-- 'required', 'allowedRenditionSize_required' - Set to ENABLED to force a rendition to be included.
--
-- 'width', 'allowedRenditionSize_width' - Use Width to define the video resolution width, in pixels, for this
-- rule.
newAllowedRenditionSize ::
  AllowedRenditionSize
newAllowedRenditionSize =
  AllowedRenditionSize'
    { height = Prelude.Nothing,
      required = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | Use Height to define the video resolution height, in pixels, for this
-- rule.
allowedRenditionSize_height :: Lens.Lens' AllowedRenditionSize (Prelude.Maybe Prelude.Natural)
allowedRenditionSize_height = Lens.lens (\AllowedRenditionSize' {height} -> height) (\s@AllowedRenditionSize' {} a -> s {height = a} :: AllowedRenditionSize)

-- | Set to ENABLED to force a rendition to be included.
allowedRenditionSize_required :: Lens.Lens' AllowedRenditionSize (Prelude.Maybe RequiredFlag)
allowedRenditionSize_required = Lens.lens (\AllowedRenditionSize' {required} -> required) (\s@AllowedRenditionSize' {} a -> s {required = a} :: AllowedRenditionSize)

-- | Use Width to define the video resolution width, in pixels, for this
-- rule.
allowedRenditionSize_width :: Lens.Lens' AllowedRenditionSize (Prelude.Maybe Prelude.Natural)
allowedRenditionSize_width = Lens.lens (\AllowedRenditionSize' {width} -> width) (\s@AllowedRenditionSize' {} a -> s {width = a} :: AllowedRenditionSize)

instance Data.FromJSON AllowedRenditionSize where
  parseJSON =
    Data.withObject
      "AllowedRenditionSize"
      ( \x ->
          AllowedRenditionSize'
            Prelude.<$> (x Data..:? "height")
            Prelude.<*> (x Data..:? "required")
            Prelude.<*> (x Data..:? "width")
      )

instance Prelude.Hashable AllowedRenditionSize where
  hashWithSalt _salt AllowedRenditionSize' {..} =
    _salt
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` width

instance Prelude.NFData AllowedRenditionSize where
  rnf AllowedRenditionSize' {..} =
    Prelude.rnf height `Prelude.seq`
      Prelude.rnf required `Prelude.seq`
        Prelude.rnf width

instance Data.ToJSON AllowedRenditionSize where
  toJSON AllowedRenditionSize' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("height" Data..=) Prelude.<$> height,
            ("required" Data..=) Prelude.<$> required,
            ("width" Data..=) Prelude.<$> width
          ]
      )
