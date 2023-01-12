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
-- Module      : Amazonka.LookoutVision.Types.ImageSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ImageSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source for an image.
--
-- /See:/ 'newImageSource' smart constructor.
data ImageSource = ImageSource'
  { -- | The type of the image.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'imageSource_type' - The type of the image.
newImageSource ::
  ImageSource
newImageSource =
  ImageSource' {type' = Prelude.Nothing}

-- | The type of the image.
imageSource_type :: Lens.Lens' ImageSource (Prelude.Maybe Prelude.Text)
imageSource_type = Lens.lens (\ImageSource' {type'} -> type') (\s@ImageSource' {} a -> s {type' = a} :: ImageSource)

instance Data.FromJSON ImageSource where
  parseJSON =
    Data.withObject
      "ImageSource"
      (\x -> ImageSource' Prelude.<$> (x Data..:? "Type"))

instance Prelude.Hashable ImageSource where
  hashWithSalt _salt ImageSource' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData ImageSource where
  rnf ImageSource' {..} = Prelude.rnf type'
