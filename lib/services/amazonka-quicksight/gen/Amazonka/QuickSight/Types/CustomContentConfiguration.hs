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
-- Module      : Amazonka.QuickSight.Types.CustomContentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomContentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomContentImageScalingConfiguration
import Amazonka.QuickSight.Types.CustomContentType

-- | The configuration of a @CustomContentVisual@.
--
-- /See:/ 'newCustomContentConfiguration' smart constructor.
data CustomContentConfiguration = CustomContentConfiguration'
  { -- | The content type of the custom content visual. You can use this to have
    -- the visual render as an image.
    contentType :: Prelude.Maybe CustomContentType,
    -- | The input URL that links to the custom content that you want in the
    -- custom visual.
    contentUrl :: Prelude.Maybe Prelude.Text,
    -- | The sizing options for the size of the custom content visual. This
    -- structure is required when the @ContentType@ of the visual is
    -- @\'IMAGE\'@.
    imageScaling :: Prelude.Maybe CustomContentImageScalingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomContentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'customContentConfiguration_contentType' - The content type of the custom content visual. You can use this to have
-- the visual render as an image.
--
-- 'contentUrl', 'customContentConfiguration_contentUrl' - The input URL that links to the custom content that you want in the
-- custom visual.
--
-- 'imageScaling', 'customContentConfiguration_imageScaling' - The sizing options for the size of the custom content visual. This
-- structure is required when the @ContentType@ of the visual is
-- @\'IMAGE\'@.
newCustomContentConfiguration ::
  CustomContentConfiguration
newCustomContentConfiguration =
  CustomContentConfiguration'
    { contentType =
        Prelude.Nothing,
      contentUrl = Prelude.Nothing,
      imageScaling = Prelude.Nothing
    }

-- | The content type of the custom content visual. You can use this to have
-- the visual render as an image.
customContentConfiguration_contentType :: Lens.Lens' CustomContentConfiguration (Prelude.Maybe CustomContentType)
customContentConfiguration_contentType = Lens.lens (\CustomContentConfiguration' {contentType} -> contentType) (\s@CustomContentConfiguration' {} a -> s {contentType = a} :: CustomContentConfiguration)

-- | The input URL that links to the custom content that you want in the
-- custom visual.
customContentConfiguration_contentUrl :: Lens.Lens' CustomContentConfiguration (Prelude.Maybe Prelude.Text)
customContentConfiguration_contentUrl = Lens.lens (\CustomContentConfiguration' {contentUrl} -> contentUrl) (\s@CustomContentConfiguration' {} a -> s {contentUrl = a} :: CustomContentConfiguration)

-- | The sizing options for the size of the custom content visual. This
-- structure is required when the @ContentType@ of the visual is
-- @\'IMAGE\'@.
customContentConfiguration_imageScaling :: Lens.Lens' CustomContentConfiguration (Prelude.Maybe CustomContentImageScalingConfiguration)
customContentConfiguration_imageScaling = Lens.lens (\CustomContentConfiguration' {imageScaling} -> imageScaling) (\s@CustomContentConfiguration' {} a -> s {imageScaling = a} :: CustomContentConfiguration)

instance Data.FromJSON CustomContentConfiguration where
  parseJSON =
    Data.withObject
      "CustomContentConfiguration"
      ( \x ->
          CustomContentConfiguration'
            Prelude.<$> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "ContentUrl")
            Prelude.<*> (x Data..:? "ImageScaling")
      )

instance Prelude.Hashable CustomContentConfiguration where
  hashWithSalt _salt CustomContentConfiguration' {..} =
    _salt `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` contentUrl
      `Prelude.hashWithSalt` imageScaling

instance Prelude.NFData CustomContentConfiguration where
  rnf CustomContentConfiguration' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf contentUrl
      `Prelude.seq` Prelude.rnf imageScaling

instance Data.ToJSON CustomContentConfiguration where
  toJSON CustomContentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentType" Data..=) Prelude.<$> contentType,
            ("ContentUrl" Data..=) Prelude.<$> contentUrl,
            ("ImageScaling" Data..=) Prelude.<$> imageScaling
          ]
      )
