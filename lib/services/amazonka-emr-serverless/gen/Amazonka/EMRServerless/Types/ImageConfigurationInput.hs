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
-- Module      : Amazonka.EMRServerless.Types.ImageConfigurationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.ImageConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The image configuration.
--
-- /See:/ 'newImageConfigurationInput' smart constructor.
data ImageConfigurationInput = ImageConfigurationInput'
  { -- | The URI of an image in the Amazon ECR registry. This field is required
    -- when you create a new application. If you leave this field blank in an
    -- update, Amazon EMR will remove the image configuration.
    imageUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageUri', 'imageConfigurationInput_imageUri' - The URI of an image in the Amazon ECR registry. This field is required
-- when you create a new application. If you leave this field blank in an
-- update, Amazon EMR will remove the image configuration.
newImageConfigurationInput ::
  ImageConfigurationInput
newImageConfigurationInput =
  ImageConfigurationInput'
    { imageUri =
        Prelude.Nothing
    }

-- | The URI of an image in the Amazon ECR registry. This field is required
-- when you create a new application. If you leave this field blank in an
-- update, Amazon EMR will remove the image configuration.
imageConfigurationInput_imageUri :: Lens.Lens' ImageConfigurationInput (Prelude.Maybe Prelude.Text)
imageConfigurationInput_imageUri = Lens.lens (\ImageConfigurationInput' {imageUri} -> imageUri) (\s@ImageConfigurationInput' {} a -> s {imageUri = a} :: ImageConfigurationInput)

instance Prelude.Hashable ImageConfigurationInput where
  hashWithSalt _salt ImageConfigurationInput' {..} =
    _salt `Prelude.hashWithSalt` imageUri

instance Prelude.NFData ImageConfigurationInput where
  rnf ImageConfigurationInput' {..} =
    Prelude.rnf imageUri

instance Data.ToJSON ImageConfigurationInput where
  toJSON ImageConfigurationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [("imageUri" Data..=) Prelude.<$> imageUri]
      )
