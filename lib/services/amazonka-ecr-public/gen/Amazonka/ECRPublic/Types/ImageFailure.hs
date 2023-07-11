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
-- Module      : Amazonka.ECRPublic.Types.ImageFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.ImageFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types.ImageFailureCode
import Amazonka.ECRPublic.Types.ImageIdentifier
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon ECR image failure.
--
-- /See:/ 'newImageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { -- | The code associated with the failure.
    failureCode :: Prelude.Maybe ImageFailureCode,
    -- | The reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The image ID associated with the failure.
    imageId :: Prelude.Maybe ImageIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'imageFailure_failureCode' - The code associated with the failure.
--
-- 'failureReason', 'imageFailure_failureReason' - The reason for the failure.
--
-- 'imageId', 'imageFailure_imageId' - The image ID associated with the failure.
newImageFailure ::
  ImageFailure
newImageFailure =
  ImageFailure'
    { failureCode = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The code associated with the failure.
imageFailure_failureCode :: Lens.Lens' ImageFailure (Prelude.Maybe ImageFailureCode)
imageFailure_failureCode = Lens.lens (\ImageFailure' {failureCode} -> failureCode) (\s@ImageFailure' {} a -> s {failureCode = a} :: ImageFailure)

-- | The reason for the failure.
imageFailure_failureReason :: Lens.Lens' ImageFailure (Prelude.Maybe Prelude.Text)
imageFailure_failureReason = Lens.lens (\ImageFailure' {failureReason} -> failureReason) (\s@ImageFailure' {} a -> s {failureReason = a} :: ImageFailure)

-- | The image ID associated with the failure.
imageFailure_imageId :: Lens.Lens' ImageFailure (Prelude.Maybe ImageIdentifier)
imageFailure_imageId = Lens.lens (\ImageFailure' {imageId} -> imageId) (\s@ImageFailure' {} a -> s {imageId = a} :: ImageFailure)

instance Data.FromJSON ImageFailure where
  parseJSON =
    Data.withObject
      "ImageFailure"
      ( \x ->
          ImageFailure'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "imageId")
      )

instance Prelude.Hashable ImageFailure where
  hashWithSalt _salt ImageFailure' {..} =
    _salt
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData ImageFailure where
  rnf ImageFailure' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf imageId
