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
-- Module      : Network.AWS.ECR.Types.ImageFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailure where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.ImageFailureCode
import Network.AWS.ECR.Types.ImageIdentifier
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon ECR image failure.
--
-- /See:/ 'newImageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { -- | The code associated with the failure.
    failureCode :: Core.Maybe ImageFailureCode,
    -- | The image ID associated with the failure.
    imageId :: Core.Maybe ImageIdentifier,
    -- | The reason for the failure.
    failureReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'imageId', 'imageFailure_imageId' - The image ID associated with the failure.
--
-- 'failureReason', 'imageFailure_failureReason' - The reason for the failure.
newImageFailure ::
  ImageFailure
newImageFailure =
  ImageFailure'
    { failureCode = Core.Nothing,
      imageId = Core.Nothing,
      failureReason = Core.Nothing
    }

-- | The code associated with the failure.
imageFailure_failureCode :: Lens.Lens' ImageFailure (Core.Maybe ImageFailureCode)
imageFailure_failureCode = Lens.lens (\ImageFailure' {failureCode} -> failureCode) (\s@ImageFailure' {} a -> s {failureCode = a} :: ImageFailure)

-- | The image ID associated with the failure.
imageFailure_imageId :: Lens.Lens' ImageFailure (Core.Maybe ImageIdentifier)
imageFailure_imageId = Lens.lens (\ImageFailure' {imageId} -> imageId) (\s@ImageFailure' {} a -> s {imageId = a} :: ImageFailure)

-- | The reason for the failure.
imageFailure_failureReason :: Lens.Lens' ImageFailure (Core.Maybe Core.Text)
imageFailure_failureReason = Lens.lens (\ImageFailure' {failureReason} -> failureReason) (\s@ImageFailure' {} a -> s {failureReason = a} :: ImageFailure)

instance Core.FromJSON ImageFailure where
  parseJSON =
    Core.withObject
      "ImageFailure"
      ( \x ->
          ImageFailure'
            Core.<$> (x Core..:? "failureCode")
            Core.<*> (x Core..:? "imageId")
            Core.<*> (x Core..:? "failureReason")
      )

instance Core.Hashable ImageFailure

instance Core.NFData ImageFailure
