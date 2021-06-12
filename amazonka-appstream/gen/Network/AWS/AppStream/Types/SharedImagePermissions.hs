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
-- Module      : Network.AWS.AppStream.Types.SharedImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SharedImagePermissions where

import Network.AWS.AppStream.Types.ImagePermissions
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the permissions that are available to the specified AWS
-- account for a shared image.
--
-- /See:/ 'newSharedImagePermissions' smart constructor.
data SharedImagePermissions = SharedImagePermissions'
  { -- | The 12-digit identifier of the AWS account with which the image is
    -- shared.
    sharedAccountId :: Core.Text,
    -- | Describes the permissions for a shared image.
    imagePermissions :: ImagePermissions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SharedImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedAccountId', 'sharedImagePermissions_sharedAccountId' - The 12-digit identifier of the AWS account with which the image is
-- shared.
--
-- 'imagePermissions', 'sharedImagePermissions_imagePermissions' - Describes the permissions for a shared image.
newSharedImagePermissions ::
  -- | 'sharedAccountId'
  Core.Text ->
  -- | 'imagePermissions'
  ImagePermissions ->
  SharedImagePermissions
newSharedImagePermissions
  pSharedAccountId_
  pImagePermissions_ =
    SharedImagePermissions'
      { sharedAccountId =
          pSharedAccountId_,
        imagePermissions = pImagePermissions_
      }

-- | The 12-digit identifier of the AWS account with which the image is
-- shared.
sharedImagePermissions_sharedAccountId :: Lens.Lens' SharedImagePermissions Core.Text
sharedImagePermissions_sharedAccountId = Lens.lens (\SharedImagePermissions' {sharedAccountId} -> sharedAccountId) (\s@SharedImagePermissions' {} a -> s {sharedAccountId = a} :: SharedImagePermissions)

-- | Describes the permissions for a shared image.
sharedImagePermissions_imagePermissions :: Lens.Lens' SharedImagePermissions ImagePermissions
sharedImagePermissions_imagePermissions = Lens.lens (\SharedImagePermissions' {imagePermissions} -> imagePermissions) (\s@SharedImagePermissions' {} a -> s {imagePermissions = a} :: SharedImagePermissions)

instance Core.FromJSON SharedImagePermissions where
  parseJSON =
    Core.withObject
      "SharedImagePermissions"
      ( \x ->
          SharedImagePermissions'
            Core.<$> (x Core..: "sharedAccountId")
            Core.<*> (x Core..: "imagePermissions")
      )

instance Core.Hashable SharedImagePermissions

instance Core.NFData SharedImagePermissions
