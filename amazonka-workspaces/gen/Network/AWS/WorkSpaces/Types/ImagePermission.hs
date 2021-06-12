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
-- Module      : Network.AWS.WorkSpaces.Types.ImagePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ImagePermission where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the AWS accounts that have been granted permission to use a
-- shared image. For more information about sharing images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image>.
--
-- /See:/ 'newImagePermission' smart constructor.
data ImagePermission = ImagePermission'
  { -- | The identifier of the AWS account that an image has been shared with.
    sharedAccountId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImagePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedAccountId', 'imagePermission_sharedAccountId' - The identifier of the AWS account that an image has been shared with.
newImagePermission ::
  ImagePermission
newImagePermission =
  ImagePermission' {sharedAccountId = Core.Nothing}

-- | The identifier of the AWS account that an image has been shared with.
imagePermission_sharedAccountId :: Lens.Lens' ImagePermission (Core.Maybe Core.Text)
imagePermission_sharedAccountId = Lens.lens (\ImagePermission' {sharedAccountId} -> sharedAccountId) (\s@ImagePermission' {} a -> s {sharedAccountId = a} :: ImagePermission)

instance Core.FromJSON ImagePermission where
  parseJSON =
    Core.withObject
      "ImagePermission"
      ( \x ->
          ImagePermission'
            Core.<$> (x Core..:? "SharedAccountId")
      )

instance Core.Hashable ImagePermission

instance Core.NFData ImagePermission
