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
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens

-- | The filter for the lifecycle policy preview.
--
-- /See:/ 'newLifecyclePolicyPreviewFilter' smart constructor.
data LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
  { -- | The tag status of the image.
    tagStatus :: Core.Maybe TagStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LifecyclePolicyPreviewFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagStatus', 'lifecyclePolicyPreviewFilter_tagStatus' - The tag status of the image.
newLifecyclePolicyPreviewFilter ::
  LifecyclePolicyPreviewFilter
newLifecyclePolicyPreviewFilter =
  LifecyclePolicyPreviewFilter'
    { tagStatus =
        Core.Nothing
    }

-- | The tag status of the image.
lifecyclePolicyPreviewFilter_tagStatus :: Lens.Lens' LifecyclePolicyPreviewFilter (Core.Maybe TagStatus)
lifecyclePolicyPreviewFilter_tagStatus = Lens.lens (\LifecyclePolicyPreviewFilter' {tagStatus} -> tagStatus) (\s@LifecyclePolicyPreviewFilter' {} a -> s {tagStatus = a} :: LifecyclePolicyPreviewFilter)

instance Core.Hashable LifecyclePolicyPreviewFilter

instance Core.NFData LifecyclePolicyPreviewFilter

instance Core.ToJSON LifecyclePolicyPreviewFilter where
  toJSON LifecyclePolicyPreviewFilter' {..} =
    Core.object
      ( Core.catMaybes
          [("tagStatus" Core..=) Core.<$> tagStatus]
      )
