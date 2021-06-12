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
-- Module      : Network.AWS.ECR.Types.ListImagesFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ListImagesFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens

-- | An object representing a filter on a ListImages operation.
--
-- /See:/ 'newListImagesFilter' smart constructor.
data ListImagesFilter = ListImagesFilter'
  { -- | The tag status with which to filter your ListImages results. You can
    -- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
    tagStatus :: Core.Maybe TagStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImagesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagStatus', 'listImagesFilter_tagStatus' - The tag status with which to filter your ListImages results. You can
-- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
newListImagesFilter ::
  ListImagesFilter
newListImagesFilter =
  ListImagesFilter' {tagStatus = Core.Nothing}

-- | The tag status with which to filter your ListImages results. You can
-- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
listImagesFilter_tagStatus :: Lens.Lens' ListImagesFilter (Core.Maybe TagStatus)
listImagesFilter_tagStatus = Lens.lens (\ListImagesFilter' {tagStatus} -> tagStatus) (\s@ListImagesFilter' {} a -> s {tagStatus = a} :: ListImagesFilter)

instance Core.Hashable ListImagesFilter

instance Core.NFData ListImagesFilter

instance Core.ToJSON ListImagesFilter where
  toJSON ListImagesFilter' {..} =
    Core.object
      ( Core.catMaybes
          [("tagStatus" Core..=) Core.<$> tagStatus]
      )
