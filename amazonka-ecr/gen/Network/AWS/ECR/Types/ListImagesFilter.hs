{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a filter on a ListImages operation.
--
-- /See:/ 'newListImagesFilter' smart constructor.
data ListImagesFilter = ListImagesFilter'
  { -- | The tag status with which to filter your ListImages results. You can
    -- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
    tagStatus :: Prelude.Maybe TagStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ListImagesFilter' {tagStatus = Prelude.Nothing}

-- | The tag status with which to filter your ListImages results. You can
-- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
listImagesFilter_tagStatus :: Lens.Lens' ListImagesFilter (Prelude.Maybe TagStatus)
listImagesFilter_tagStatus = Lens.lens (\ListImagesFilter' {tagStatus} -> tagStatus) (\s@ListImagesFilter' {} a -> s {tagStatus = a} :: ListImagesFilter)

instance Prelude.Hashable ListImagesFilter

instance Prelude.NFData ListImagesFilter

instance Prelude.ToJSON ListImagesFilter where
  toJSON ListImagesFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("tagStatus" Prelude..=) Prelude.<$> tagStatus]
      )
