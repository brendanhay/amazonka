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
-- Module      : Amazonka.ECR.Types.DescribeImagesFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.DescribeImagesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.TagStatus
import qualified Amazonka.Prelude as Prelude

-- | An object representing a filter on a DescribeImages operation.
--
-- /See:/ 'newDescribeImagesFilter' smart constructor.
data DescribeImagesFilter = DescribeImagesFilter'
  { -- | The tag status with which to filter your DescribeImages results. You can
    -- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
    tagStatus :: Prelude.Maybe TagStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImagesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagStatus', 'describeImagesFilter_tagStatus' - The tag status with which to filter your DescribeImages results. You can
-- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
newDescribeImagesFilter ::
  DescribeImagesFilter
newDescribeImagesFilter =
  DescribeImagesFilter' {tagStatus = Prelude.Nothing}

-- | The tag status with which to filter your DescribeImages results. You can
-- filter results based on whether they are @TAGGED@ or @UNTAGGED@.
describeImagesFilter_tagStatus :: Lens.Lens' DescribeImagesFilter (Prelude.Maybe TagStatus)
describeImagesFilter_tagStatus = Lens.lens (\DescribeImagesFilter' {tagStatus} -> tagStatus) (\s@DescribeImagesFilter' {} a -> s {tagStatus = a} :: DescribeImagesFilter)

instance Prelude.Hashable DescribeImagesFilter where
  hashWithSalt _salt DescribeImagesFilter' {..} =
    _salt `Prelude.hashWithSalt` tagStatus

instance Prelude.NFData DescribeImagesFilter where
  rnf DescribeImagesFilter' {..} = Prelude.rnf tagStatus

instance Core.ToJSON DescribeImagesFilter where
  toJSON DescribeImagesFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [("tagStatus" Core..=) Prelude.<$> tagStatus]
      )
