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
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter where

import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The filter for the lifecycle policy preview.
--
-- /See:/ 'newLifecyclePolicyPreviewFilter' smart constructor.
data LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
  { -- | The tag status of the image.
    tagStatus :: Prelude.Maybe TagStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The tag status of the image.
lifecyclePolicyPreviewFilter_tagStatus :: Lens.Lens' LifecyclePolicyPreviewFilter (Prelude.Maybe TagStatus)
lifecyclePolicyPreviewFilter_tagStatus = Lens.lens (\LifecyclePolicyPreviewFilter' {tagStatus} -> tagStatus) (\s@LifecyclePolicyPreviewFilter' {} a -> s {tagStatus = a} :: LifecyclePolicyPreviewFilter)

instance
  Prelude.Hashable
    LifecyclePolicyPreviewFilter

instance Prelude.NFData LifecyclePolicyPreviewFilter

instance Prelude.ToJSON LifecyclePolicyPreviewFilter where
  toJSON LifecyclePolicyPreviewFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("tagStatus" Prelude..=) Prelude.<$> tagStatus]
      )
