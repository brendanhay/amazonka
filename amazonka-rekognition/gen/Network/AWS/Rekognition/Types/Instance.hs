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
-- Module      : Network.AWS.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Instance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.BoundingBox

-- | An instance of a label returned by Amazon Rekognition Image
-- (DetectLabels) or by Amazon Rekognition Video (GetLabelDetection).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The position of the label instance on the image.
    boundingBox :: Core.Maybe BoundingBox,
    -- | The confidence that Amazon Rekognition has in the accuracy of the
    -- bounding box.
    confidence :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'instance_boundingBox' - The position of the label instance on the image.
--
-- 'confidence', 'instance_confidence' - The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
newInstance ::
  Instance
newInstance =
  Instance'
    { boundingBox = Core.Nothing,
      confidence = Core.Nothing
    }

-- | The position of the label instance on the image.
instance_boundingBox :: Lens.Lens' Instance (Core.Maybe BoundingBox)
instance_boundingBox = Lens.lens (\Instance' {boundingBox} -> boundingBox) (\s@Instance' {} a -> s {boundingBox = a} :: Instance)

-- | The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
instance_confidence :: Lens.Lens' Instance (Core.Maybe Core.Double)
instance_confidence = Lens.lens (\Instance' {confidence} -> confidence) (\s@Instance' {} a -> s {confidence = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Core.<$> (x Core..:? "BoundingBox")
            Core.<*> (x Core..:? "Confidence")
      )

instance Core.Hashable Instance

instance Core.NFData Instance
